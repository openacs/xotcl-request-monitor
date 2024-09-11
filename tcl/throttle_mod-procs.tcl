#############################################################################
# Main procs of Request Monitor
#
# Create a separate thread (named "throttle") to act as a monitor of
# the incoming requests. The monitor blocks repeated requests,
# throttles over-eager users and provides a wide set of statistics.
#############################################################################

if {"async-cmd" ni [ns_job queues]} {
  ns_job create async-cmd 4
  #ns_job configure -jobsperthread 10000
}

::xotcl::THREAD create throttle {

  #
  # Never block the following provided Sec-Fetch-Dest values.
  #
  # set ::never_blocked_fetchDest {image script}
  set ::never_blocked_fetchDest {image iframe script}
  set ::monitor_urls {/ /register/ /dotlrn/}

  set ::verbose_blocking 0

  #
  # A simple helper class to provide a faster an easier-to-use
  # interface to package parameters. Eventually, this will move in a
  # more general way into xotcl-core.
  #
  Class create package_parameter \
      -parameter {{default ""} value name} \
      -instproc defaultmethod {} {return ${:value}} \
      -instproc update {value} {set :value $value} \
      -instproc init {} {
        set :name [namespace tail [self]]
        set :value [parameter::get_from_package_key \
                        -package_key "xotcl-request-monitor" \
                        -parameter ${:name} \
                        -default ${:default}]
      }

  package_parameter log-dir \
      -default [file dirname [file rootname [ns_config ns/parameters ServerLog]]]

  package_parameter do_double_click_prevention -default on
  package_parameter do_slowdown_overactive     -default off
  package_parameter do_throttle                -default on
  package_parameter do_track_activity          -default off
  package_parameter max-stats-elements         -default 5
  package_parameter max-url-stats              -default 500
  package_parameter monitor_urls               -default "/ /register/ /dotlrn/"
  package_parameter time-window                -default 10
  package_parameter trend-elements             -default 48
  package_parameter map-slow-pool-duration     -default [expr {[ns_baseunit -time 12h]*1000}]

  #
  # When updates happen on
  #   - max-stats-elements or
  #   - trend-elements
  # Propagate changes of values to all instances of
  # counters.
  #
  max-stats-elements proc update {value} {
    next
    Counter set_in_all_instances nr_stats_elements $value
  }
  trend-elements proc update {value} {
    next
    Counter set_in_all_instances nr_trend_elements $value
  }
  do_throttle proc update {value} {
    next
    throttler set do_throttle $value
  }
  do_double_click_prevention proc update {value} {
    next
    throttler set do_double_click_prevention $value
  }
  monitor_urls proc update {value} {
    next
    set ::monitor_urls $value
  }

  # get the value from the logdir parameter
  set ::logdir [log-dir]
  if {![ad_file isdirectory $logdir]} {file mkdir $logdir}

  #
  # Create AsyncLogFile class
  #
  Class create AsyncLogFile -parameter {filename {mode a}}

  AsyncLogFile instproc init {} {
    if {![info exists :filename]} {
      set :filename $::logdir/[namespace tail [self]]
    }
    :open
  }

  if {[acs::icanuse ns_asynclogfile]} {
    #
    # Use NaviServer builtin async disk writer.
    #
    ns_log notice "... AsyncLogFile uses NaviServer ns_asynclogfile"

    AsyncLogFile instproc open {} {
      #
      # The open "append" mode is the default mode, we use nothing
      # else here.
      #
      set :handle [ns_asynclogfile open ${:filename}]
    }

    AsyncLogFile instproc write {{-sanitize 0} msg} {
      ns_asynclogfile write -sanitize $sanitize ${:handle} $msg\n
    }

    AsyncLogFile instproc destroy {} {
      ns_asynclogfile close ${:handle}
      next
    }

  } else {
    #
    # Make AsyncLogFile a client of the
    # AsyncDiskWriter in bgdelivery.
    #
    ns_log notice "... AsyncLogFile uses bgdelivery"

    AsyncLogFile instproc open {} {
      set :handle [bgdelivery do AsyncDiskWriter new -autoflush true]
      bgdelivery do ${:handle} open -filename ${:filename} -mode ${:mode}
    }

    AsyncLogFile instproc write {{-sanitize 0} msg} {
      bgdelivery do ${:handle} async_write $msg\n
    }

    AsyncLogFile instproc destroy {} {
      catch {bgdelivery do ${:handle} close}
      next
    }

  }

  # open the used log-files
  AsyncLogFile create counter.log
  AsyncLogFile create long-calls.log
  AsyncLogFile create switches.log

  #
  # A class to keep simple statistics
  #
  Class create ThrottleStat -parameter { type requester timestamp ip_address url }

  #
  # class for throtteling eager requesters or to block duplicate requests
  #
  Class create Throttle -parameter {
    {timeWindow 10}
    {timeoutMs 2000}
    {startThrottle 11}
    {toMuch 10}
    {alerts 0} {throttles 0} {rejects 0} {repeats 0}
  }

  Throttle instproc init {} {
    set :do_throttle [do_throttle]
    set :do_double_click_prevention [do_double_click_prevention]
    Object create [self]::stats
    Object create [self]::users
    next
  }

  Throttle instproc add_statistics { type requester ip_address url query } {
    #set furl [expr {$query ne "" ? "$url?$query" : $url}]
    incr :${type}s
    # :log "++++ add_statistics   -type $type -user_id $requester "
    set entry [ThrottleStat new -childof [self]::stats \
                   -type $type -requester $requester \
                   -timestamp [clock seconds] \
                   -ip_address $ip_address -url $url]
  }

  Throttle instproc url_statistics {{-flush 0}} {
    set data [[self]::stats info children]
    if { [llength $data] == 0} {
      return $data
    } elseif {$flush} {
      foreach c $data {$c destroy}
      return ""
    } else {
      foreach stat $data {
        lappend output [list [$stat type] [$stat requester] \
                            [$stat timestamp] [$stat ip_address] [$stat url]]
      }
      return $output
    }
  }

  Throttle instproc call_statistics {} {
    set l [list]
    foreach t {seconds minutes hours} {
      lappend l [list $t [$t set last] [$t set trend] [$t set stats]]
    }
    return $l
  }

  Throttle instproc register_access {requestKey pa url community_id is_embedded_request} {
    #ns_log notice "register_access $requestKey $pa $url "
    set obj [Users current_object]
    $obj addKey $requestKey $pa $url $community_id $is_embedded_request
    Users expSmooth [$obj point_in_time] $requestKey
  }

  Throttle instproc running {} {
    array get :running_url
  }

  #
  # Global variables in the thread to calculate thread
  # statistics of the server
  #
  set ::threads_busy 0
  set ::threads_current 0
  set ::threads_datapoints 0 ;# make sure, we never divide by 0

  if {[ns_info name] eq "NaviServer"} {
    Throttle instproc server_threads {} {ns_server threads}
  } else {
    Throttle instproc server_threads {} {
      # flatten the list
      return [concat {*}[ns_server threads]]
    }
  }
  Throttle instproc update_threads_state {} {
    array set threadInfo [:server_threads]
    incr ::threads_busy [expr {$threadInfo(current) - $threadInfo(idle)}]
    incr ::threads_current $threadInfo(current)
    incr ::threads_datapoints
  }

  Throttle instproc thread_avgs {} {
    return [list \
                busy [format %.2f [expr {1.0 * $::threads_busy / $::threads_datapoints}]] \
                current [format %.2f [expr {1.0 * $::threads_current / $::threads_datapoints}]]]
  }

  Throttle instproc throttle_check {requestKey pa url conn_time content_type community_id {context ""}} {
    #
    # Return: toMuch ms repeat
    #
    #set t0 [clock milliseconds]
    #ns_log notice "throttle_check context <$context>"

    seconds ++
    :update_threads_state

    set fetchDest [expr {[dict exists $context Sec-Fetch-Dest]   ? [dict get $context Sec-Fetch-Dest] : "document"}]
    set range     [expr {[dict exists $context Range]            ? [dict get $context Range]          : ""}]
    set ajax_p    [expr {[dict get $context X-Requested-With] eq "XMLHttpRequest"}]

    #
    # Check whether all request monitor performance tracking is turned
    # off. If so, it does not even track the number of active users.
    #
    if {!${:do_throttle}} {
      return [list 0 0 0]
    }

    set var :running_url($requestKey,$url)
    set overactive ov($requestKey,$url)

    #
    # Never block certain requests, such as embedded requests, range
    # requests, system requests, requests from the fast pool, ...
    #
    if {
        $fetchDest in $::never_blocked_fetchDest
        || $range ne ""
        || [dict get $context pool] eq "fast"
        || $ajax_p
        || [string match "image/*" $content_type]
        || [string match "video/*" $content_type]
        || $content_type in {
          application/vnd.apple.mpegurl
          text/css
          application/javascript
          application/x-javascript
        }
        || [string match "/SYSTEM/*" $url]
        || [string match "/shared/*" $url]
        || "/proctoring/upload" eq $url
      } {

      if {$::verbose_blocking && [info exists $var]} {
        ns_log notice "request not blocked although apparently running: fetchDest $fetchDest $requestKey $url"
      }
      set $var $conn_time

      return [list 0 0 0]
    }

    #
    # Check whether the same user has already the same request issued;
    # if yes, block this request. Caveat: some html-pages use the same
    # image in many places, so we can't block it, but this is already
    # covered above.
    #
    if {${:do_double_click_prevention} && [info exists $var]} {
      #
      # Request already running
      #
      # Keep value in per-minute counter
      minutes incr $overactive
      #
      #ns_log notice  "### block $var overactive [minutes set $overactive]"
      return [list 0 0 1]
    } elseif {$::verbose_blocking && [info exists $var]} {
      ns_log notice "would block: fetchDest $fetchDest $requestKey $url"
    }

    #
    # Check, if have blocked (429) this URL already 15 times for this
    # user in this minute.  If so, block this URL for this user, until
    # the minute is over.
    #
    if {[minutes exists $overactive] && [minutes set $overactive] > 15} {
      ns_log notice  "### request $overactive blocked since user has issued in this minute too many repeated requests"
      return [list 0 0 2]
    }

    set $var $conn_time
    #ns_log notice  "### new $var"
    #set t1 [clock milliseconds]

    :register_access $requestKey $pa $url $community_id 0 ;# $is_embedded_request
    #set t2 [clock milliseconds]

    if {[do_slowdown_overactive]} {
      #
      # Check, whether the last request from a user was within
      # the minimum time interval. We are not keeping a full table
      # of all request keys, but use a timeout triggered mechanism
      # to keep only the active request keys in an associative array.
      #
      incr :alerts
      if {[info exists :active($requestKey)]} {
        #
        # If more than one request for this key is already active,
        # return blocking time.
        #
        lassign [set :active($requestKey)] to cnt
        set retMs [expr {$cnt > ${:startThrottle} ? 500 : 0}]
        # cancel the timeout
        after cancel $to
      } else {
        set retMs 0
        set cnt 0
      }
      incr cnt
      # establish a new timeout
      set to [after ${:timeoutMs} [list [self] cancel $requestKey]]
      set :active($requestKey) [list $to $cnt]
      if {$cnt <= ${:toMuch}} {
        set cnt 0
      }
      return [list $cnt $retMs 0]
    }

    return [list 0 0 0]
  }

  Throttle instproc statistics {} {
    return "<table>
        <tr><td>Number of alerts:</td><td>[:alerts]</td></tr>
        <tr><td>Number of throttles:</td><td>[:throttles]</td></tr>
        <tr><td>Number of rejects:</td><td>[:rejects]</td></tr>
        <tr><td>Number of repeats:</td><td>[:repeats]</td></tr>
        </table>\n"
  }

  Throttle instproc cancel {requestKey} {
    # cancel a timeout and clean up active request table for this key
    if {[info exists :active($requestKey)]} {
      after cancel [lindex [set :active($requestKey)] 0]
      unset :active($requestKey)
      # :log "+++ Cancel $requestKey block"
    } else {
      :log "+++ Cancel for $requestKey failed !!!"
    }
  }

  Throttle instproc active { } {
    # return the currently active requests (for debugging and introspection)
    return [array get :active]
  }

  Throttle instproc add_url_stat {method url partialtimes key pa content_type pool} {
    #ns_log notice "Throttle.add_url_stat($method,$url,$partialtimes,$key,$pa,$content_type)"
    unset -nocomplain :running_url($key,$url)
    # :log "### unset running_url($key,$url) $errmsg"
    if {[string match "text/html*" $content_type]} {
      [Users current_object] add_view $key
    }
    response_time_minutes add_url_stat $url [dict get $partialtimes ms] $key
  }
  Throttle instforward report_url_stats response_time_minutes %proc
  Throttle instforward flush_url_stats  response_time_minutes %proc
  Throttle instforward last100          response_time_minutes %proc
  Throttle create throttler

  Class create ThrottleTrace
  ThrottleTrace instproc log {msg} {
    if {![info exists :traceFile]} {
      set file $::logdir/calls
      set :traceFile [open $file a]
      set :traceCounter 0
    }
    puts ${:traceFile} $msg
  }
  ThrottleTrace instproc throttle_check args {
    incr :traceCounter
    :log "CALL ${:traceCounter} [self args]"
    next
  }
  ThrottleTrace instproc add_url_stat args {
    catch {:log "END ${:traceCounter} [self args]"}
    next
  }

  # throttle do throttler mixin ThrottleTrace

  Class create TraceLongCalls
  TraceLongCalls set count 0
  TraceLongCalls instproc log {msg} {
    set entry "[clock format [clock seconds]] -- $msg"
    long-calls.log write $entry
    [self class] append log "$entry\n"
    [self class] incr count
  }

  TraceLongCalls instproc add_url_stat {method url partialtimes key pa content_type pool} {
    regexp {^([^?]+)[?]?(.*)$} $url . url query
    #
    # conntime: time spent in connection thread in ms, not including queuing times
    # totaltime: time since start of the request
    #
    set conntime [expr {int(([dict get $partialtimes runtime] + [dict get $partialtimes filtertime]) * 1000)}]
    set totaltime [dict get $partialtimes ms]

    #ns_log notice "conntime $conntime totaltime $totaltime url=<$url>"
    if { $url in $::monitor_urls } {
      #
      # Calculate for certain URLs separate statistics.  These can be
      # used via munin with the responsetime plugin, configured e.g. as
      #
      # [naviserver_production_responsetime]
      #    env.urls / /register/ /dotlrn/
      #
      incr ::agg_time($url) $totaltime
      incr ::count(calls:$url)
    }

    ::xo::remap_pool -runtime [dict get $partialtimes runtime] $method $url

    #
    # Handling of longcalls counter
    #
    if {$conntime > 3000
        || [dict get $partialtimes filtertime] > 1.0
        || [dict get $partialtimes queuetime] > 0.5
      } {
      if {$url eq "/register/"} {
        set color unexpected
      } elseif {$conntime > 7000} {
        set color red
      } elseif {$conntime > 5000} {
        set color orange
      } else {
        set color yellow
      }
      incr ::count(longcalls:$color)

      #
      # Add query to URL, truncate in case it is not too long.
      #
      set ql [string length $query]
      if {$ql > 0} {
        if {$ql < 80} {
          set loggedUrl $url?$query
        } else {
          set loggedUrl $url?[string range $query 0 77]...
        }
      } else {
        set loggedUrl $url
      }

      #
      # Finally, log the entry with to log/long-calls.log
      #
      if {[catch {:log [list $loggedUrl $partialtimes $key $pa $content_type $pool]} errorMsg]} {
        ns_log error "long-call error: $errorMsg"
      }

    }
    next
  }

  #
  # Simple means for banning users, e.g. performing too eager
  # requests.  Requests from banned users receive a "duplicate
  # request" reply.
  #
  Class create BanUser
  # BanUser instproc throttle_check {requestKey pa url conn_time content_type community_id {context ""}} {
  #   #if {$requestKey eq 37958315} {return [list 0 0 1]}
  #   #if {[string match "155.69.25.*" $pa]} {return [list 0 0 1]}
  #   next
  # }

  throttle do throttler mixin {BanUser TraceLongCalls}

  ############################
  # A simple counter class, which is able to aggregate values in some
  # higher level counters (report_to) and to keep statistics in form
  # of a trend and max values)
  Class create Counter -parameter {
    report
    timeoutMs
    {stats ""}
    {last ""}
    {trend ""}
    {c 0}
    {logging 0}
    {nr_trend_elements [trend-elements]}
    {nr_stats_elements [max-stats-elements]}
  } -ad_doc {
    This class holds the counted statistics so they do not have to be computed
    all the time from the list of requests.

    The statistics holding objects are instances of this class and initialized and called after
    the timeoutMS

    @param report Report type of the instance. This could e.g. be hours and minutes
    @param timeoutMS How often are the statistics for this report computed
    @param stats stats keeps nr_stats_elements highest values with timestamp.
    These hold a list of lists of the actual stats in the form {time value}.
    Time is given like "Thu Sep 13 09:17:30 CEST 2007".
    This is used for displaying the maximum values
    @param trend  trend keeps nr_trend_elements most recent values. This is used for displaying the graphics
    @param c counter
    @param logging If set to 1 the instance current value is logged to the counter.log file
    @param nr_trend_elements Number of data points that are used for the trend calculation. The default of 48 translates into "48 minutes" for the Views per minute or 48 hours for the views per hour.
    @param nr_stats_elements Number of data points for the stats values. The default of 5 will give you the highest datapoints over the whole period.
  }

  Counter ad_proc set_in_all_instances {var value} {
    A helper function to set in all (direct or indirect) instances
    an instance variable to the same value. This is used here
    in combination with changing parameters
  } {
    foreach object [:allinstances] {
      $object set $var $value
    }
  }

  Counter instproc ++ {} {
    incr :c
  }
  Counter instproc end {} {
    if {[info exists :report]} {
      [:report] incr c ${:c}
    }
    :finalize ${:c}
    set :c 0
  }


  Counter instproc log_to_file {timestamp label value} {
    set server [ns_info server]
    counter.log write "$timestamp -- $server $label $value"
  }

  Counter instproc add_value {timestamp n} {
    #
    # trend keeps nr_trend_elements most recent values
    #
    lappend :trend $n
    set lt [llength ${:trend}]
    if {$lt > ${:nr_trend_elements}} {
      set :trend [lrange ${:trend} $lt-${:nr_trend_elements} end]
    }
    #
    # stats keeps nr_stats_elements highest values with timestamp
    #
    lappend :stats [list $timestamp $n]
    set :stats [lrange [lsort -real -decreasing -index 1 ${:stats}] 0 ${:nr_stats_elements}-1]
  }
  Counter instproc finalize {n} {
    if {[info exists :to]} {
      after cancel ${:to}
      #
      # update statistics
      #
      set now [clock format [clock seconds]]
      :add_value $now $n
      #
      # log if necessary
      #
      catch {if {${:logging}} {:log_to_file $now [self] $n}}
      #
    } else {
      ns_log notice "request-monitor: [self] has no timeout defined"
    }
    set :to [after ${:timeoutMs} [list [self] end]]
  }

  Counter instproc init {} {
    set :to [after ${:timeoutMs} [list [self] end]]
    next
  }
  Counter instproc destroy {} {
    after cancel ${:to}
    next
  }

  Counter create hours -timeoutMs [expr {60000*60}] -logging 1
  Counter create minutes -timeoutMs 60000 -report hours -logging 1
  Counter create seconds -timeoutMs 1000 -report minutes

  minutes proc end {} {
    #
    # Delete overactive counters.
    #
    array unset :ov
    next
  }

  # The counter user_count_day just records the number of active user
  # per day. It differs from other counters by keeping track of a pair
  # of values (authenticated and non-authenticated).

  Counter user_count_day -timeoutMs [expr {71000*60}] -logging 1
  user_count_day proc end {} {
    lassign [throttle users nr_users_per_day] auth ip
    set now [clock format [clock seconds]]
    #
    # The counter logs its intrinsic value (:c) anyhow, which are the
    # authenticated users. We also want to record the number of
    # unauthenticated users, and do this here manually.
    #
    :log_to_file $now [self]-non-auth $ip
    set :c $auth
    #
    # Perform as well bookkeeping (per default once per hour)
    #
    Users perDayCleanup
    next
  }

  Class create MaxCounter -superclass Counter \
      -parameter {{metric nr_active}} \
      -instproc end {} {
        set :c [Users ${:metric}]
        if {[info exists :report]} {
          if {[${:report} set c] < ${:c}} {
            ${:report} set c ${:c}
          }
        }
        :finalize ${:c}
        set :c 0
      }

  MaxCounter create user_count_hours -timeoutMs [expr {60000*60}] -logging 1
  MaxCounter create user_count_minutes -timeoutMs 60000 -report user_count_hours -logging 1

  MaxCounter create authenticated_count_hours   -metric nr_authenticated -timeoutMs [expr {60000*60}] -logging 1
  MaxCounter create authenticated_count_minutes -metric nr_authenticated -timeoutMs 60000 \
      -report authenticated_count_hours -logging 1

  Class create AvgCounter -superclass Counter \
      -parameter {{t 0} {atleast 1}} -instproc end {} {
        if {${:c} > 0} {
          set avg [expr {int(${:t} * 1.0 / ${:c})}]
        } else {
          set avg 0
        }
        if {[info exists :report]} {
          ${:report} incr c ${:c}
          ${:report} incr t ${:t}
        }
        :finalize $avg
        set :c 0
        set :t 0
      }

  Class create UrlCounter -superclass AvgCounter \
      -parameter {
        {truncate_check 10}
        {max_urls 0}
      } \
      -set seconds [clock seconds]

  UrlCounter instproc add_url_stat {url ms requester} {
    #ns_log notice "UrlCounter.add_url_stat($url,$ms,$requester)"
    my ++
    # :log "[self proc] $url /$ms/ $requester (${:c})"
    incr :t $ms
    #
    # Set up a value for the right ordering in last 100.  We take the
    # difference in seconds since start, multiply by 10000 (there
    # should be no overflow); there should be less than this number
    # requests per minute.
    #
    set now [clock seconds]
    set order [expr {($now - [[self class] set seconds]) * 10000 + ${:c}}]
    set :last100([expr {$order%99}]) [list $now $order $url $ms $requester]

    set has_param [regexp {^(.*)[?]} $url _ url]
    if {$has_param} {set url $url?...}

    ### Add statistics
    incr :stat($url) $ms
    incr :cnt($url)
  }

  UrlCounter instproc last100  {} {
    array get :last100
  }
  UrlCounter instproc flush_url_stats {} {
    :log "flush_url_stats"
    array unset :stat
    array unset :cnt
  }
  UrlCounter instproc url_stats {} {
    set result [list]
    foreach url [array names :stat] {
      lappend result [list $url [set :stat($url)] [set :cnt($url)]]
    }
    set result [lsort -real -decreasing -index 1 $result]
    return $result
  }
  UrlCounter instproc check_truncate_stats {} {
    #
    # Truncate statistics if necessary.
    #
    set max [max-url-stats]
    if {$max>1} {
      set result [:url_stats]
      set l [llength $result]
      for {set i $max} {$i < $l} {incr i} {
        set url [lindex $result $i 0]
        unset :stat($url) :cnt($url)
      }
      set result [lrange $result 0 $max-1]
      return $result
    }
    return ""
  }
  UrlCounter instproc cleanup_stats {} {
    #
    # We use the timer to check other parameters as well here.
    #
    set time_window [time-window]
    if {$time_window != [throttler timeWindow]} {
      throttler timeWindow $time_window
      after 0 [list Users purge_access_stats]
    }
    return ""
  }
  UrlCounter instproc report_url_stats {} {
    set stats [:check_truncate_stats]
    if {$stats eq ""} {
      set stats [:url_stats]
    }
    return $stats
  }
  UrlCounter instproc finalize args {
    next
    #
    # Each time the timer runs out, perform the cleanup.
    #
    after 0 [list [self] cleanup_stats]
  }

  #
  # Create UrlCounter instances
  #
  UrlCounter create response_time_hours \
      -timeoutMs [expr {60000*60}] \
      -atleast 500 \
      -logging 1
  UrlCounter create response_time_minutes \
      -timeoutMs 60000 \
      -report response_time_hours \
      -atleast 100 \
      -logging 1

  #
  # Class for the user tracking

  Class create Users -parameter {
    point_in_time
    {ip24 0}
    {auth24 0}
  } -ad_doc {
    This class is responsible for the user tracking and is defined only
    in a separate Tcl thread named <code>throttle</code>.
    For each minute within the specified <code>time-window</code> an instance
    of this class exists keeping various statistics.
    When a minute ends the instance dropping out of the
    time window is destroyed. The procs of this class can be
    used to obtain various kinds of information.

    @author Gustaf Neumann
    @cvs-id $Id$
  }

  #
  # Make sure to always provide initialized aggregated values in case
  # that "dump read" fails to initialized these.
  #
  Users set ip24 0
  Users set auth24 0

  Users ad_proc active {-full:switch}  {

    Return a list of lists containing information about current
    users. If the switch 'full' is used this list contains these users
    who have used the server within the monitoring time window (per
    default: 10 minutes). Otherwise, just a list of requesters
    (user_ids or peer addresses for unauthenticated requests) is
    returned.

    If "-full" is used for each requester the last peer address, the
    last timestamp, the number of hits, a list of values for the
    activity calculations and the number of ip-switches the user is
    returned.

    The activity calculations are performed on base of an exponential
    smoothing algorithm which is calculated through an aggregated
    value, a timestamp (in minutes) and the number of hits in the
    monitored time window.

    @return list with detailed user info
  } {
    if {$full} {
      set info [list]
      foreach key [array names :pa] {
        set entry [list $key [set :pa($key)]]
        foreach var [list timestamp hits expSmooth switches] {
          set k ${var}($key)
          lappend entry [expr {[info exists :$k] ? [set :$k] : 0}]
        }
        lappend info $entry
      }
      return $info
    } else {
      return [array names :pa]
    }
  }
  Users proc unknown { obj args } {
    :log "unknown called with $obj $args"
  }
  Users ad_proc nr_active {} {
    @return number of active users (in time window)
  } {
    return [array size :pa]
  }
  Users ad_proc nr_authenticated {} {
    @return number of authenticated users (in time window)
  } {
    return [lindex [:nr_users_time_window] 1]
  }

  # Users ad_proc nr_users_time_window {} {
  #   @return number of different IP addresses and authenticated users (in time window)
  # } {
  #   set ip 0; set auth 0
  #   foreach i [array names :pa] {
  #     if {[::xo::is_ip $i]} {incr ip} {incr auth}
  #   }
  #   return [list $ip $auth]
  # }

  Users ad_proc nr_users_time_window {} {
    @return number of different IP addresses and authenticated users (in time window)
  } {
    set ip 0; set auth 0; set reverseAuthDict {}; set ipDict {}
    #
    # Separate "pa" data into authenticated and not-authenticated, where
    # we use the authenticated data as a reverse lookup dict later.
    #
    foreach {k v} [array get :pa] {
      if {[::xo::is_ip $k]} {
        lappend ipDict $k $v
      } else {
        lappend reverseAuthDict $v $k
        incr auth
      }
    }
    #
    # Don't count cases from the ipDict which are already counted in
    # for the auth cases. This assumes that from one IP address, there
    # is never a person connected authenticated and not authenticated
    # at the same time in the give time window. If it is, it is
    # ignored in the statistics.
    #
    foreach {k v} $ipDict {
      if {![dict exists $reverseAuthDict $v]} {
        incr ip
      }
    }
    return [list $ip $auth]
  }

  Users ad_proc user_is_active {uid} {
    @return boolean value whether user is active
  } {
    info exists :pa($uid)
  }

  Users ad_proc hits {uid} {
    @param uid request key
    @return Number of hits by this user (in time window)
  } {
    if {[info exists :hits($uid)]} {
      return [set :hits($uid)]
    } else {
      return 0
    }
  }
  Users ad_proc last_pa {uid} {
    @param uid request key
    @return last peer address of the specified users
  } {
    if {[info exists :pa($uid)]} {
      return [set :pa($uid)]
    } else {
      return ""
    }
  }
  Users proc last_click {uid} {
    if {[info exists :timestamp($uid)]} {
      return [set :timestamp($uid)]
    } else {
      return 0
    }
  }
  Users proc last_requests {uid} {
    set urls {}
    if {[info exists :pa($uid)]} {
      foreach i [Users info instances] {
        if {[$i exists urls($uid)]} {
          foreach u [$i set urls($uid)] { lappend urls $u }
        }
      }
      set urls [lsort -index 0 $urls]
    }
    return $urls
  }

  Users proc active_communities {} {
    foreach i [Users info instances] {
      lappend communities \
          [list [$i point_in_time] [$i array names in_community]]
      foreach {c names} [$i array get in_community] {
        lappend community($c) $names
      }
    }
    return [array get community]
  }

  Users proc nr_active_communities {} {
    foreach i [Users info instances] {
      foreach c [$i array names in_community] {
        set community($c) 1
      }
    }
    set n [array size community]
    return [incr n -1];   # subtract "non-community" with empty string id
  }

  Users proc in_community {community_id} {
    set users [list]
    foreach i [Users info instances] {
      if {[$i exists in_community($community_id)]} {
        set time [$i point_in_time]
        foreach u [$i set in_community($community_id)] {
          lappend users [list $time $u]
        }
      }
    }
    return $users
  }

  Users proc current_object {} {
    set now   [clock seconds]
    set mkey  [expr { ($now / 60) % [throttler timeWindow]}]
    set obj   [self]::users::$mkey

    if {$mkey ne ${:last_mkey}} {
      if {${:last_mkey} ne ""} {:purge_access_stats}
      # create or recreate the container object for that minute
      if {[nsf::is object $obj]} {
        $obj destroy
      }
      Users create $obj -point_in_time $now
      set :last_mkey $mkey
    }
    return $obj
  }

  Users proc purge_access_stats {} {
    set time [clock seconds]
    # purge stale entries (for low traffic)
    set secs [expr {[throttler timeWindow] * 60}]
    if { [info commands [self]::users::${:last_mkey}] ne ""
         && $time - [[self]::users::${:last_mkey} point_in_time] > $secs
       } {
      # no requests for a while; delete all objects under [self]::users::
      Object create [self]::users
    } else {
      # delete selectively
      foreach element [[self]::users info children] {
        if { [$element point_in_time] < $time - $secs } {$element destroy}
      }
    }
  }

  Users proc community_access {requester pa community_id} {
    [:current_object] community_access $requester $pa $community_id
  }

  Users proc entered_community {key now community_id data reason} {
    #ns_log notice "=== user $key entered community $community_id at $now reason $reason"
    set :user_in_community($key) [dict replace $data \
                                      community_id $community_id \
                                      community_clicks 1 \
                                      community_start $now]
  }

  Users proc left_community {key pa now community_id data reason} {
    set seconds [expr {$now - [dict get $data community_start]}]
    set clicks [dict get $data community_clicks]
    dict unset data community_start
    dict unset data community_clicks
    dict unset data community_id
    set :user_in_community($key) $data
    #ns_log notice "=== user $key left community $community_id at $now reason $reason after $seconds seconds clicks $clicks"
    if {[do_track_activity] && $seconds > 0} {
      xo::job_enqueue [list ::xo::request_monitor_record_community_activity $key $pa $community_id $seconds $clicks $reason]
    }
  }

  Users proc left_system {key pa now data reason} {
    if {[dict exists $data start]} {
      set seconds [expr {$now - [dict get $data start]}]
      set clicks [dict get $data clicks]
    } else {
      if {[info exists :timestamp($key)]} {
        set seconds [expr {$now - [set :timestamp($key)]}]
        set clicks 0
      } else {
        ns_log warning "could not determine online duration <$key> <$pa> data <$data>"
        set seconds -1
        set clicks -1
      }
    }
    #ns_log notice "=== user $key left system at $now reason $reason after $seconds seconds clicks $clicks"
    if {[do_track_activity] && $seconds > 0} {
      xo::job_enqueue [list ::xo::request_monitor_record_activity $key $pa $seconds $clicks $reason]
    }
    unset -nocomplain :user_in_community($key) :refcount($key) :pa($key) :expSmooth($key) :switches($key)
  }

  Users instproc init {} {
    next
    #
    # The following event is a heart-beat just necessary for idle
    # systems. It makes sure that per-minute objects don't hang
    # around much longer than required (maximum 1 second), but that at
    # the same time that last_mkey never points to an invalid object.
    #
    set ms [expr {([time-window] * 60000) + 1000}]
    after $ms [list [self class] current_object]
  }

  Users instproc community_access {key pa community_id} {
    set class [self class]
    set now [clock seconds]
    set var user_in_community($key)

    #ns_log notice "=== [self] community_access $key $community_id have timestamp [$class exists timestamp($key)] in community [$class exists $var]"

    if {[$class exists $var]} {
      #
      # The user was already in a community.
      #
      if {[$class exists timestamp($key)] && [$class set timestamp($key)] == $now } {
        #
        # ignore clicks less than one-second interval (probably embedded content)
        #
        return
      }
      set data [$class set $var]
      set old_community_id [dict get $data community_id]
      if {$old_community_id != $community_id} {
        #
        # The user was in a different community.
        #
        Users left_community $key $pa $now $old_community_id $data switch
        dict incr data clicks
        Users entered_community $key $now $community_id $data switch
      } else {
        dict incr data clicks
        dict incr data community_clicks
        $class set $var $data
      }
    } else {
      #
      # The user was in no community before.
      #
      set data [list start $now clicks 1]
      Users entered_community $key $now $community_id $data new
      set $var 1
    }

    #
    # Keep the currently active users in the per-minute objects.
    #
    set var :user_in_community($key,$community_id)
    if {![info exists $var]} {
      set $var 1
      lappend :in_community($community_id) $key
    }
  }

  Users instproc check_pa_change {key pa url} {
    set class [self class]
    #
    # Check, if we have already a peer address for the given user.
    #
    if {[$class exists pa($key)]} {
      #
      # Check, if the peer address changed. This might be some
      # indication, that multiple users are working under the same
      # user_id, or that the identity was hijacked. Therefore, we
      # note such occurrences.
      #
      if {[$class set pa($key)] ne $pa} {
        $class incr switches($key)
        # log the change
        set timestamp [clock format [clock seconds]]
        switches.log write "$timestamp -- switch -- $key from\
         [$class set pa($key)] to $pa $url"
      }
    } elseif {[$class exists pa($pa)]} {
      #
      # We have for this peer address already an entry. Since we do
      # not want to count this user twice, we assume, that this is the
      # same user, when the requests were within a short time period.
      #
      if {[$class exists timestamp($pa)] && [clock seconds] - [$class set timestamp($pa)] < 60} {
        #ns_log notice "=== turn anonymous user from $pa into authenticated user $key"

        if {[$class exists user_in_community($pa)]} {
          $class set user_in_community($key) [$class set user_in_community($pa)]
        }
        $class incr ip24 -1
        $class set pa($key)        [$class set pa($pa)]
        $class set timestamp($key) [$class set timestamp($pa)]
        $class unset pa($pa)
        $class unset timestamp($pa)
        ns_log notice "UNSET timestamp($pa) turned into timestamp($key)"
      }
    }
  }

  Users instproc addKey {key pa url community_id is_embedded_request} {
    #ns_log notice "=== [self] addKey $key $pa $url '$community_id' $is_embedded_request"
    #
    # This method stores information about the current request partly
    # in the round-robin objects of the specified time windows, and
    # keeps global information in the class objects.
    #
    # key: either user_id or peer address
    # pa:  peer address
    #
    set class [self class]

    if {$key ne $pa} {
      :check_pa_change $key $pa $url
    }

    #
    # Increase the number of requests that were issued from the user
    # in the current minute.
    #
    set counter :active($key)
    if {[incr $counter] == 1} {
      #
      # On the first occurrence in the current minute, increment the
      # global reference count
      #
      $class incrRefCount $key $pa
    }

    if {!$is_embedded_request} {
      set blacklisted_url [expr {[string match "/RrdGraphJS/public/*" $url]
                                 || [string match "/munin/*" $url]
                               }]
      #ns_log notice "=== $url black $blacklisted_url, community_access $key $pa $community_id"
      if {!$blacklisted_url} {
        #
        # Register the fact that the user is doing something in the community
        #
        :community_access $key $pa $community_id
      }

      #
      # Handle logout
      #
      if {[string match "*/logout" $url]} {
        set now [clock seconds]
        set var user_in_community($key)
        if {[$class exists $var]} {
          set data [$class set $var]
          if {[dict exists $data community_id]} {
            #
            # Logout from "community"
            #
            Users left_community $key $pa $now [dict get $data community_id] $data logout
          }
        } else {
          set data ""
        }
        #
        # Logout from the system
        #
        Users left_system $key $pa $now $data logout
      }
    }

    #
    # The array "urls" keeps triples of timestamps, URLs and peer
    # addresses per user.
    #
    lappend :urls($key) [list ${:point_in_time} $url $pa]

    #
    # The global array "hits" keeps overall activity of the user.
    #
    $class incr hits($key)
    $class set timestamp($key) [clock seconds]
    #ns_log notice "[self] addKey ENDS  $class timestamp($key) [$class set timestamp($key)] counter $counter value [set $counter]"
  }

  Users instproc add_view {uid} {
    # :log "#### add_view $uid"
    incr :views($uid)
  }
  Users proc views_per_minute {uid} {
    set mins 0
    set views 0
    set key views($uid)
    foreach i [Users info instances] {
      if {[$i exists $key]} {
        incr mins
        incr views [$i set $key]
      }
    }
    if {$mins > 0} {
      return [expr {$views*1.0/$mins}]
    }
    return 0
  }

  Users instproc destroy {} {
    set class [self class]
    #ns_log notice "=== [self] destroy [array names :active]"
    if {[Users exists last_mkey] && [Users set last_mkey] eq [self]} {
      Users set last_mkey ""
    }
    foreach key [array names :active] {
      if {[::xo::is_ip $key]} {
        set pa $key
      } else {
        set pa [expr {[$class exists pa($key)] ? [$class set pa($key)] : "unknown"}]
      }
      #ns_log notice "=== [self] destroy: $class exists pa($key) ?[$class exists pa($key)] => '$pa'"
      $class decrRefCount $key $pa [set :active($key)]
    }
    next
  }
  Users proc expSmooth {ts key} {
    set mins [expr {$ts/60}]
    if {[info exists :expSmooth($key)]} {
      lassign [set :expSmooth($key)] _ aggval lastmins hits
      set mindiff [expr {$mins-$lastmins}]
      if {$mindiff == 0} {
        incr hits
        set retval [expr {$aggval*0.3 + $hits*0.7}]
      } else {
        set aggval [expr {$aggval*pow(0.3,$mindiff) + $hits*0.7}]
        set hits 1
      }
    } else {
      set hits 1
      set aggval 1.0
    }
    if {![info exists retval]} {set retval $aggval}
    set :expSmooth($key) [list $retval $aggval $mins $hits]
    return $retval
  }

  Users proc incrRefCount {key pa} {
    #
    # This method is called whenever the user (key) was seen the first
    # time in the current minute.
    #
    if {[incr :refcount($key)] == 1} {
      #
      # We saw the user for the first time ever, so increment as well
      # the counters of logged-in and not logged-in users.... but not
      # in cases, where the timestamp data was restored.
      #
      if {![info exists :timestamp($key)]} {
        if {[::xo::is_ip $key]} {incr :ip24} {incr :auth24}
      }
    }
    set :pa($key) $pa
  }

  Users proc decrRefCount {key pa hitcount} {
    #ns_log notice "=== decrRefCount $key $hitcount"
    if {[info exists :refcount($key)]} {
      set x [incr :refcount($key) -1]
      incr :hits($key) -$hitcount
      if {$x < 1} {
        #
        # The user fell out of the per-minute objects due to
        # inactivity.
        #
        set var :user_in_community($key)
        if {[info exists $var]} {
          set data [set $var]
          Users left_community $key $pa [clock seconds] [dict get $data community_id] $data inactive
          Users left_system $key $pa [clock seconds] $data inactive
        } else {
          Users left_system $key $pa [clock seconds] {} inactive
          if {![::xo::is_ip $key]} {
            #
            # It is ok, when the user has only accessed blackisted
            # content, but when the user was logged in, this should
            # not happen - it is at least unusual
            #
            set address [expr {[info exists :pa($pa)] ? "peer address [set :pa($pa)]" : ""}]
            ns_log warning "no community info for $key available $address"
          }
        }
      }
    } else {
      #Users left_system $key $pa [clock seconds] {} inactive-error
      ns_log notice "no refcount for $key available, probably explicit logout"
    }
  }

  Users proc forget_community {community_id} {
    #
    # Forget all the data about users in a community, meant to be
    # called when a community is being deleted, so that we stop its
    # tracking.
    #
    foreach {key data} [array get :user_in_community] {
      if {[dict get $data community_id] == $community_id} {
        unset -nocomplain :user_in_community($key)
      }
    }
    foreach i [Users info instances] {
      $i unset -nocomplain in_community($community_id)
    }
  }

  Users proc compute_nr_users_per_day {} {
    #
    # this method is just for maintenance issues and updates the
    # aggregated values of the visitors
    #
    set :ip24 0
    set :auth24 0
    foreach i [array names :timestamp] {
      if {[::xo::is_ip $i]} {incr :ip24} {incr :auth24}
    }
  }

  Users proc nr_users_per_day {} {
    return [list ${:ip24} ${:auth24}]
  }
  Users proc users_per_day {} {
    set ip [list]; set auth [list]
    foreach i [array names :timestamp] {
      if {[::xo::is_ip $i]} {
        set var ip
      } else {
        set var auth
      }
      lappend $var [list $i [set :timestamp($i)]]
    }
    return [list $ip $auth]
  }

  Users proc time_window_cleanup {} {
    #
    # Purge stale entries (maintenance only)
    #

    #ns_log notice "=== time_window_cleanup"
    set now [clock seconds]
    set maxdiff [expr {[throttler timeWindow] * 60}]

    foreach i [array names :pa] {
      if {![info exists :timestamp($i)]
        || ($now - [set :timestamp($i)] > $maxdiff)
      } {
        #ns_log notice "=== time_window_cleanup unsets pa($i)"
        unset -nocomplain :pa($i) :refcount($i) :expSmooth($i) :switches($i)
      }
    }

    foreach i [array names :refcount] {
      if {![info exists :pa($i)]} {
        #ns_log notice "throttle: void refcount for $i"
        unset :refcount($i)
      }
    }
  }

  Users proc perDayCleanup {} {
    #
    # Get rid of overdue elements.
    #
    :time_window_cleanup
    #
    # Refresh per day counter.
    #
    set :ip24 0
    set :auth24 0
    set secsPerDay [expr {3600*24}]
    set now [clock seconds]
    foreach i [array names :timestamp] {
      if {$now - [set :timestamp($i)] > $secsPerDay} {
        unset :timestamp($i)
      } else {
        if {[::xo::is_ip $i]} {incr :ip24} {incr :auth24}
      }
    }
    #
    # Save a dump, in case we have an unexpected restart.
    #
    #ns_log notice "=== auth24 perDayCleanup -> ${:ip24} ${:auth24}"
    dump write
  }

  ad_proc -private ::unmap_pool {
    {-pool slow}
    {-ms}
    method
    url
  } {
    Function within throttle monitor thread for registering pool
    unmapping requests after a specified time. This function has to run
    in this thread to be able to use "::after".
  } {
    if {![info exists ms]} {
      set ms [::map-slow-pool-duration]
    }
    after $ms [list ::xo::unmap_pool -pool $pool $method $url]
    ns_log notice "slow request: mapping of '$url' moved to '$pool' connection pool will be canceled in $ms ms"
  }

  Object create dump
  dump set file ${logdir}/throttle-data.dump
  dump proc read {} {
    # make sure, timestamp exists as an array
    array set Users::timestamp [list]
    if {[ad_file readable ${:file}]} {
      # in case of disk-full, the file might be damaged, so make sure,
      # we can continue
      if {[catch {source ${:file}} errorMsg]} {
        ns_log error "during source of ${:file}:\n$errorMsg"
      }
    }
    # The dump file data is merged with maybe preexisting data
    # make sure to adjust the counters and timings.
    Users time_window_cleanup
    Users compute_nr_users_per_day
    #
    # When old data is restored, don't trust user-info unless it is
    # very recent (e.g. younger than 3 munutes)
    #
    if {[ad_file readable ${:file}] && ([clock seconds] - [ad_file mtime ${:file}] > 180)} {
      Users array unset user_in_community
    }
  }
  dump proc collect {} {
    set cmds {}
    #
    # Dump most variables of the object ::Users
    #
    set o ::Users
    foreach var [$o info vars] {
      #
      # No need to preserve "last_mkey" (just for internal purposes)
      # and "hits" (might be large).
      #
      if {$var in {last_mkey hits}} {
        continue
      }

      #
      # The remainder are primarily run time statistics
      #
      if {[$o array exists $var]} {
        lappend cmds [list $o array set $var [$o array get $var]]
      } else {
        lappend cmds [list $o set $var [$o set $var]]
      }
    }
    return $cmds
  }

  dump proc write {{-sync false}} {
    set cmds [:collect]
    if {$sync} {
      set dumpFile [open ${:file} w]
      puts -nonewline $dumpFile [join $cmds \n]\n
      close $dumpFile
    } else {
      file delete -force -- ${:file}
      set dumpFile [AsyncLogFile new -filename ${:file}]
      #
      # Write the content in smaller chunks.
      #
      foreach cmd $cmds {
        $dumpFile write $cmd
      }
      $dumpFile destroy
    }
  }

  # dump proc write {{-sync false}} {
  #   # -sync is currently ignored
  #   ns_job queue -detached async-cmd [subst {
  #     set dumpFile \[open ${:file} w\]
  #     puts \$dumpFile [list [join [:collect] \n]]
  #     close \$dumpFile
  #   }]
  # }

  # initialization of Users class object
  #Users perDayCleanup
  Object create Users::users
  Users set last_mkey ""

  # for debugging purposes: return all running timers
  proc showTimers {} {
    set _ ""
    foreach t [after info] { append _ "$t [after info $t]\n" }
    return $_
  }

  #
  # define a class value, which refreshes itself all "refresh" ms.
  #
  Class create Value -parameter {{value ""} {refresh 10000}}
  Value instproc updateValue {} {set :handle [after ${:refresh} [list [self] updateValue]]}

  #
  # Define an object loadAvg.
  #
  # query with: "throttle do loadAvg value"
  #
  Value create loadAvg
  loadAvg proc updateValue {} {
    set procloadavg /proc/loadavg
    if {[ad_file readable $procloadavg]} {
      set f [open $procloadavg];
      set :value [lrange [read $f] 0 2]
      close $f
    }
    next
  }
  loadAvg updateValue

  set tail [::util::which tail]
  if {[ad_file readable ${logdir}/counter.log] && $tail ne ""} {
    #
    # Populate the counters from log file
    #
    ns_log notice "+++ request-monitor: initialize counters"

    # Create the file to load. This is per hour = 60*3 + 2 lines
    set number_of_lines [expr {182 * [trend-elements]}]

    try {
      exec $tail -n $number_of_lines ${logdir}/counter.log >${logdir}/counter-new.log
      set f [open $logdir/counter-new.log]
      while {-1 != [gets $f line]} {
        regexp {(.*) -- (.*) ::(.*) (.*)} $line match timestamp server counter value
        #ns_log notice "$counter add_value $timestamp $value"
        if {[nsf::is object $counter]} {
          $counter add_value $timestamp $value
        } elseif {![info exists complain($counter)]} {
          ns_log notice "request-monitor: ignore reload of value $value for counter $counter"
          set complain($counter) 1
        }
      }
    } on error {errorMsg} {
      ns_log Warning "+++ request-monitor: error initializing counters: $errorMsg"
    } finally {
      if {[info exists f]} {
        close $f
        unset f
      }
    }
  }

  #
  # Read in the last dump data
  #
  dump read

  #
  # Add an exit handler to write out the dump, when this thread goes
  # down.
  #
  ::xotcl::Object setExitHandler {
    ns_log notice "::throttle: exiting"
    dump write -sync true
    #
    # Delete all users objects, that will flush all activity data to
    # the tables if configured
    #
    foreach obj [Users info instances] {$obj destroy}

    ns_log notice "::throttle specific exist handler finished"
  }

  #ns_log notice "============== Thread initialized ===================="

} -persistent 1 -ad_doc {
  This is a small request-throttle application that handles simple
  DoS-attacks on the server.  A user (request key) is identified
  via ipAddr or some other key, such as an authenticated userid.
  <p>
  XOTcl Parameters for Class <a
  href='/xotcl/show-object?object=%3a%3athrottle+do+%3a%3aThrottle'>Throttle</a>:
  <ul>
  <li><em>timeWindow:</em>Time window for computing detailed statistics; can
  be configured via OpenACS package parameter <code>time-window</code></li>
  <li><em>timeoutMs:</em> Time window to keep statistics for a user</li>
  <li><em>startThrottle:</em> If user requests more than this #, her
  requests are delayed. When larger than toMuch, the parameter is ignored</li>
  <li><em>toMuch:</em> If user requests more than this #, she is kicked out</li>
  </ul>
  The throttler is defined as a class running in a detached thread.
  See <a href='/api-doc/procs-file-view?path=packages/xotcl-core/tcl/40-thread-mod-procs.tcl'>XOTcl
  API for Thread management</a> for more details.
  It can be subclassed to define e.g. different kinds of throttling policies for
  different kind of request keys. Note that the throttle thread itself
  does not block, only the connection thread blocks if necessary (on throttles).
  <p>
  The controlling thread contains the classes
  <a href='/xotcl/show-object?object=%3a%3athrottle+do+%3a%3aUsers'>Users</a>,
  <a href='/xotcl/show-object?object=%3a%3athrottle+do+%3a%3aThrottle'>Throttle</a>,
  <a href='/xotcl/show-object?object=%3a%3athrottle+do+%3a%3aCounter'>Counter</a>,
  <a href='/xotcl/show-object?object=%3a%3athrottle+do+%3a%3aMaxCounter'>MaxCounter</a>, ...
  @author Gustaf Neumann
  @cvs-id $Id$
}

throttle proc destroy {} {
  #puts stderr throttle-DESTROY
  ns_log notice throttle-DESTROY-shutdownpending->[ns_info shutdownpending]
  #::xo::show_stack
  if {[ns_info shutdownpending] && [nsv_exists ::xotcl::THREAD [self]]} {
    set tid [nsv_get ::xotcl::THREAD [self]]
    ns_log notice =========throttle-DESTROY-shutdown==========================$tid-??[::thread::exists $tid]
    if {[::thread::exists $tid]} {
      ns_log notice =========throttle-DESTROY-shutdown==========================THREAD-EXISTS
      set refcount [::thread::release $tid]
      ns_log notice throttle-DESTROY-shutdownpending->[ns_info shutdownpending]-refCount$refcount
    }
  }
  next
}

#
# Use the feature of connection pool unmapping to determine, if we can
# use "ns_conn partialtimes". We can't use the latter directly, since
# this file is typically loaded from a non-connection thread.
#
if {![::acs::icanuse "ns_conn partialtimes"]} {
  #
  # Older version of NaviServer or AOLserver
  #
  throttle proc partialtimes {} {
    set s [ns_conn start]
    set t [ns_time diff [ns_time get] $s]
    set ms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}]
    return [list start $s ms $ms runtime [expr {$ms/1000.0}] filtertime 0 queuetime 0 accepttime 0]
  }
} else {
  #
  # Use variant based on "ns_conn partialtimes"
  #
  throttle proc partialtimes {} {
    set s [ns_conn start]
    set d [ns_conn partialtimes]
    set t [ns_time diff [ns_time get] $s]
    lappend d \
        ms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}] \
        start $s
    return $d
  }
}
throttle proc ms {-start_time} {
  if {![info exists start_time]} {set start_time [ns_conn start]}
  set t [ns_time diff [ns_time get] $start_time]
  set ms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}]
  return $ms
}

throttle proc get_context {} {
  # :log "--t [info exists :context_initialized] url=[ns_conn url]"
  if {[info exists :context_initialized]} return

  #
  # In case, the connection got terminated due to e.g. invalid URLs
  # earlier, fall back to URL "/" to avoid hard DB errors resulting
  # from the sitemap lookup of the URL.
  #
  if {[ns_conn isconnected]} {
    set :url [ns_conn url]
    #ns_log notice "URL <${:url}> invalid? [regexp  {[^[:print:]]} ${:url}]"
  } else {
    set :url /
  }
  set :method [ns_conn method]

  set :community_id 0
  if {[info exists ::ad_conn(package_id)]} {
    set :community_id [ad_conn subsite_id]
    # :log "--t we have a package_id"
    # ordinary request, ad_conn is initialized
    set package_id [ad_conn package_id]
    ::xo::ConnectionContext require -package_id $package_id -url ${:url}
    if {[info commands dotlrn_community::get_community_id_from_url] ne ""} {
      set community_id [dotlrn_community::get_community_id_from_url -url ${:url}]
      if {$community_id ne ""} {
        set :community_id $community_id
      }
    }
  } else {
    #
    # Requests for /resources/* land here
    #
    # :log "--t we have no package_id , subsite_id ?[info exists ::ad_conn(subsite_id)] [ns_conn url]"
    ::xo::ConnectionContext require -url ${:url}
  }

  set :requester [::xo::cc requester]
  set :user      [::xo::cc user]
  set :query     [ad_conn query]
  set :pa        [ad_conn peeraddr]
  if {${:query} ne ""} {
    append :url ?${:query}
  }
  #ns_log notice log "### setting url to ${:url} Q=${:query}"
  #xo::show_stack
  set :context_initialized 1
  # :log "--i leaving [ns_conn url] vars=[lsort [info vars]]"
}

throttle ad_proc check {} {

  This method should be called once per request that is monitored.  It
  should be called after authentication such we have already the
  userid if the user is authenticated.

} {
  #set t0 [clock milliseconds]

  :get_context
  # :log "### check"

  #
  # We could as well pass the whole header set via
  #
  #  {*}[ns_set array [ns_conn headers]]
  #
  # but since this code is time critical, just pass the information
  # actually needed.
  #
  set hdrs [ns_conn headers]
  lassign [:throttle_check ${:requester} ${:pa} ${:url} \
               [ns_conn start] [ns_guesstype [ns_conn url]] ${:community_id} \
               [list \
                    pool [ns_conn pool] \
                    Sec-Fetch-Dest [ns_set iget $hdrs Sec-Fetch-Dest] \
                    X-Requested-With [ns_set iget $hdrs X-Requested-With] \
                    Range [ns_set iget $hdrs Range] \
                   ]] \
      toMuch ms repeat
  #set t1 [clock milliseconds]

  #
  # result == 0 OK
  # result < 0 blocked
  # result > 0 This web server is only open for interactive usage
  #
  if {$repeat > 0} {
    :add_statistics repeat ${:requester} ${:pa} ${:url} ${:query}
    if {$repeat > 1} {
      set result 1
    } else {
      set result -1
    }
  } elseif {$toMuch} {
    :log "*** we have to refuse user ${:requester} with $toMuch requests"
    :add_statistics reject ${:requester} ${:pa} ${:url} ${:query}
    set result $toMuch
  } elseif {$ms} {
    :log "*** we have to block user ${:requester} for $ms ms"
    :add_statistics throttle ${:requester} ${:pa} ${:url} ${:query}
    after $ms
    :log "*** continue for user ${:requester}"
    set result 0
  } else {
    set result 0
  }
  #set tend [clock milliseconds]
  #if {$tend - $t0 > 500} {
  #  ns_log warning "throttle_filter slow, can lead to filter time >1sec: total time [expr {$tend - $t0}], t1 [expr {$t1 - $t0}]"
  #}

  return $result
}
####
# The following procs are forwarder to the monitoring thread
# for convenience.
####
throttle forward statistics              %self do throttler %proc
throttle forward url_statistics          %self do throttler %proc
throttle forward add_url_stat            %self do throttler %proc
throttle forward flush_url_stats         %self do throttler %proc
throttle forward report_url_stats        %self do throttler %proc
throttle forward add_statistics          %self do throttler %proc
throttle forward throttle_check          %self do throttler %proc
throttle forward last100                 %self do throttler %proc
throttle forward thread_avgs             %self do throttler %proc
throttle forward off                     %self do throttler set do_throttle 0
throttle forward on                      %self do throttler set do_throttle 1
throttle forward running                 %self do throttler %proc
throttle forward server_threads          %self do throttler %proc
throttle forward nr_running              %self do throttler array size running_url
throttle forward trend                   %self do %1 set trend
throttle forward max_values              %self do %1 set stats
throttle forward purge_access_stats      %self do Users %proc
throttle forward users                   %self do Users
throttle forward views_per_minute        %self do Users %proc
throttle forward user_is_active          %self do Users %proc

####
# the next procs are for the filters (registered from the -init file)
####
throttle proc postauth args {
  # :log "+++ [self proc] [ad_conn url] auth ms [:partialtimes] [ns_conn isconnected]"
  # :do set ::cookies(${:requester}) [ns_set get [ns_conn headers] Cookie]
  set r [:check]
  if {$r < 0} {
    set url ${:url}

    catch {ns_log notice "blocked request for user ${:user} Sec-Fetch-Dest [ns_set iget [ns_conn headers] Sec-Fetch-Dest] url ${:url}"}
    catch { ns_log notice ".... [ns_set array [ns_conn  headers]]" }

    ns_return 429 text/html "
      <h1>[_ xotcl-request-monitor.repeated_operation]</h1>
      [_ xotcl-request-monitor.operation_blocked]<p>"
    return filter_return
  } elseif {$r > 0} {
    ns_return 429 text/html "
      <h1>Invalid Operation</h1>
      This web server is only open for interactive usage.<br>
      Automated copying and mirroring is not allowed!<p>
      Please slow down your requests...<p>"
    return filter_return
  } else {
    # :log "-- filter_ok"
    return filter_ok
  }
}
throttle proc trace args {
  # :log "+++ [self proc] <$args> [ad_conn url] [:partialtimes] [ns_conn isconnected]"
  #
  # OpenACS 5.2 bypasses for requests to /resources the user filter,
  # thesefore, the pre- or postauth are not called in these cases, but
  # only trace.  So we have to make sure we have the needed context
  # here.
  #
  :get_context
  # :log "CT=[ns_set array [ns_conn outputheaders]] -- ${:url}"

  :add_url_stat ${:method} ${:url} [:partialtimes] ${:requester} ${:pa} \
      [ns_set iget [ns_conn outputheaders] Content-Type] [ns_conn pool]
  unset :context_initialized
  return filter_ok
}

throttle proc community_access {community_id} {
  :get_context
  if {${:community_id} eq ""} {
    :users community_access ${:requester} ${:pa} $community_id
  }
}

namespace eval ::xo {

  ad_proc -private ::xo::unmap_pool {
    {-pool slow}
    method
    url
  } {
    ns_server -pool $pool unmap -noinherit [list $method $url]
    ns_log notice "slow request: mapping of ' $method $url' to pool $pool canceled"
  }

  ad_proc -private ::xo::remap_pool {
    {-threshold 3.0}
    {-except {/ /dotlrn/ /dotlrn}}
    {-pool slow}
    -runtime
    method
    url
  } {

    Function for dynamically managing connection pool mappings.  When
    a connection pool "slow", is defined, and the query took longer
    than "threshold" seconds, and the URL is not 'except' list, then
    move this request to the "slow" pool.

  } {
    if {$runtime > $threshold
        && [::acs::icanuse "ns_server unmap"]
        && $pool in [ns_server pools]
        && [ns_server mapped [list $method $url]] eq ""
        && $url ni $except
      } {
      ns_server -pool $pool map -noinherit [list $method $url]
      ns_log notice "slow request: '$url' moved to '$pool' connection pool"

      #
      # In case, we are executing in the throttle monitor thread, call
      # the register unmap function directly, otherwise instruct the
      # monitor thread to do so.
      #
      set prefix [expr {[ns_thread name] eq "::throttle" ? {} : {::throttle do}}]
      {*}$prefix ::unmap_pool -pool $pool $method $url
    }
  }

  ad_proc -private ::xo::pool_remap_watchdog {{-maxWaiting 10} {-maxRunning 100}} {

    Watchdoc function to ensure liveliness of the server.

    This watchdog checks every minute the running jobs and maps very
    slow requests to the slow pool (if configured) to avoid that the
    default pool is getting filled up with more stuck requests.

    The watchdog is managed via an ad_schedule_proc started from the
    init-procs.

  } {
    foreach s [ns_info servers] {
      #
      # Check default connection pool and remap slow request to the
      # "slow" pool when defined.
      #
      set reqs [ns_server -server $s -pool "" active]
      foreach req $reqs {
        set runtime [lindex $req end-1]
        if {$runtime >= 3.0} {
          set method [lindex $req 3]
          set url [lindex $req 4]
          ns_log notice "CALL TRY REMAP ::xo::remap_pool -runtime $runtime $method $url"
          ::xo::remap_pool -runtime $runtime $method $url
        }
      }
      #
      # Check queueing situation for every connection pool and report
      # to sysadmin when things pile up.
      #
      set message ""
      foreach pool [ns_server -server $s pools] {
        set reqs [ns_server -server $s -pool $pool active]
        set waiting [ns_server -server $s -pool $pool waiting]
        set running [llength $reqs]
        if {$waiting >= $maxWaiting || $running >= $maxRunning} {
          set threadInfo [ns_server -server $s -pool $pool threads]
          lappend threadInfo waiting $waiting
          set poolName [expr {$pool eq "" ? "default" : "'$pool'"}]
          set message ""
          append message \
              "Server '$s' on [ad_system_name]: " \
              "more than $maxWaiting requests are waiting " \
              "in connection pool $poolName ($threadInfo)" \n \
              "Currently running requests:" \n \
              "   " [join $reqs "\n   "] \n
        }
      }
      if {$message ne ""} {
        ns_log warning $message
        try {
          #
          # Try to send a mail to the webmaster and include a link to
          # the recommended nsstats location.
          #
          acs_mail_lite::send -send_immediately \
              -to_addr [ad_host_administrator] \
              -from_addr [ad_system_owner] \
              -subject "High load warning on [ad_system_name]" \
              -body "$message\nVisit:  [ad_url]/admin/nsstats/admin/nsstats"
        } on error {errorMsg} {
          ns_log error "Could not send high-load warning: $errorMsg"
        }
      }
    }
  }

  proc is_ip {key} {
    expr { [string match "*.*" $key] || [string match "*:*" $key] }
  }

  #
  # Check, if we have a NaviServer with the atomic ns_set operation
  # with the "-reset" option
  #
  #    "nsv_set ?-default? ?-reset? ?--? array key ?value?"
  #
  # available. If so, implement an async job-queue with little
  # overhead based on it.
  #
  catch {nsv_set} errMsg
  if {[string match *-reset* $errMsg]} {
    #
    # Yes, we have the "-reset" option.
    #
    proc job_enqueue {cmd} {
      nsv_lappend request_monitor jobs $cmd
    }

    proc job_dequeue {} {
      foreach cmd [nsv_set -reset request_monitor jobs {}] {
        {*}$cmd
      }
    }

  } else {
    #
    # Older version of NaviServer, so the classic approach via "ns_job
    # queue -detached async-cmd ...."
    #
    proc job_enqueue {cmd} {
      ns_job queue -detached async-cmd $cmd
    }
  }

  ad_proc -private request_monitor_user_info {key} {
  } {
    if {[nsf::is integer $key]} {
      #
      # It looks like a user_id
      #
      set person [person::get_person_info -person_id $key]
      if {[dict exists $person last_name]} {
        set user_label "[dict get $person last_name], [dict get $person first_names]"
        set user_url [acs_community_member_url -user_id $key]
      } else {
        #
        # Maybe, the user was deleted in the meanwhile
        #
        set user_label "Unknown user_id $key"
        set user_url ""
      }
    } else {
      # it was an IP address
      set user_label $key
      set user_url ""
    }
    return [list label $user_label url $user_url]
  }

  proc request_monitor_record_activity {key pa seconds clicks reason} {
    if {[::xo::is_ip $key]} {
      set user_id -1
    } else {
      set user_id $key
    }
    xo::dc dml add_activity {
      insert into request_monitor_activities (user_id, peer_address, start_time, end_time, clicks, reason)
      values (:user_id, :pa,  now() - :seconds * INTERVAL '1 second', now(), :clicks, :reason)
    }
  }

  proc request_monitor_record_community_activity {key pa community_id seconds clicks reason} {
    if {[::xo::is_ip $key]} {
      set user_id -1
    } else {
      set user_id $key
    }
    xo::dc dml add_community_activity {
      insert into request_monitor_community_activities (user_id, peer_address, community_id, start_time, end_time, clicks, reason)
      values (:user_id, :pa, :community_id, now() - :seconds * INTERVAL '1 second', now(), :clicks, :reason)
    }
  }

  if {[::parameter::get_from_package_key \
           -package_key "xotcl-request-monitor" \
           -parameter "do_track_activity" \
           -default "off"]
    } {
    #
    # Data model for the activity statistics of a full session
    #
    ::xo::db::require table request_monitor_activities {
      user_id      {integer references parties(party_id) on delete cascade}
      peer_address text
      start_time   timestamptz
      end_time     timestamptz
      clicks       integer
      reason       text
    }
    ::xo::db::require index -table request_monitor_activities -col user_id
    ::xo::db::require index -table request_monitor_activities -col start_time -using btree
    ::xo::db::require index -table request_monitor_activities -col end_time -using btree

    #
    # Data model for per-community / per-subsite activity statistics
    #
    # we had previously an FK on community_id to acs_objects:
    #
    #     community_id {integer references acs_objects(object_id) on delete cascade}
    #
    # When a user deletes a community, then also the traces of this
    # activity in the community will be deleted, although the fact
    # that the users did something there will be flushed as well. This
    # can be a problem, when communities are created and deleted
    # frequently. Furthermore, during deletion FK violations might
    # have appeared for the deleting user.
    #
    ::xo::db::require table request_monitor_community_activities {
      user_id      {integer references parties(party_id) on delete cascade}
      peer_address text
      community_id integer
      start_time   timestamptz
      end_time     timestamptz
      clicks       integer
      reason       text
    }
    ::xo::db::require index -table request_monitor_community_activities -col user_id
    ::xo::db::require index -table request_monitor_community_activities -col start_time -using btree
    ::xo::db::require index -table request_monitor_community_activities -col end_time -using btree
    ::xo::db::require index -table request_monitor_community_activities -col community_id
  }
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
