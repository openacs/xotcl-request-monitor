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

  package_parameter max-url-stats      -default 500
  package_parameter time-window        -default 10
  package_parameter trend-elements     -default 48
  package_parameter max-stats-elements -default 5
  package_parameter do_throttle        -default on
  package_parameter do_track_activity  -default off

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
    throttler set off $value
  }

  # get the value from the logdir parameter
  set ::logdir [log-dir]
  if {![file isdirectory $logdir]} {file mkdir $logdir}

  #
  # Create AsyncLogFile class, which is one client of the
  # AsyncDiskWriter from bgdelivery
  #
  Class create AsyncLogFile -parameter {filename {mode a}}
  AsyncLogFile instproc init {} {
    if {![info exists :filename]} {
      set :filename $::logdir/[namespace tail [self]]
    }
    set :handle [bgdelivery do AsyncDiskWriter new -autoflush true]
    bgdelivery do ${:handle} open -filename ${:filename} -mode ${:mode}
  }
  AsyncLogFile instproc write {msg} {
    bgdelivery do ${:handle} async_write $msg\n
  }

  # open the used log-files
  AsyncLogFile create counter.log
  AsyncLogFile create long-calls.log
  AsyncLogFile create switches.log

  #
  # A class to keep simple statistics
  #
  Class create ThrottleStat -parameter { type requestor timestamp ip_address url }

  #
  # class for throtteling eager requestors or to block duplicate requests
  #
  Class create Throttle -parameter {
    {timeWindow 10}
    {timeoutMs 2000}
    {startThrottle 11}
    {toMuch 10}
    {alerts 0} {throttles 0} {rejects 0} {repeats 0}
  }

  Throttle instproc init {} {
    set :off [do_throttle]
    Object create [self]::stats
    Object create [self]::users
    next
  }

  Throttle instproc add_statistics { type requestor ip_address url query } {
    #set furl [expr {$query ne "" ? "$url?$query" : $url}]
    incr :${type}s
    # :log "++++ add_statistics   -type $type -user_id $requestor "
    set entry [ThrottleStat new -childof [self]::stats \
                   -type $type -requestor $requestor \
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
        lappend output [list [$stat type] [$stat requestor] \
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

  Throttle instproc throttle_check {requestKey pa url conn_time content_type community_id} {
    #set t0 [clock milliseconds]

    seconds ++

    :update_threads_state

    set var :running_url($requestKey,$url)
    #
    # Check first, whether the same user has already the same request
    # issued; if yes, block this request. Caveat: some html-pages
    # use the same image in many places, so we can't block it.
    #
    set is_embedded_request [expr {
                                   [string match "image/*" $content_type]
                                   || $content_type in { text/css application/javascript application/x-javascript }
                                 }]
    if {[info exists $var] && !$is_embedded_request && !${:off}} {
      #ns_log notice  "### already $var"
      return [list 0 0 1]
    } else {
      set $var $conn_time
      #ns_log notice  "### new $var"
    }
    #set t1 [clock milliseconds]
    :register_access $requestKey $pa $url $community_id $is_embedded_request
    #set t2 [clock milliseconds]

    #if {$t2 - $t0 > 500} {
    #  ns_log warning "throttle_check slow, can lead to filter time >1sec: total time [expr {$t2 - $t0}], t1 [expr {$t1 - $t0}]"
    #}

    #
    # Allow up to 14 requests to be executed concurrently.... the
    # number of 14 is arbitrary. One of our single real request might
    # have up to 14 subrequests (through iframes)....
    #
    if {${:off} || $is_embedded_request || [array size :running_url] < 14} {
      #
      # Maybe the throttler is off, or we have an embedded request or
      # less than 14 running requests running. Everything is
      # fine, let people do what they want.
      #
      return [list 0 0 0]

    } else {
      #
      # Check, whether the last request from a user was within
      # the minimum time interval. We are not keeping a full table
      # of all request keys, but use an timeout triggered mechanism
      # to keep only the active request keys in an associative array.
      #
      incr :alerts
      if {[info exists :active($requestKey)]} {
        # if more than one request for this key is already active,
        # return blocking time
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

  Throttle instproc add_url_stat {method url partialtimes key pa content_type} {
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

  TraceLongCalls instproc add_url_stat {method url partialtimes key pa content_type} {
    regexp {^([^?]+)[?]?(.*)$} $url . url query
    #
    # conntime: time spent in connection thread in ms, not including queuing times
    # totaltime: time since start of the request
    #
    set conntime [expr {int(([dict get $partialtimes runtime] + [dict get $partialtimes filtertime]) * 1000)}]
    set totaltime [dict get $partialtimes ms]

    #ns_log notice "conntime $conntime totaltime $totaltime url=<$url>"
    if { $url in {/register/ / /dotlrn/} } {
      #
      # Calculate for certain URLs separate statistics
      #
      incr ::agg_time($url) $totaltime
      incr ::count(calls:$url)
    }

    #
    # Handling of longcalls counter
    #
    if {$conntime > 3000} {
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
      # Add url, in case it is not too long
      #
      set ql [string length $query]
      if {$ql > 0 && $ql < 60} {
        set loggedUrl $url?$query
      } else {
        set loggedUrl $url
      }

      #
      # Finally, log the entry with to log/long-calls.log
      #
      catch {:log [list $loggedUrl $partialtimes $key $pa $content_type]}

    }
    next
  }

  #
  # Simple means for banning users, e.g. performing too eager
  # requests.  Requests from banned users receive a "duplicate
  # request" reply.
  #
  Class create BanUser
  # BanUser instproc throttle_check {requestKey pa url conn_time content_type community_id} {
  #   #if {$requestKey eq 37958315} {return [list 0 0 1]}
  #   #if {[string match 155.69.25.* $pa]} {return [list 0 0 1]}
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
    @param stats stats keeps nr_stats_elements highest values with time stamp. These hold a list of lists of the actual stats in the form {time value}. Time is given like "Thu Sep 13 09:17:30 CEST 2007". This is used for displaying the Max values
    @param last
    @param trend  trend keeps nr_trend_elements most recent values. This is used for displaying the graphics
    @param c
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
    # stats keeps nr_stats_elements highest values with time stamp
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

  # The counter user_count_day just records the number of active user
  # per day. It differs from other counters by keeping track of a pair
  # of values (authenticated and non-authenticated).

  Counter user_count_day -timeoutMs [expr {60000*60}] -logging 1
  user_count_day proc end {} {
    lassign [throttle users nr_users_per_day] auth ip
    set now [clock format [clock seconds]]
    # The counter logs its intrinsic value (c) anyhow, which are the
    # authenticated users. We also want to record the number of
    # unauthenticated users, and do this here manually.
    :log_to_file $now [self]-non-auth $ip
    set :c $auth
    Users perDayCleanup
    next
  }

  Class create MaxCounter -superclass Counter -instproc end {} {
    set :c [Users nr_active]
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

  UrlCounter instproc add_url_stat {url ms requestor} {
    #ns_log notice "UrlCounter.add_url_stat($url,$ms,$requestor)"
    my ++
    # :log "[self proc] $url /$ms/ $requestor (${:c})"
    incr :t $ms

    ### set up a value for the right ordering in last 100.
    ### We take the difference in seconds since start, multiply by
    ### 10000 (there should be no overflow); there should be less
    ### than this number requests per minute.
    set now [clock seconds]
    set order [expr {($now-[[self class] set seconds])*10000 + ${:c}}]
    set :last100([expr {$order%99}]) [list $now $order $url $ms $requestor]

    set has_param [regexp {^(.*)[?]} $url _ url]
    if {$has_param} {set url $url?...}

    ### Add statistics in detail
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
    # truncate statistics if necessary
    set max [max-url-stats]
    if {$max>1} {
      set result [:url_stats]
      set l [llength $result]
      for {set i $max} {$i<$l} {incr i} {
        set url [lindex $result $i 0]
        unset :stat($url) :cnt($url)
      }
      set result [lrange $result 0 $max-1]
      return $result
    }
    return ""
  }
  UrlCounter instproc cleanup_stats {} {
    # truncate statistics if necessary
    # :check_truncate_stats
    # we use the timer to check other parameters as well here
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
    # each time the timer runs out, perform the cleanup
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

  Users ad_proc active {-full:switch}  {
    Return a list of lists containing information about current
    users. If the switch 'full' is used this list contains
    these users who have used the server within the
    monitoring time window (per default: 10 minutes). Otherwise,
    just a list of requestors (user_ids or peer addresses for unauthenticated
                               requests) is returned.
    <p>
    If -full is used for each requestor the last
    peer address, the last timestamp, the number of hits, a list
    of values for the activity calculations and the number of ip-switches
    the user is returned.
    <p>
    The activity calculations are performed on base of an exponential smoothing
    algorithm which is calculated through an aggregated value, a timestamp
    (in minutes) and the number of hits in the monitored time window.
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

  Users ad_proc nr_users_time_window {} {
    @return number of different ip addresses and authenticated users (in time window)
  } {
    set ip 0; set auth 0
    foreach i [array names :pa] {
      if {[::xo::is_ip $i]} {incr ip} {incr auth}
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
    if {[info exists :pa($uid)]} {
      set urls [list]
      foreach i [Users info instances] {
        if {[$i exists urls($uid)]} {
          foreach u [$i set urls($uid)] { lappend urls $u }
        }
      }
      return [lsort -index 0 $urls]
    } else { return "" }
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
    set now     [clock seconds]
    set mkey     [expr { ($now / 60) % [throttler timeWindow]}]
    set obj     [self]::users::$mkey

    if {$mkey ne ${:last_mkey}} {
      if {${:last_mkey} ne ""} {:purge_access_stats}
      # create or recreate the container object for that minute
      if {[:isobject $obj]} {
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

  Users proc community_access {requestor pa community_id} {
    [:current_object] community_access $requestor $pa $community_id
  }

  Users proc entered_community {key now community_id data reason} {
    ns_log notice "=== user $key entered community $community_id at $now reason $reason"
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
      #set t0 [clock milliseconds]
      ns_job queue -detached async-cmd \
          [list ::xo::request_monitor_record_community_activity $key $pa $community_id $seconds $clicks $reason]
      #set t1 [clock milliseconds]
      #if {$t1 - $t0 > 500} {
      #  ns_log warning "request_monitor_record_community_activity left_community slow, can lead to filter time >1sec: total time [expr {$t1 - $t0}]"
      #}
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
    ns_log notice "=== user $key left system at $now reason $reason after $seconds seconds clicks $clicks"
    if {[do_track_activity] && $seconds > 0} {
      #set t0 [clock milliseconds]
      ns_job queue -detached async-cmd \
          [list ::xo::request_monitor_record_activity $key $pa $seconds $clicks $reason]
      #set t1 [clock milliseconds]
      #if {$t1 - $t0 > 500} {
      #  ns_log warning "::xo::request_monitor_record_activity left_system slow, can lead to filter time >1sec: total time [expr {$t1 - $t0}]"
      #}
    }
    unset -nocomplain :user_in_community($key) :refcount($key) :pa($key) :expSmooth($key) :switches($key)
  }

  Users instproc init {} {
    next
    #
    # The following event is a heart-beat just necessary for idle
    # systems. It makes sure, that per-minute objects don't hang
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
      # user_id, or that the identity was highjacked. Therefore, we
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
        ns_log notice "=== turn anonymous user from $pa into authenticated user $key"

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
    # in the round-robbin objects of the specified time windows, and
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
      set blacklisted_url [expr {[string match /RrdGraphJS/public/* $url]
                                 || [string match /munin/* $url]
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
    # The array "urls" keeps triples of time stamps, urls and peer
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
    if {[Users set last_mkey] eq [self]} {
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
    # Whis method is called whenever the user (key) was seen the first
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
    #ns_log notice "=== time_window_cleanup"
    # purge stale entries (maintenance only)
    set now [clock seconds]
    set maxdiff [expr {[throttler timeWindow] * 60}]
    foreach i [lsort [array names :pa]] {
      set purge 0
      if {![info exists :timestamp($i)]} {
        ns_log notice "throttle: no timestamp for $i"
        set purge 1
      } else {
        set age [expr {$now - [set :timestamp($i)]}]
        if {$age > $maxdiff} {
          if {[info exists :pa($i)]} {
            ns_log notice "throttle: entry stale $i => [info exists :pa($i)], age=$age"
            set purge 1
          }
        }
      }
      if {$purge} {
        ns_log notice "=== time_window_cleanup unsets pa($i)"
        unset -nocomplain :pa($i) :refcount($i) :expSmooth($i) :switches($i)
      }
    }
    foreach i [lsort [array names :refcount]] {
      if {![info exists :pa($i)]} {
        ns_log notice "throttle: void refcount for $i"
        unset :refcount($i)
      }
    }
  }

  Users proc perDayCleanup {} {
    set :ip24 0
    set :auth24 0
    set secsPerDay [expr {3600*24}]
    foreach i [lsort [array names :timestamp]] {
      set secs [expr {[clock seconds]-[set :timestamp($i)]}]
      # :log "--- $i: last click $secs secs ago"
      if {$secs > $secsPerDay} {
        #foreach {d h m s} [clock format [expr {$secs-$secsPerDay}] \
                                    #               -format {%j %H %M %S}] break
        #regexp {^[0]+(.*)$} $d match d
        #regexp {^[0]+(.*)$} $h match h
        #incr d -1
        #incr h -1
        # :log "--- $i expired $d days $h hours $m minutes ago"
        unset :timestamp($i)
        ns_log notice "UNSET timestamp($i) deleted due to perDayCleanup after $secs seconds (> $secsPerDay)"
      } else {
        if {[::xo::is_ip $i]} {incr :ip24} {incr :auth24}
      }
    }
    #ns_log notice "=== auth24 perDayCleanup -> ${:ip24} ${:auth24}"
    dump write
  }

  Object create dump
  dump set file ${logdir}/throttle-data.dump
  dump proc read {} {
    # make sure, timestamp exists as an array
    array set Users::timestamp [list]
    if {[file readable ${:file}]} {
      # in case of disk-full, the file might be damaged, so make sure,
      # we can continue
      if {[catch {source ${:file}} errorMsg]} {
        ns_log error "during source of ${:file}:\n$errorMsg"
      }
    }
    # The dump file data is merged with maybe preexisting data
    # make sure to adjust the counters and timings
    Users time_window_cleanup
    Users compute_nr_users_per_day
    #
    # When old data is restored, don't trust user-info unless it is
    # very recent (e.g. younger than 3 munutes)
    #
    if {[file readable ${:file}] && ([clock seconds] - [file mtime ${:file}] > 180)} {
      Users array unset user_in_community
    }
  }
  dump proc write {{-sync false}} {
    set cmd ""
    # dump all variables of the object ::Users
    set o ::Users
    foreach var [$o info vars] {
      # last_mkey is just for internal purposes
      if {$var eq "last_mkey"} continue
      # the remainder are primarily runtime statistics
      if {[$o array exists $var]} {
        append cmd [list $o array set $var [$o array get $var]] \n
      } else {
        append cmd [list $o set $var [$o set $var]] \n
      }
    }
    if {$sync} {
      set dumpFile [open ${:file} w]
      puts -nonewline $dumpFile $cmd
      close $dumpFile
    } else {
      set dumpFile [bgdelivery do AsyncDiskWriter new]
      bgdelivery do $dumpFile open -filename ${:file}
      bgdelivery do $dumpFile async_write $cmd
      bgdelivery do $dumpFile close
    }
  }

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
  # define a object loadAvg.
  #
  # query with: "throttle do loadAvg value"
  #
  Value create loadAvg
  loadAvg proc updateValue {} {
    set procloadavg /proc/loadavg
    if {[file readable $procloadavg]} {
      set f [open $procloadavg];
      set :value [lrange [read $f] 0 2]
      close $f
    }
    next
  }
  loadAvg updateValue

  set tail [::util::which tail]
  if {[file readable ${logdir}/counter.log] && $tail ne ""} {
    #
    # Populate the counters from log file
    #
    ns_log notice "+++ request-monitor: initialize counters"

    # Create the file to load. This is per hour = 60*3 + 2 lines
    set number_of_lines [expr {182 * [trend-elements]}]
    exec $tail -n $number_of_lines ${logdir}/counter.log >${logdir}/counter-new.log

    try {
      set f [open $logdir/counter-new.log]
      while {-1 != [gets $f line]} {
        regexp {(.*) -- (.*) ::(.*) (.*)} $line match timestamp server counter value
        #ns_log notice "$counter add_value $timestamp $value"
        if {[::xotcl::Object isobject $counter]} {
          $counter add_value $timestamp $value
        } elseif {![info exists complain($counter)]} {
          ns_log notice "request-monitor: ignore reload of value $value for counter $counter"
          set complain($counter) 1
        }
      }
    } finally {
      close $f
    }
    unset f
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
    ns_log notice "::thottle: exiting"
    dump write -sync true
    #
    # Delete all users objects, that will flush all activity data to
    # the tables if configured
    #
    foreach obj [Users info instances] {$obj destroy}

    ns_log notice "::thottle speficic exist handler finished"
  }

  #ns_log notice "============== Thread initialized ===================="

} -persistent 1 -ad_doc {
  This is a small request-throttle application that handles simple
  DOS-attracks on an AOL-server.  A user (request key) is identified
  via ipAddr or some other key, such as an authenticated userid.
  <p>
  XOTcl Parameters for Class <a
  href='/xotcl/show-object?object=%3a%3athrottle+do+%3a%3aThrottle'>Throttle</a>:
  <ul>
  <li><em>timeWindow:</em>Time window for computing detailed statistics; can
  be configured via OACS package parameter <code>time-window</code></li>
  <li><em>timeoutMs:</em> Time window to keep statistics for a user</li>
  <li><em>startThrottle:</em> If user requests more than this #, thre requests are delayed. When larger than toMuc, the parameter is ignored</li>
  <li><em>toMuch:</em> If user requests more than this #, he is kicked out</li>
  </ul>
  The throttler is defined as a class running in a detached thread. See <a href='/api-doc/procs-file-view?path=packages/xotcl-core/tcl/40-thread-mod-procs.tcl'>XOTcl API for Thread management</a> for more details.
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
  #puts stderr thottle-DESTROY
  ns_log notice thottle-DESTROY-shutdownpending->[ns_info shutdownpending]
  if {[ns_info shutdownpending] && [nsv_exists ::xotcl::THREAD [self]]} {
    set tid [nsv_get ::xotcl::THREAD [self]]
    ns_log notice =========thottle-DESTROY-shutdown==========================$tid-??[::thread::exists $tid]
    if {[::thread::exists $tid]} {
      ns_log notice =========thottle-DESTROY-shutdown==========================THREAD-EXISTS
      set refcount [::thread::release $tid]
      ns_log notice thottle-DESTROY-shutdownpending->[ns_info shutdownpending]-refCount$refcount
    }
  }
  next
}

#
# Use the feature of connection pool unmapping to determine, if we can
# use "ns_conn partialtimes". We can't use the latter directly, since
# this file is typically loaded from a non-connection thread.
#
if {[catch {ns_server unmap "GET /*JUST_FOR_TESTING*"}]} {
  #
  # Older version of NaviServer or AOLserver
  #
  throttle proc partialtimes {} {
    set t [ns_time diff [ns_time get] [ns_conn start]]
    set ms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}]
    return [list ms $ms runtime [expr {$ms/1000.0}] filtertime 0 queuetime 0 accepttime 0]
  }
} else {
  #
  # Use variant based on "ns_conn partialtimes"
  #
  throttle proc partialtimes {} {
    set d [ns_conn partialtimes]
    set t [ns_time diff [ns_time get] [ns_conn start]]
    lappend d ms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}]
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

  set :url [ns_conn url]
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

  set :requestor [::xo::cc requestor]
  set :user      [::xo::cc user]
  set :query     [ad_conn query]
  set :pa        [ad_conn peeraddr]
  if {${:query} ne ""} {
    append :url ?${:query}
  }
  # :log "### setting url to ${:url}"
  #xo::show_stack
  set :context_initialized 1
  # :log "--i leaving [ns_conn url] vars=[lsort [info vars]]"
}

throttle ad_proc check {} {
  This method should be called once per request that is monitored.
  It should be called after authentication such we have already
  the userid if the user is authenticated
} {
  #set t0 [clock milliseconds]

  :get_context
  # :log "### check"

  lassign [my throttle_check ${:requestor} ${:pa} ${:url} \
               [ns_conn start] [ns_guesstype [ns_conn url]] ${:community_id}] \
      toMuch ms repeat
  #set t1 [clock milliseconds]

  if {$repeat} {
    my add_statistics repeat ${:requestor} ${:pa} ${:url} ${:query}
    set result -1
  } elseif {$toMuch} {
    :log "*** we have to refuse user ${:requestor} with $toMuch requests"
    my add_statistics reject ${:requestor} ${:pa} ${:url} ${:query}
    set result $toMuch
  } elseif {$ms} {
    :log "*** we have to block user ${:requestor} for $ms ms"
    my add_statistics throttle ${:requestor} ${:pa} ${:url} ${:query}
    after $ms
    :log "*** continue for user ${:requestor}"
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
# the following procs are forwarder to the monitoring thread
# for convenience
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
throttle forward off                     %self do throttler set off 1
throttle forward on                      %self do throttler set off 0
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
  # :log "+++ [self proc] [ad_conn url] auth ms [:partialtimes] [ad_conn isconnected]"
  # :do set ::cookies(${:requestor}) [ns_set get [ns_conn headers] Cookie]
  set r [:check]
  if {$r < 0} {
    set url ${:url}
    ns_return 200 text/html "
      <h1>[_ xotcl-request-monitor.repeated_operation]</h1>
      [_ xotcl-request-monitor.operation_blocked]<p>"
    return filter_return
  } elseif {$r > 0} {
    ns_return 200 text/html "
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
  # :log "+++ [self proc] <$args> [ad_conn url] [:partialtimes] [ad_conn isconnected]"
  # OpenACS 5.2 bypasses for requests to /resources the user filter
  # in these cases pre- or postauth are not called, but only trace.
  # So we have to make sure we have the needed context here
  :get_context
  # :log "CT=[ns_set array [ns_conn outputheaders]] -- ${:url}"

  my add_url_stat ${:method} ${:url} [:partialtimes] ${:requestor} ${:pa} \
      [ns_set get [ns_conn outputheaders] Content-Type]
  unset :context_initialized
  return filter_ok
}

throttle proc community_access {community_id} {
  :get_context
  if {${:community_id} eq ""} {
    :users community_access ${:requestor} ${:pa} $community_id
  }
}

ad_proc string_truncate_middle {{-ellipsis ...} {-len 100} string} {
  cut middle part of a string in case it is to long
} {
  set string [string trim $string]
  if {[string length $string]>$len} {
    set half  [expr {($len-2)/2}]
    set left  [string trimright [string range $string 0 $half]]
    set right [string trimleft  [string range $string end-$half end]]
    return $left$ellipsis$right
  }
  return $string
}

namespace eval ::xo {
  proc is_ip {key} {
    expr { [string match *.* $key] || [string match *:* $key] }
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
    ::xo::db::require table request_monitor_community_activities {
      user_id      {integer references parties(party_id) on delete cascade}
      peer_address text
      community_id {integer references acs_objects(object_id) on delete cascade}
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
