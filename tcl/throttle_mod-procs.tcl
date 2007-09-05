#############################################################################
::xotcl::THREAD create throttle {

  Class create ThrottleStat -parameter { type requestor timestamp ip_adress url }

  Class create Throttle -parameter {
    {timeWindow 10}
    {timeoutMs 2000}
    {startThrottle 7} 
    {toMuch 10} 
    {alerts 0} {throttles 0} {rejects 0} {repeats 0}
  }

  Throttle instproc init {} {
    my set off 0
    Object create [self]::stats
    Object create [self]::users
    next
  }

  Throttle instproc add_statistics { type requestor ip_adress url query } {
    set furl [expr {$query ne "" ? "$url?$query" : $url}]
    my incr ${type}s
    #my log "++++ add_statistics   -type $type -user_id $requestor "
    set entry [ThrottleStat new -childof [self]::stats \
		   -type $type -requestor $requestor \
		   -timestamp [clock seconds] \
		   -ip_adress $ip_adress -url $furl]
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
		    [$stat timestamp] [$stat ip_adress] [$stat url]]
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

  Throttle instproc register_access {requestKey pa url community_id} {
    set obj [Users current_object]
    $obj addKey $requestKey $pa $url $community_id
    Users expSmooth [$obj point_in_time] $requestKey
  }


  Throttle instproc running {} {
    my array get running_url
  }

  Throttle instproc throttle_check {requestKey pa url conn_time content_type community_id} {
    my instvar off
    seconds ++
      
    set var running_url($requestKey,$url)
    # check first, whether the same user has already the same request
    # issued; if yes, block this request. Caveat: some html-pages
    # use the same image in many places, so we can't block it. This
    # will make the sttistics for images look better than they are.
    set is_image_request [string match "image/*" $content_type]
    if {[my exists $var] && !$is_image_request && !$off} {
      #my log "### already $var"
      return [list 0 0 1]
    } else {
      my set $var $conn_time
      #my log "### new $var"
    }
    my register_access $requestKey $pa $url $community_id

    # ... the number of 14 is arbitrary. one of our single real request
    # might have up to 14 subrequests (through iframes).... 
    if {$off || $is_image_request || [my array size running_url] < 14} {

      # less than forteen running requests, let people do what they want;
      # don't throttle/block embedded images.....
      return [list 0 0 0]

    } else {

      # Check, whether the last request from a user was within 
      # the minimum time interval. We are not keeping a full table
      # of all request keys, but use an timeout triggered mechanism 
      # to keep only the active request keys in an associative array

      my incr alerts
      if {[my exists active($requestKey)]} {
	# if more than one request for this key is already active, 
	# return blocking time
	foreach {to cnt} [my set active($requestKey)] break
	set retMs [expr {$cnt>[my startThrottle] ? 500 : 0}]
	# cancel the timeout
	after cancel $to
      } else {
	set retMs 0
	set cnt 0
      }
      incr cnt
      # establish a new timeout
      set to [after [my timeoutMs] [list [self] cancel $requestKey]]
      my set active($requestKey) [list $to $cnt]
      if {$cnt <= [my toMuch]} {
	set cnt 0
      }
      return [list $cnt $retMs 0]
    }
  }

  Throttle instproc statistics {} {
    return "<table>
        <tr><td>Number of alerts:</td><td>[my alerts]</td></tr>
        <tr><td>Number of throttles:</td><td>[my throttles]</td></tr>
        <tr><td>Number of rejects:</td><td>[my rejects]</td></tr>
        <tr><td>Number of repeats:</td><td>[my repeats]</td></tr>
        </table>\n"
  }

  Throttle instproc cancel {requestKey} {
    # cancel a timeout and clean up active request table for this key
    if {[my exists active($requestKey)]} {
      after cancel [lindex [my set active($requestKey)] 0]
      my unset active($requestKey)
      #my log "+++ Cancel $requestKey block"
    } else {
      my log "+++ Cancel for $requestKey failed !!!"
    }
  }

  Throttle instproc active { } {
    # return the currently active requests (for debugging and introspection)
    return [my array get active]
  }

  Throttle instproc add_url_stat {url time_used key pa} {
    catch {my unset running_url($key,$url)}
    #my log "### unset running_url($key,$url) $errmsg"
    response_time_minutes add_url_stat $url $time_used $key
  }
  Throttle instforward report_url_stats response_time_minutes %proc
  Throttle instforward flush_url_stats  response_time_minutes %proc
  Throttle instforward last100          response_time_minutes %proc
  Throttle create throttler

  ############################
  # A simple counter class, which is able to aggregate values in some
  # higher level counters (report_to) and to keep statistics in form 
  # of a trend and max values)
  Class create Counter -parameter { 
    report timeoutMs 
    {stats ""} {last ""} {trend ""} {c 0} {logging 0}
    {nr_trend_elements 48} {nr_stats_elements 5} 
  }
  Counter instproc ++ {} {
    my incr c
  }
  Counter instproc end {} {
    if {[my exists report]} {
      [my report] incr c [my c]
    }
    my finalize [my c]
    my c 0
  }
  Counter instproc log_to_file {timestamp label value} {
    if {![my logging]} return
    set server [ns_info server]
    set f [open $::logdir/counter.log a]
    puts $f "$timestamp -- $server $label $value"
    close $f
  } 
  Counter instproc finalize {n} {
    after cancel [my set to]
    my instvar stats trend
    #
    # trend keeps nr_trend_elements most recent values
    #
    lappend trend $n
    set lt [llength $trend]
    if {$lt > [my nr_trend_elements]} {
      set trend [lrange $trend [expr {$lt-[my nr_trend_elements]}] end]
    }
    #
    # stats keeps nr_stats_elements highest values with time stamp
    #
    set now [clock format [clock seconds]]
    lappend stats [list $now $n]
    set stats [lrange [lsort -real -decreasing -index 1 $stats] 0 [my nr_stats_elements]]
    #
    # log if necessary
    #
    catch {my log_to_file $now [self] $n}
    #
    my set to [after [my timeoutMs] [list [self] end]]
  }

  Counter instproc init {} {
    my set to [after [my timeoutMs] [list [self] end]]
    next
  }
  Counter instproc destroy {} {
    after cancel [my set to]
    next
  }

  Counter hours -timeoutMs [expr {60000*60}] -logging 1
  Counter minutes -timeoutMs 60000 -report hours -logging 1
  Counter seconds -timeoutMs 1000 -report minutes 

  Class create MaxCounter -superclass Counter -instproc end {} {
    my c [Users nr_active]
    if {[my exists report]} {
      [my report] instvar {c rc}
      if {$rc < [my c]} {set rc [my c]}
    }
    my finalize [my c]
    my c 0
  }

  MaxCounter user_count_hours -timeoutMs [expr {60000*60}] -logging 1
  MaxCounter user_count_minutes -timeoutMs 60000 -report user_count_hours -logging 1


  Class create AvgCounter -superclass Counter \
      -parameter {{t 0} {atleast 1}} -instproc end {} {
    if {[my c]>0} {
      set avg [expr {int([my t]*1.0/[my c])}]
    } else {
      set avg 0
    }
    if {[my exists report]} {
      [my report] incr c [my c]
      [my report] incr t [my t]
    }
    my finalize $avg
    my c 0
    my t 0
  }

  Class create UrlCounter -superclass AvgCounter \
      -parameter {{truncate_check 10} {max_urls 0}} \
      -set seconds [clock seconds] \
      -instproc add_url_stat {url ms requestor} {
    my instvar c
    my ++
    # my log "[self proc] $url /$ms/ $requestor ($c)"
    my incr t $ms

    ### set up a value for the right ordering in last 100.
    ### We take the difference in seconds since start, multiply by
    ### 10000 (there should be no overflow); there should be less
    ### than this number requests per minute.
    set now [clock seconds]
    set order [expr {($now-[[self class] set seconds])*10000+$c}]
    my set last100([expr {$order%99}]) [list $now $order $url $ms $requestor]

    ### Add statistics in detail
    if {[my exists stat($url)]} {
      my incr stat($url) $ms
      my incr cnt($url)
    } else {
      my set stat($url) $ms
      my set cnt($url) 1
    }
  } -instproc last100  {} {
    my array get last100
  } -instproc flush_url_stats {} {
    my log "flush_url_stats"
    my array unset stat
    my array unset cnt
  } -instproc url_stats {} {
    set result [list]
    foreach url [my array names stat] {
      lappend result [list $url [my set stat($url)] [my set cnt($url)]]
    }
    set result [lsort -real -decreasing -index 1 $result]
    return $result
  } -instproc check_truncate_stats {} {
    # truncate statistics if necessary
    if {[info exists ::package_id]} {
      # get values from package parameters
      my max_urls [parameter::get -package_id $::package_id \
		       -parameter max-url-stats -default 13]
      # we use the timer to check other parameters as well here
      set time_window [parameter::get -package_id $::package_id \
			   -parameter time-window -default 10]
      if {$time_window != [throttler timeWindow]} {
	throttler timeWindow $time_window
	after 0 [list Users purge_access_stats]
      }
    }
    set max [my max_urls]
    if {$max>1} {
      set result [my url_stats]
      set l [llength $result]
      for {set i $max} {$i<$l} {incr i} {
	set url [lindex [lindex $result $i] 0]
	my unset stat($url)
	my unset cnt($url)
      }
      set result [lrange $result 0 [expr {$max-1}]]
      return $result
    }
    return ""
  } -instproc report_url_stats {} {
    set stats [my check_truncate_stats]
    if {$stats eq ""} {
      set stats [my url_stats]
    }
    return $stats
  } -instproc finalize args {
    next
    # each time the timer runs out, perform the cleanup
    after 0 [list [self] check_truncate_stats]
  }


  UrlCounter response_time_hours -timeoutMs [expr {60000*60}] \
     -atleast 500 -logging 1
  UrlCounter response_time_minutes -timeoutMs 60000 \
     -report response_time_hours -atleast 100 \
     -logging 1

  #
  # Class for the user tracking

  Class create Users -parameter { point_in_time } -ad_doc {
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
      foreach key [my array names pa] {
	set entry [list $key [my set pa($key)]]
	foreach var [list timestamp hits expSmooth switches] {
	  set k ${var}($key)
	  lappend entry [expr {[my exists $k] ? [my set $k] : 0}]
	}
	lappend info $entry
      }
      return $info
    } else {
      return [my array names pa]
    }
  }
  Users proc unknown { obj args } {
    my log "unknown called with $obj $args"
  }
  Users ad_proc nr_active {} {
    @return number of active users (in time window)
  } {
    return [my array size pa]
  }

  Users ad_proc nr_users_time_window {} {
    @return number of different ip addresses and authenticated users (in time window)
  } {
    set ip 0; set auth 0
    foreach i [my array names pa] {
      if {[string match *.* $i]} {incr ip} {incr auth}
    }
    return [list $ip $auth]
  }


  Users ad_proc hits {uid} {
    @param uid request key
    @return Number of hits by this user (in time window)
  } {
    if {[my exists hits($uid)]} {return [my set hits($uid)]} else {return 0}
  }
  Users ad_proc last_pa {uid} {
    @param uid request key
    @return last peer address of the specified users
  } {
    if {[my exists pa($uid)]} { return [my set pa($uid)]} else { return "" }
  }
  Users proc last_click {uid} {
    if {[my exists timestamp($uid)]} {return [my set timestamp($uid)]} else {return 0}
  }
  Users proc last_requests {uid} {
    if {[my exists pa($uid)]} { 
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
    throttler instvar timeWindow
    my instvar last_mkey
    set now 	[clock seconds]
    set mkey 	[expr { ($now / 60) % $timeWindow}]
    set obj 	[self]::users::$mkey

    if {$mkey ne $last_mkey} { 
      if {$last_mkey ne ""} {my purge_access_stats}
      # create or recreate the container object for that minute
      if {[my isobject $obj]} {$obj destroy}
      Users create $obj -point_in_time $now
      my set last_mkey $mkey
    }
    return $obj
  }
  Users proc purge_access_stats {} {
    throttler instvar timeWindow
    my instvar last_mkey
    set time [clock seconds]
    # purge stale entries (for low traffic)
    set secs [expr {$timeWindow*60}]
    if { $time - [[self]::users::$last_mkey point_in_time] > $secs } {
      # no requests for a while; delete all objects under [self]::users::
      Object create [self]::users
    } else {
      # delete selectively
      foreach element [[self]::users info children] {
        if { [$element point_in_time] < $time - $secs } {$element destroy}
      }
    } 
  }

  Users proc community_access {requestor community_id} {
    [my current_object] community_access $requestor $community_id
  }

  Users instproc community_access {key community_id} {
    if {![my exists user_in_community($key,$community_id)]} {
      my set user_in_community($key,$community_id) 1
      my lappend in_community($community_id) $key
    } 
  }

  Users instproc addKey {key pa url community_id} {
    set class [self class]
    if {[$class exists pa($key)]} {
      # check, if the peer address changed
      if {[$class set pa($key)] ne $pa} {
	if {[catch {$class incr switches($key)}]} {
	  $class set switches($key) 1
	}
	# log the change
	set timestamp [clock format [clock seconds]]
	set f [open $::logdir/switches.log a]
	puts $f "$timestamp -- switch $key from\
 		[$class set pa($key)] to $pa $url"
	close $f
      }
    }
    set counter active($key)
    if {[catch {my incr $counter}]} {
      my set $counter 1
    }
    if {[my set $counter] == 1} {
      # we could combine this test with the incr, but in
      # Tcl 8.5, incr on unknown variables does not throw errors
      $class incrRefCount $key $pa
    }

    my community_access $key $community_id
    set entry [list [my point_in_time] $url $pa]
    if {[catch {my lappend urls($key) $entry}]} {
      my set urls($key) [list $entry]
    }
    if {[catch {$class incr hits($key)}]} {
      $class set hits($key) 1
    }
  }

  Users instproc destroy {} {
    foreach key [my array names active] {
      [self class] decrRefCount $key [my set active($key)]
    }
    next
  }
  Users proc expSmooth {ts key} {
    set mins [expr {$ts/60}]
    if {[my exists expSmooth($key)]} {
      foreach {_ aggval lastmins hits} [my set expSmooth($key)] break
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
    my set expSmooth($key) [list $retval $aggval $mins $hits]
    return $retval
  }
  Users proc incrRefCount {key pa} {
    if {[my exists refcount($key)]} {
      my incr refcount($key)
    } else {
      my set refcount($key) 1
    }
    my set pa($key) $pa
    my set timestamp($key) [clock seconds]
  }
  Users proc decrRefCount {key hitcount} {
    if {[my exists refcount($key)]} {
      set x [my incr refcount($key) -1]
      my incr hits($key) -$hitcount
      if {$x < 1} {
	my unset refcount($key)
	my unset pa($key)
        catch {my unset expSmooth($key)}
        catch {my unset switches($key)}
      }
    } else {
      my log "+++ cannot decrement refcount for '$key' by $hitcount"
    }
  }
  Users proc perDay {} {
    set ip 0; set auth 0
    foreach i [my array names timestamp] {
      if {[string match *.* $i]} {incr ip} {incr auth}
    }
    return [list $ip $auth]
  }
  
  Users proc perDayCleanup {} {
    set secsPerDay [expr {3600*24}]
    foreach i [lsort [my array names timestamp]] {
      set secs [expr {[clock seconds]-[my set timestamp($i)]}]
      # my log "--- $i: last click $secs secs ago"
      if {$secs>$secsPerDay} {
	foreach {d h m s} [clock format [expr {$secs-$secsPerDay}] \
			       -format {%j %H %M %S}] break
	regexp {^[0]+(.*)$} $d match d
	regexp {^[0]+(.*)$} $h match h
	incr d -1
	incr h -1
	my log "--- $i expired $d days $h hours $m minutes ago"
	my unset timestamp($i)
      }
    }
    after [expr {60000*60}] [list [self] [self proc]]
  }
  
  # initialization of Users class object
  Users perDayCleanup
  Object create Users::users
  Users set last_mkey ""
 
  # for debugging purposes: return all running timers
  proc showTimers {} {
     set _ ""
     foreach t [after info] { append _ "$t [after info $t]\n" }
     return $_
  }


  if {[catch {set ::package_id [::xo::package_id_from_package_key xotcl-request-monitor]}]} {
    # old style
    set ::package_id [::Generic::package_id_from_package_key xotcl-request-monitor]
  }
  ns_log notice "+++ package_id of xotcl-request-monitor is $::package_id"

  set logdir [parameter::get -package_id $::package_id \
		  -parameter log-dir \
		  -default [file dirname [file root [ns_config ns/parameters ServerLog]]]]
  if {![file isdirectory $logdir]} {file mkdir $logdir}

} -persistent 1 -ad_doc {
  This is a small request-throttle application that handles simple 
  DOS-attracks on an AOL-server.  A user (request key) is identified 
  via ipAddr or some other key, such as an authenticated userid.
  <p>
  XOTcl Parameters for Class <a 
  href='/xotcl/show-object?object=%3a%3athrottle+do+%3a%3aThrottle'>Throttle</a>:
  <ul>
  <li><em>timeWindow:</em>Time window for computing detailed statistics; can 
     be configured via OACS parameter <code>time-window</code></li>
  <li><em>timeoutMs:</em> Time window to keep statistics for a user</li>
  <li><em>startThrottle:</em> If user requests more than this #, he is throttled</li>
  <li><em>toMuch:</em> If user requests more than this #, he is kicked out</li>
  </ul>
  The throttler is defined as a class running in a detached thread.
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

throttle proc ms {-start_time} {
  if {![info exists start_time]} {
    set start_time [ns_conn start]
  }
  set t [ns_time diff [ns_time get] $start_time]
  #my log "+++ $t [ns_conn url]"
  set ms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}]
  return $ms
}

throttle proc get_context {} {
  my instvar url query requestor user pa
  #my log "--t [my exists context_initialized] url=[ns_conn url]"
  if {[my exists context_initialized]} return

  set url [ns_conn url]

  my set community_id 0
  if {[info exists ::ad_conn(user_id)]} {
    #my log "--t we have a user_id"
    # ordinary request, ad_conn is initialized
    set package_id [ad_conn package_id]
    ::xo::ConnectionContext require -package_id $package_id -url $url
    if {[info command dotlrn_community::get_community_id] ne "" &&
      $package_id ne ""} {
      my set community_id [dotlrn_community::get_community_id \
			       -package_id $package_id]
    }
  } else {
    #my log "--t we have no user_id and cannot use ad_conn package_id" 
    ::xo::ConnectionContext require -url $url
  }

  set requestor [::xo::cc requestor]
  set user      [::xo::cc user]
  set query     [ad_conn query]
  set pa        [ad_conn peeraddr]
  if {$query ne ""} {
     append url ?$query
  }
  #my log "### setting url to $url"
  #xo::show_stack
  my set context_initialized 1
  #my log "--i leaving [ns_conn url] vars=[lsort [info vars]]"
}


throttle ad_proc check {} {
  This method should be called once per request that is monitored.
  It should be called after authentication shuch we have already 
  the userid if the user is authenticated
} {
  my instvar url requestor user pa query community_id
  my get_context
  #my log "### check"

  foreach {toMuch ms repeat} \
      [my throttle_check $requestor $pa $url \
	   [ns_conn start] [ns_guesstype $url] $community_id] \
      break
  if {$repeat} {
    my add_statistics repeat $requestor $pa $url $query
    return -1
  } elseif {$toMuch} {
    my log "*** we have to refuse user $requestor with $toMuch requests"
    my add_statistics reject $requestor $pa $url $query
    return $toMuch
  } elseif {$ms} {
    my log "*** we have to block user $requestor for $ms ms"
    my add_statistics throttle $requestor $pa $url $query
    after $ms
    my log "*** continue for user $requestor"
  }
  return 0
}
####
# the following procs are forwarder to the monitoring thread
# for conveniance
####
throttle forward statistics              %self do throttler %proc
throttle forward url_statistics          %self do throttler %proc
throttle forward add_url_stat            %self do throttler %proc
throttle forward flush_url_stats         %self do throttler %proc
throttle forward report_url_stats        %self do throttler %proc
throttle forward add_statistics          %self do throttler %proc
throttle forward throttle_check          %self do throttler %proc
throttle forward last100                 %self do throttler %proc
throttle forward off                     %self do throttler set off 1
throttle forward on                      %self do throttler set off 0
throttle forward running                 %self do throttler %proc
throttle forward nr_running              %self do throttler array size running_url
throttle forward trend                   %self do %1 set trend
throttle forward max_values              %self do %1 set stats
throttle forward purge_access_stats      %self do Users %proc
throttle forward users                   %self do Users

####
# the next procs are for the filters (registered from the -init file)
####
throttle proc postauth args {
  #my log "+++ [self proc] [ad_conn url] auth ms [my ms] [ad_conn isconnected]"
  set r [my check]
  if {$r<0} {
    ns_return 200 text/html "
      <H1>Repeated Operation</H1>
      Operation blocked.
      Don't submit the same query, while the old one is still
      running...<p>"
    return filter_return
  } elseif {$r>0} {
    ns_return 200 text/html "
      <H1>Invalid Operation</H1>
      This web server is only open for interactive usage.<br>
      Automated copying and mirroring is not allowed!<p>
      Please slow down your requests...<p>"
    return filter_return
  } else {
    #my log "-- filter_ok"
    return filter_ok
  }
}
throttle proc trace args {
  #my log "+++ [self proc] <$args> [ad_conn url] [my ms] [ad_conn isconnected]"
  # openacs 5.2 bypasses for requests to /resources the user filter
  # in these cases pre- or postauth are not called, but only trace.
  # So we have to make sure we have the needed context here
  my get_context
  my add_url_stat [my set url] [my ms] [my set requestor] [my set pa]
  my unset context_initialized
  return filter_ok
}

throttle proc community_access {community_id} {
  my get_context
  if {[my set community_id] eq ""} {
    my users community_access [my set requestor] $community_id
  }
}
#throttle proc {} args {my eval $args}


ad_proc string_truncate_middle {{-ellipsis ...} {-len 100} string} {
  cut middle part of a string in case it is to long
} {
  set string [string trim $string]
  if {[string length $string]>$len} {
    set half [expr {($len-2)/2}]
    set left  [string trimright [string range $string 0 $half]]
    set right [string trimleft [string range $string end-$half end]]
    return $left$ellipsis$right
  }
  return $string
}