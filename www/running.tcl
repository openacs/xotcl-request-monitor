ad_page_contract {
  Displays last requests of a user

  @author Gustaf Neumann (adapted for interaction with controlling thread)
  @cvs-id $Id$
} -query {
  orderby:token,optional
} -properties {
  title:onevalue
  context:onevalue
  user_string:onevalue
}

set admin_p [acs_user::site_wide_admin_p]
if {!$admin_p} {
  ad_return_warning "Insufficient Permissions" \
      "Only side wide admins are allowed to view this page!"
  ad_script_abort
}

set running_requests [throttle running]
if {[info commands bgdelivery] ne "" && [nsv_array names ::xotcl::THREAD ::bgdelivery] ne ""} {
  set background_requests [bgdelivery running]
} else {
  set background_requests [list]
}
set nr_bg  [expr {[llength $background_requests]/2}]
set nr_req [expr {[llength $running_requests]/2}]
set counts $nr_req/$nr_bg
if {[ns_info name] eq "NaviServer"}  {
  set writer_requests [ns_writer list]
  append counts /[llength $writer_requests]
}

set title "Currently Running Requests ($counts)"
set context [list "Running Requests"]

TableWidget create t1 -volatile \
    -actions [subst {
      Action new -label Refresh -url [ad_conn url] -tooltip "Reload current page"
    }] \
    -columns {
      AnchorField user -label "User"
      Field url        -label "Url"
      Field elapsed    -label "Elapsed Time" -html { align right }
      Field background -label "Background"
      Field progress   -label "Progress"
    } \
    -no_data "Currently no running requests"

set sortable_requests [list]
foreach {key elapsed} $running_requests {
  lassign [split $key ,] requester url
  set ms [format %.2f [expr {[throttle ms -start_time $elapsed]/1000.0}]]
  set user_info [xo::request_monitor_user_info $requester]
  set user_string [dict get $user_info label]
  set user_url "last-requests?request_key=$requester"
  lappend sortable_requests [list $user_string $user_url $url $ms ""]
}
foreach {index entry} $background_requests {
  lassign $entry key elapsed
  lassign [split $key ,] requester url
  set ms [format %.2f [expr {[throttle ms -start_time $elapsed]/-1000.0}]]
  set user_info [xo::request_monitor_user_info $requester]
  set user_string [dict get $user_info label]
  set user_url "last-requests?request_key=$requester"
  lappend sortable_requests [list $user_string $user_url $url $ms "::bgdelivery"]
}
if {[ns_info name] eq "NaviServer"}  {
  foreach {entry} $writer_requests {
    if {[llength $entry] == 8} {
      #
      # Versions before rate management.
      #
      lassign $entry starttime thread driver ip fd remaining done clientdata
    } elseif {[llength $entry] == 10} {
      #
      # In some versions of NaviServer, pool can be empty.
      #
      lassign $entry starttime thread driver ip fd remaining done targetRate actualRate clientdata
    } elseif {[llength $entry] == 11} {
      lassign $entry starttime thread driver pool ip fd remaining done targetRate actualRate clientdata
    } else {
      ns_log notice "ns_writer list returns unknown number ([llength $entry]) of elements"
      continue
    }
    lassign $clientdata requester url
    set size [expr {$remaining+$done}]
    set percentage [expr {$done*100.0/$size}]
    set progress [format {%5.2f%% of %5.2f MB} $percentage [expr {$size/1000000.0}]]
    set ms [format %.2f [expr {([clock milliseconds] - $starttime*1000)/-1000.0}]]
    if {[nsf::is integer $requester]} {
      set user_string [person::name -person_id $requester]
    } else {
      set user_string $requester
    }
    set user_url "last-requests?request_key=$requester"
    lappend sortable_requests [list $user_string $user_url $url $ms $thread $progress]
  }
}

foreach r [lsort -decreasing -real -index 3 $sortable_requests] {
  lassign $r user_string user_url url ms mode progress
  if {$ms<0} {set ms [expr {-1*$ms}]}
  t1 add \
      -user $user_string -user.href $user_url \
      -url $url -elapsed $ms -background $mode -progress $progress
}

set t1 [t1 asHTML]

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
