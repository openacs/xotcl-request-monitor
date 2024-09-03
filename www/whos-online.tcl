ad_page_contract {
  Displays who's currently online

  @author Gustaf Neumann (adapted for interaction with controlling thread)

  @cvs-id $Id$
} -query {
  {orderby:token,optional "activity,desc"}
  {all:boolean,notnull 0}
} -properties {
  title:onevalue
  context:onevalue
}

#  {orderby:optional "name,asc"}

set title "Who's online?"
set context [list "Who's online"]

# get value from package parameters
set peer_groups [parameter::get -parameter peer-groups \
                         -default {*wlan* *dsl* *.com *.net *.org}]

set admin [acs_user::site_wide_admin_p]
#set admin 0

set label(0) "Authenticated only"
set tooltip(0) "Show authenticated users only"
set label(1) all
set tooltip(1) "Show all users"
set all [expr {!$all}]
set url [export_vars -base [ad_conn url] {request_key all}]

TableWidget create t1 \
    -actions [subst {
      Action new -CSSclass "double-click-prevention btn" -label "$label($all)" -url $url -tooltip "$tooltip($all)"
    }] \
    -columns [subst {
      AnchorField name  -label "User" -orderby name
      Field online_time -label "Last Activity" -html { align right } \
          -orderby online_time
      Field vpm       -label "Views per min" -html { align center } -orderby vpm
      if {$admin} {
        Field activity -label "Activity" -html { align right } -orderby activity
        AnchorField hits -label "Hits" -orderby hits
        Field switches  -label "Switches" -html { align center } -orderby switches
        Field peer_address -label "Peer" -orderby peer_address
      }
    }] \
    -no_data "no registered users online"


foreach cat $peer_groups {set peer_cat_count($cat) 0}
set peer_cat_count(others) 0

# this proc is used only for caching purposes
proc my_hostname pa {
  if {[catch {set peer [ns_hostbyaddr $pa]}]} { return $pa }
  return "$peer ($pa)"
  #return "$peer"
}

#ns_log notice USERS=[throttle users active -full]
set users [list]
foreach element [throttle users active -full] {
  lassign $element user_id pa timestamp hits smooth switches

  set user_info [xo::request_monitor_user_info $user_id]
  set user_label [dict get $user_info label]
  set user_url   [dict get $user_info url]
  if {![nsf::is integer $user_id] && $all} {
    # it was an IP address
    continue
  }
  set timestamp [lindex $smooth 2]
  set last_request_minutes [expr {[clock seconds]/60 - $timestamp}]

  set peer $pa
  if {$admin} {
    catch {set peer [util_memoize [string tolower \
                                       [list ::template::my_hostname $pa]]]}
    set match 0
    foreach cat $peer_groups {
      if {[string match "$cat *" $peer]} {
        incr peer_cat_count($cat)
        set match 1
        break
      }
      }
    if {!$match} {
      incr peer_cat_count(others)
      append peer " ???"
    }
  }
  set loadparam "1m=[lindex $smooth 3], 10m=$hits"
  set detail_url "last-requests?request_key=$user_id"

  lappend users [list $user_label \
                     $user_url \
                     $last_request_minutes "$last_request_minutes minutes ago" \
                     [format %.2f [lindex $smooth 0]] \
                     $hits $loadparam $detail_url \
                     $switches \
                     $peer \
                     $user_id \
                     [throttle views_per_minute $user_id] \
                    ]
}

lassign [split $orderby ,] att order
set order [ad_decode $order desc decreasing asc increasing increasing]
set type [ad_decode $att activity integer switches integer vpm real dictionary]

ns_log notice "ORDERBY $orderby -> [list t1 orderby -order $order -type $type $att]"
t1 orderby -order $order -type $type $att

if {$admin} {
  set total $peer_cat_count(others)
  foreach cat $peer_groups {incr total $peer_cat_count($cat)}
  if {$total > 0} {
    set summarize_categories "$total users logged in from: "
    foreach cat $peer_groups {
      append summarize_categories "$cat [format %.2f [expr {$peer_cat_count($cat)*100.0/$total}]]%, "
    }
    append summarize_categories "others [format %.2f [expr {$peer_cat_count(others)*100.0/$total}]]%. "
  } else {
    set summarize_categories "$total users logged in"
  }
} else {
  set summarize_categories ""
}

foreach e $users {
  if {$admin} {
    t1 add      -name         [lindex $e 0] \
                -name.href    [lindex $e 1] \
                -online_time  [lindex $e 3] \
                -activity     [lindex $e 4] \
                -hits         [lindex $e 6] \
                -hits.href    [lindex $e 7] \
                -switches     [lindex $e 8] \
                -vpm          [format %.2f [lindex $e 11]] \
                -peer_address [lindex $e 9]
  } else {
    t1 add      -name         [lindex $e 0] \
                -name.href    [lindex $e 1] \
                -online_time  [lindex $e 3] \
                -vpm          [format %.2f [lindex $e 11]]
  }
}

set t1 [t1 asHTML]

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
