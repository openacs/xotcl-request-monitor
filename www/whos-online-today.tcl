ad_page_contract {
  Displays who was online today

  @author Gustaf Neumann

  @cvs-id $Id$
} -query {
  {orderby:token,optional "date,desc"}
  {all:boolean,optional 0}
} -properties {
  title:onevalue
  context:onevalue
}

set title "Who was online today?"
set context [list "Who was online today"]

set admin [acs_user::site_wide_admin_p]
#set admin 0

set label(0) "Authenticated only"
set tooltip(0) "Show authenticated users only"
set label(1) all
set tooltip(1) "Show all users"
set all [expr {!$all}]
set url [export_vars -base [ad_conn url] {all}]

TableWidget create t1 \
    -actions [subst {
      Action new -label "$label($all)" -url $url -tooltip "$tooltip($all)"
    }] \
    -columns [subst {
      AnchorField name  -label "User" -orderby name
      HiddenField date
      Field last -label "Last Activity" -html { align right } \
          -orderby date
      }] \
    -no_data "no registered online today"


set users [list]
lassign [throttle users users_per_day] ip auth
if {!$all} {
  set elements [concat $ip $auth]
} {
  set elements $auth
}
set summary "We noticed in [expr {[llength $ip]+[llength $auth]}] users in total, containing [llength $auth] authenticated users"

set now_ansi  [lc_clock_to_ansi [clock seconds]]

foreach element $elements {
  lassign $element user_id timestamp
  set user_info [xo::request_monitor_user_info $user_id]
  lappend users [list [dict get $user_info label] \
                     [dict get $user_info url] \
                     $timestamp \
                     [::xowiki::utility pretty_age \
                          -timestamp [clock scan [lc_clock_to_ansi $timestamp]]] \
                    ]
}


foreach e $users {
  if {$admin} {
    t1 add  -name   [lindex $e 0] \
        -name.href  [lindex $e 1] \
        -date       [lindex $e 2] \
        -last       [lindex $e 3] \
      }
}

lassign [split $orderby ,] att order
t1 orderby \
    -order [ad_decode $order desc decreasing asc increasing increasing] \
    -type [ad_decode $att date integer dictionary] \
    $att

set t1 [t1 asHTML]

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
