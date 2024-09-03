ad_page_contract {
  Displays active users in a community

  @author Gustaf Neumann

  @cvs-id $Id$
} -query {
  community_id:naturalnum
  {community_name:nohtml ""}
} -properties {
  title:onevalue
  context:onevalue
}

set title "Users in Community $community_name"
set context [list $title]
set stat [list]

TableWidget create t1 \
    -columns {
      Field time -label "Last Activity" -html {align center}
      Field user -label User
    }

foreach e [lsort -decreasing -index 0 \
               [throttle users in_community $community_id]] {
  lassign $e timestamp requester
  if {[info exists listed($requester)]} continue
  set listed($requester) 1
  set user_info [xo::request_monitor_user_info $requester]
  set time [clock format $timestamp -format "%H:%M"]
  t1 add -time $time -user [dict get $user_info label]
}

set t1 [t1 asHTML]

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
