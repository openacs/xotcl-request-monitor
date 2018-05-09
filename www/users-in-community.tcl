ad_page_contract {
  Displays active users in a community

  @author Gustaf Neumann 

  @cvs-id $id$
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
  lassign $e timestamp requestor
  if {[info exists listed($requestor)]} continue
  set listed($requestor) 1
  if {[string is integer -strict $requestor]} {
    acs_user::get -user_id $requestor -array user
    set user_string "$user(first_names) $user(last_name)"
  } else {
    set user_string $requestor
  }
  set time [clock format $timestamp -format "%H:%M"]
  t1 add -time $time -user $user_string
}

set t1 [t1 asHTML]

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
