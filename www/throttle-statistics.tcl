ad_page_contract {
  present throttle statistics, active users, etc

  @author Gustaf Neumann
  @cvs-id $Id$
} -properties {
  title:onevalue
  context:onevalue
  throttle_statistics
  throttle_url_statistics
}

set title "Throttle statistics"
set context [list $title]
set throttle_statistics [throttle statistics]
set data [throttle url_statistics]

template::list::create \
    -name url_statistics \
    -elements {
      time {label Time}
      type {label Type}
      user {
        label Userid
        link_url_col user_url}
      IPaddress  {label "IP Address"}
      URL {label "URL"}
    }

multirow create url_statistics type user user_url time IPaddress URL
foreach l [lsort -index 2 $data] {
  lassign $l type uid time IPaddress URL
  if {![string is integer -strict $uid]} {
    set user "Anonymous"
    set user_url ""
  } else {
    set user_info [xo::request_monitor_user_info $uid]
    set user [dict get $user_info label]
    set user_url [dict get $user_info url]
  }
  set time [clock format $time -format "%Y-%m-%d %H:%M:%S"]
  multirow append url_statistics $type $user $user_url $time $IPaddress $URL
}

#set throttle_url_statistics [throttle url_statistics]


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
