ad_page_contract {
    Displays last 100 requests in the system

    @author Gustaf Neumann

    @cvs-id $Id$
} -query {
    {orderby:token,optional "time,desc"}
} -properties {
    title:onevalue
    context:onevalue
}

set title "Last 100 Requests"
set context [list $title]
set admin_p [acs_user::site_wide_admin_p]

set stat [list]
foreach {key value} [throttle last100] {lappend stat $value}

Class create CustomField -volatile \
    -instproc render-data {row} {
      html::div -style {
        border: 1px solid #a1a5a9; padding: 0px 5px 0px 5px; background: #e2e2e2} {
          html::t  [$row set ${:name}]
        }
    }

TableWidget create t1 -volatile \
    -columns {
      Field time       -label "Time" -orderby time -mixin ::template::CustomField
      AnchorField user -label "Userid" -orderby user
      Field ms         -label "Ms" -orderby ms
      AnchorField url  -label "URL" -orderby url
    }

lassign [split $orderby ,] att order
t1 orderby \
    -order [ad_decode $order desc decreasing asc increasing increasing] \
    -type [ad_decode $att ms integer dictionary] \
    $att

foreach l $stat {
  lassign $l timestamp c url ms requester

  set user_info   [xo::request_monitor_user_info $requester]
  set user_string [dict get $user_info label]

  #
  # Provide the URLs only to admins as links.
  #
  # First of all, it is questionable, whether this page should be
  # public. However, when this page is public, and a spider
  # (re)submits a previously broken link, and visits then the last100
  # page, it thinks that the site has still the broken page. This
  # might happen e.g. with Google bot, leading to worse than necessary
  # error rating.
  #
  if {$admin_p} {
    set href "[ad_url]$url"
  } else {
    set href ""
  }
  t1 add -time [clock format $timestamp -format "%H:%M:%S"] \
      -user $user_string \
      -user.href [export_vars -base last-requests {{request_key $requester}}] \
      -ms $ms \
      -url $url \
      -url.href $href
}

set t1 [t1 asHTML]
set last_url [ad_return_url]

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
