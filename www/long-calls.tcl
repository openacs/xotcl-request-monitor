ad_page_contract {
  Displays last n lines of long-calls log

    @author Gustaf Neumann 

    @cvs-id $id$
} -query {
    {lines:naturalnum 20}
    {readsize:naturalnum 100000}
} -properties {
    title:onevalue
    context:onevalue
}

proc ::xo::userid_link {uid} {
  if {![string is integer -strict $uid]} {
    set userinfo 0
  } else {
    set user_url [acs_community_member_admin_url -user_id $uid]
    set userinfo "<a href='$user_url'>$uid</a>"
  }
  return $userinfo
}
proc ::xo::regsub_eval {re string cmd {prefix ""}} {
    set map { \" \\\" \[ \\[ \] \\] \$ \\$ \\ \\\\}
    return [uplevel [list subst [regsub -all $re [string map $map $string] "\[$cmd\]"]]]
}
proc ::xo::subst_user_link {prefix uid} {
   return $prefix[::xo::userid_link $uid]
}

set long_calls_file [file dirname [ns_info log]]/long-calls.log
set filesize [file size $long_calls_file]

set F [open $long_calls_file]
if {$readsize < $filesize} {
  seek $F -$readsize end
}
set c [read $F]; close $F

set offsets [regexp -indices -all -inline \n $c]
set o [lindex $offsets end-$lines]
set c1 [string range $c [lindex $o 0]+1 end]
set rows ""
foreach line [lreverse [split $c1 \n]] {
  if {$line eq ""} continue
  lassign $line wday mon day hours tz year dash url time uid ip fmt
  set userinfo [::xo::userid_link $uid]
  set iplink [subst {<a href="[export_vars -base ip-info {ip}]">[ns_quotehtml $ip]</a>}]
    if {$time < 6000} {
	set class info
    } elseif {$time < 10000} {
	set class warning
    } else {
	set class danger
    }
    set request [ns_quotehtml $url]
    set request [::xo::regsub_eval {user_id=([0-9]+)} $request {::xo::subst_user_link user_id= \1} user_id=]
    append rows "<tr class=$class><td class='text-right'><strong>$time</strong></td><td>$year&nbsp;$mon&nbsp;$day&nbsp;$hours</td><td class='text-right'>$userinfo</td><td>$iplink</td><td>$request</td></tr>\n"
}

set doc(title) "Long Calls"
set context [list $doc(title)]

template::head::add_css -href //maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css -media all
template::head::add_css -href //maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap-theme.min.css -media all

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
