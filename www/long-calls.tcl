ad_page_contract {
    Displays last n lines of long-calls log

    @author Gustaf Neumann

    @cvs-id $Id$
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
        set user_info [xo::request_monitor_user_info $uid]
        set user_url [dict get $user_info url]
        set userinfo "<a href='[ns_quotehtml $user_url]'>$uid</a>"
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

nsf::proc ::xo::colorize_slow_calls {-fast:required -warning:required -danger:required value} {
    if {$value > $danger} {
        return danger
    } elseif {$value > $warning} {
        return warning
    } elseif {$value > $fast} {
        return info
    } else {
        return success
    }
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
    if {[llength $time] > 1} {
        set queuetime  [dict get $time queuetime]
        set filtertime [dict get $time filtertime]
        set runtime    [dict get $time runtime]
        set totaltime  [format %8.6f [expr {$queuetime + $filtertime + $runtime}]]
        set color(queuetime)  [::xo::colorize_slow_calls -fast 0.001 -warning 0.50 -danger 1.00 $queuetime]
        set color(filtertime) [::xo::colorize_slow_calls -fast 0.010 -warning 1.00 -danger 2.00 $filtertime]
        set color(runtime)    [::xo::colorize_slow_calls -fast 0.010 -warning 5.00 -danger 10.00 $runtime]
        set color(totaltime)  [::xo::colorize_slow_calls -fast 0.010 -warning 5.00 -danger 10.00 $totaltime]
    } else {
        lassign {"" "" ""} queuetime filtertime runtime
        lassign {"" "" ""} color(queuetime) color(filtertime) color(runtime)
        set totaltime $time
        set color(totaltime)  [::xo::colorize_slow_calls -fast 0.010 -warning 3.00 -danger 10.00 $totaltime]
    }
    if {$time < 6000} {
        set class info
    } elseif {$time < 10000} {
        set class warning
    } else {
        set class danger
    }
    set request [ns_quotehtml $url]
    set request [::xo::regsub_eval {user_id=([0-9]+)} $request {::xo::subst_user_link user_id= \1} user_id=]
    append rows "<tr class=''>" \
        "<td class='text-right $color(queuetime)'><span class='info'>$queuetime</span></td>" \
        "<td class='text-right $color(filtertime)'>$filtertime</td>" \
        "<td class='text-right $color(runtime)'>$runtime</td>" \
        "<td class='text-right $color(totaltime)'><strong>$totaltime</strong></td>" \
        "<td class='$color(totaltime)'>$year&nbsp;$mon&nbsp;$day&nbsp;$hours</td>" \
        "<td class='text-right  $color(totaltime)'>$userinfo</td>" \
        "<td class='$color(totaltime)'>$iplink</td>" \
        "<td class='$color(totaltime)'>$request</td></tr>\n"
}

set doc(title) "Long Calls"
set context [list $doc(title)]

if {[template::head::can_resolve_urn urn:ad:js:bootstrap3]} {
    template::head::add_css -href urn:ad:js:bootstrap3 -media all
} else {
    template::head::add_css -href //maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css -media all
}
template::head::add_css -href //maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap-theme.min.css -media all
security::csp::require style-src maxcdn.bootstrapcdn.com

# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
