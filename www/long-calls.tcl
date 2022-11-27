ad_page_contract {
    Displays last n lines of long-calls log

    @author Gustaf Neumann

    @cvs-id $Id$
} -query {
    {lines:naturalnum 20}
    {readsize:naturalnum 100000}
    {pool:word,multiple ""}
    {by_starttime:boolean 0}
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
    return [uplevel [list subst [regsub -all -- $re [string map $map $string] "\[$cmd\]"]]]
}
proc ::xo::subst_user_link {prefix uid} {
    return $prefix[::xo::userid_link $uid]
}

nsf::proc ::xo::colorize_slow_calls {-fast:required -warning:required -danger:required value} {
    if {$value > $danger} {
        return "danger bg-danger bg-opacity-10"
    } elseif {$value > $warning} {
        return "warning bg-warning bg-opacity-10"
    } elseif {$value > $fast} {
        return "info bg-info bg-opacity-10"
    } else {
        return "success bg-success bg-opacity-10"
    }
}

set long_calls_file [file dirname [ns_info log]]/long-calls.log
set filesize [ad_file size $long_calls_file]

set F [open $long_calls_file]
if {$readsize < $filesize} {
    seek $F -$readsize end
}
set c [read $F]; close $F

set offsets [regexp -indices -all -inline \n $c]
set offset [lindex $offsets end-$lines 0]
if {$offset eq ""} {
    #
    # Trim potential partial lines
    #
    set offset [lindex $offsets 0 0]
}
set c1 [string range $c $offset+1 end]
set logLines [lreverse [split $c1 \n]]

#
# Determine the pools which where used in line range of the log lines,
# that we are looking at?
#
set foundPoolsDict ""
foreach line $logLines {
    if {$line eq ""} continue
    dict set foundPoolsDict [lindex $line 12] 1
}
#
# Remember pool settings for the number-of-lines filter
#
set filterQuery &[export_vars {pool:multiple lines by_starttime}]
ns_log notice "filterQuery = '$filterQuery'"

set toggle_request_start [expr {!$by_starttime}]
set toggle_request_start_url [export_vars -base long-calls {pool:multiple lines {by_starttime $toggle_request_start}}]
set toggle_request_time_title [expr {$by_starttime ? "Click to order by endtime" : "Click to order by starttime"}]

#
# Map in the found pools empty to "default"
#
set foundPools [lmap p [lsort [dict keys $foundPoolsDict]] {
    expr {$p eq "" ? "default" : $p}
}]

#
# In case, no "pool" filter value was provided, show all found pools.
#
if {$pool eq ""} {
    set pool $foundPools
    set filterQuery ""
}
set inputPools $pool

#
# Create a multirow to let templating make some work
#
template::multirow create  poolcheckboxes name checked
foreach name $foundPools {
    template::multirow append poolcheckboxes $name [expr {$name in $inputPools ? "checked" : ""}]
}

#
# Provide the reverse mapping for "default" to "" avoid doing the test
# in the loop.
#
set pools  [lmap p $inputPools {expr {$p eq "default" ? "" : $p}}]
set now [clock seconds]

template::head::add_style -style {
    .daydiff {
        font-size: 6pt;
        vertical-align: super;
    }
}

set rows ""
foreach line $logLines {
    if {$line eq ""} continue
    lassign $line wday mon day hours tz year dash url time uid ip contentType pool
    if {$pool ni $pools} {
        continue
    }
    set userinfo [::xo::userid_link $uid]
    set iplink [subst {<a href="[export_vars -base ip-info {ip}]">[ns_quotehtml $ip]</a>}]
    if {[llength $time] > 1} {
        set queuetime  [dict get $time queuetime]
        set filtertime [dict get $time filtertime]
        set runtime    [dict get $time runtime]
        set totaltime  [format %8.6f [expr {$queuetime + $filtertime + $runtime}]]

        if {[dict exists $time start]} {
            #
            # We have the precise start time.
            #
            set s0 [dict get $time start]
            set timestamp_start [ns_time format $s0]
        } else {
            #
            # We have only the end time precise to the second (legacy
            # data). Also in these cases, compute the approximate
            # start time by subtracting from the end time the total
            # runtime.
            #
            #set start_old "$hours.000000"
            set timestamp_end [clock scan "$year $mon $day $hours" -format "%Y %b %d %H:%M:%S"].000001
            set timestamp_start [expr {$timestamp_end-($queuetime + $filtertime + $runtime)}]
        }
        set start_secs [ns_time seconds $timestamp_start]
        set start_msecs [string range 000000[ns_time microseconds $timestamp_start] end-5 end]
        set start [clock format $start_secs -format "%H:%M:%S"].$start_msecs
        #ns_log notice "start $start timestamp_start $timestamp_start start_msecs '$start_msecs'"

        set timestamp_end [expr {$timestamp_start + $queuetime + $filtertime + $runtime}]
        set end_secs [ns_time seconds $timestamp_end]
        set end_msecs [string range 000000[ns_time microseconds $timestamp_end] end-5 end]
        set end [clock format $end_secs -format "%H:%M:%S"].$end_msecs

        set request_time_title [clock format $start_secs -format "%Y %b %d\n%H:%M:%S"].$start_msecs
        append request_time_title " -\n" [clock format $end_secs -format "%H:%M:%S"].$end_msecs

        set color(queuetime)  [::xo::colorize_slow_calls -fast 0.001 -warning 0.50 -danger 1.00 $queuetime]
        set color(filtertime) [::xo::colorize_slow_calls -fast 0.010 -warning 1.00 -danger 2.00 $filtertime]
        set color(runtime)    [::xo::colorize_slow_calls -fast 0.010 -warning 5.00 -danger 10.00 $runtime]
        set color(totaltime)  [::xo::colorize_slow_calls -fast 0.010 -warning 5.00 -danger 10.00 $totaltime]
    } else {
        lassign {"" "" "" ""} start queuetime filtertime runtime
        lassign {"" "" ""}    color(queuetime) color(filtertime) color(runtime)
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
    #ns_log notice "start $start [string length $start] end $end [string length $end]"
    if {$by_starttime} {
        set daydiff [expr {[clock format $start_secs -format %J] - [clock format $now -format %J]}]
        set prefix [expr {$daydiff == 0 ? "" : "<span class='daydiff'>${daydiff}d </span>"}]
        set time $prefix$start
    } else {
        set daydiff [expr {[clock format $end_secs -format %J] - [clock format $now -format %J]}]
        set prefix [expr {$daydiff == 0 ? "" : "<span class='daydiff'>${daydiff}d </span>"}]
        set time $prefix$end
    }
    set row [subst {<tr class=''>
        <td class='text-right text-end $color(totaltime)' style='white-space: nowrap;'><span class='info' title='$request_time_title'>$time</span></td>
        <td class='text-right text-end $color(queuetime)'><span class='info'>$queuetime</span></td>
        <td class='text-right text-end $color(filtertime)'>$filtertime</td>
        <td class='text-right text-end $color(runtime)'>$runtime</td>
        <td class='text-right text-end $color(totaltime)'><strong>$totaltime</strong></td>
        <td class='text-right text-end $color(totaltime)'>$userinfo</td>
        <td class='$color(totaltime)'>$iplink</td>
        <td class='$color(totaltime)'>$pool</td>
        <td class='$color(totaltime)'>$request</td>
        </tr>
    }]
    if {!$by_starttime} {
        append rows $row
    } else {
        append order_start($timestamp_start) $row \n
    }
}

if {$by_starttime} {
    foreach start_time [lsort -real -decreasing [array names order_start]] {
        append rows $order_start($start_time)
    }
    set request_time_label "Request Start"
} else {
    # rows are already in the order of end time
    set request_time_label "Request End"
}


set doc(title) "Long Calls"
set context [list $doc(title)]

#
# We always want bootstrap for the coloring and the filter. The markup
# in the .adp file is supposed to render reasonably for Bootstrap 3
# and Bootstrap 5 themes to ease maintenance.
#
if {[template::head::can_resolve_urn urn:ad:css:bootstrap5] && [string match "*bootstrap5*" [subsite::get_theme]]} {
    template::head::add_css -href urn:ad:css:bootstrap5 -media all
} elseif {[template::head::can_resolve_urn urn:ad:css:bootstrap3]} {
    template::head::add_css -href urn:ad:css:bootstrap3 -media all
} else {
    #
    # We have no Bootstrap based theme installed, so load it manually.
    #
    template::head::add_css -href //maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css -media all
    security::csp::require style-src maxcdn.bootstrapcdn.com
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
