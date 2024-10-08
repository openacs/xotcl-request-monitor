ad_page_contract {
    Displays call statistics

    @author Gustaf Neumann

    @cvs-id $Id$
} -query {
    {all:boolean,optional,notnull 0}
    {with_param:boolean,optional,notnull 1}
    {with_apps:boolean,optional,notnull 0}
    {orderby:token,optional "totaltime,desc"}
} -properties {
    title:onevalue
    context:onevalue
    user_string:onevalue
}
set title "Url Statistics"
set context [list "Url Statistics"]

set admin_p [acs_user::site_wide_admin_p]
if {!$admin_p} {
  ad_return_warning "Insufficient Permissions" \
      "Only side wide admins are allowed to view this page!"
  ad_script_abort
}

set hide_patterns [parameter::get -parameter hide-requests -default {*.css}]
array set apps {
  calendar 1 acs-templating 1 forums 1 file-storage 1 one-community 1
  xowiki 1
  annotations 1 gradebook 1 homework 1 lecturecast 1 res 1
}
array set vuh {
  ical 1
  lecturecast 1
  download 1
}

set stat [throttle report_url_stats]
set total 0.0
set total_cnt 0

foreach l $stat {
  set total [expr {$total+[lindex $l 1]}]
  incr total_cnt [lindex $l 2]
}
set total_avg [expr {$total_cnt>0 ? $total/($total_cnt*1000.0) : "0" }]

set full_stat [list]
if {$with_param == 0} {
  # without parameter
  # add up same URLs
  unset -nocomplain aggr_stat
  foreach l $stat {
    lassign $l url time cnt
    set p ""
    set has_param [regexp {^(.*)[?]} $url _ url]
    #
    # truncate tails, if we have VUHs
    #
    set url_list [list]
    foreach p [split $url /] {
      if {[info exists vuh($p)]} {
        lappend url_list $p
        set url [join $url_list /]/...
        break
      }
      lappend url_list $p
    }
    if {$has_param} {append url ?...}
    set key aggr_stat($url)
    if {[info exists $key]} {
      set time [expr {[lindex [set $key] 0] + $time}]
      set cnt [expr {[lindex [set $key] 1] + $cnt}]
    }
    set aggr_stat($url) [list $time $cnt]
  }
  set stat [list]
  foreach url [array names aggr_stat] {
    lassign $aggr_stat($url) time cnt
    lappend stat [list $url $time $cnt]
  }
}
if {$with_apps == 1} {
  # reduce statistics to apps
  unset -nocomplain aggr_stat
  foreach l $stat {
    lassign $l url time cnt
    set param ""
    regexp {^(.*)([?].*$)} $url _ url param
    set url_list [list]
    foreach p [split $url /] {
      if {[info exists apps($p)]} {
        if {[llength $url_list]>0} {set url_list [list .../$p]}
      } else {
        lappend url_list $p
      }
    }
    set url [join $url_list /]$param
    set key aggr_stat($url)
    if {[info exists $key]} {
      set time [expr {[lindex [set $key] 0] + $time}]
      set cnt [expr {[lindex [set $key] 1] + $cnt}]
    }
    set aggr_stat($url) [list $time $cnt]
  }
  set stat [list]
  foreach url [array names aggr_stat] {
    lassign $aggr_stat($url) time cnt
    lappend stat [list $url $time $cnt]
  }
}
set full_stat {}
# append avg
foreach l $stat {
    lassign $l url time cnt
    set avg [expr {$cnt>0 ? $time*1.0/$cnt : 0.0}]
    lappend full_stat [list $url $time $cnt $avg]
}

set show_all_label(0) "Show filtered"
set show_all_tooltip(0) "Show filtered values"
set show_all_label(1) "Show all"
set show_all_tooltip(1) "Show all values"
set not_all [expr {!$all}]

set with_param_label(1) "Without parameter"
set with_param_tooltip(1) "Statistics without parameters"
set with_param_label(0) "With parameter"
set with_param_tooltip(0) "Statistics with parameters"
set not_with_param [expr {!$with_param}]

set with_apps_label(1) "With communities"
set with_apps_tooltip(1) "Statistics with comm
unities"
set with_apps_label(0) "Strip communities"
set with_apps_tooltip(0) "Statistics without Communities"
set not_with_apps [expr {!$with_apps}]

set url_all   [export_vars -base [ad_conn url] {{all $not_all} with_apps with_param}]
set url_apps  [export_vars -base [ad_conn url] {all {with_apps $not_with_apps} with_param}]
set url_param [export_vars -base [ad_conn url] {all with_apps {with_param $not_with_param}}]

TableWidget create t1 -volatile \
    -actions [subst {
      Action new -label "$show_all_label($all)" -url $url_all -tooltip "show_all_tooltip($all)"
      Action new -label "$with_param_label($with_param)" -url $url_param -tooltip "with_param_tooltip($with_param)"
      Action new -label "$with_apps_label($with_apps)" -url $url_apps -tooltip "with_apps_tooltip($with_apps)"
      Action new -label "Delete Statistics" -url flush-url-statistics \
          -tooltip "Delete URL Statistics"
    }] \
    -columns {
      AnchorField url   -label "Request" -orderby url
      Field totaltime -label "Total Time" -html { align right } -orderby totaltime
      Field cnt   -label "Count"          -html { align right } -orderby cnt
      Field avg   -label "Ms"             -html { align right } -orderby avg
      Field total -label "Total"          -html { align right }
    }

  set nr 0
  set hidden 0
  set all [expr {!$all}]
  foreach l $full_stat {
    set avg [expr {[lindex $l 1]/[lindex $l 2]}]
    set rel [expr {($avg/1000.0)/$total_avg}]
    set url [lindex $l 0]
    if {!$all} {
      set exclude 0
      foreach pattern $hide_patterns {
        if {[string match $pattern $url]} {
          set exclude 1
          incr hidden
          break
        }
      }
      if {$exclude} continue
    }

    t1 add      -url [ad_string_truncate_middle -len 80 $url] \
                -url.href [expr {[string match "*...*" $url] ? "" : "[ad_url]$url" }] \
                -totaltime [lindex $l 1] \
                -cnt [lindex $l 2] \
                -avg $avg \
                -total [format %.2f%% [expr {[lindex $l 1]*100.0/$total}]]
  }

lassign [split $orderby ,] att order
t1 orderby \
    -order [ad_decode $order desc decreasing asc increasing increasing] \
    -type [ad_decode $att totaltime integer cnt integer avg real dictionary] \
    $att

set t1 [t1 asHTML]

append user_string "<b>Grand Total Avg Response time: </b>" \
        [format %6.2f $total_avg] " seconds/call " \
        "(base: $total_cnt requests)<br>"

append user_string "$hidden requests hidden."
if {$hidden>0} {
  append user_string " (Patterns: $hide_patterns)"
}


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
