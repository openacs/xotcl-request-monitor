
# we register the following filters only during startup, since
# existing connection threads are not aware of the throttle object.
if {[ns_server connections]==0} {
  # 
  # Register the filter progs for url statistics.
  # The methods to be called have the name of the filter type.
  #
  ns_register_filter trace GET * throttle 
  ns_register_filter trace POST * throttle

  #ns_register_filter postauth GET * throttle 
  #ns_register_filter postauth POST * throttle 
  ad_register_filter -priority 1000 postauth GET * throttle
  ad_register_filter -priority 1000 postauth POST * throttle
}

# check if we are running under oacs; if not, provide 
# minimal compatibility code
if {[info commands ad_conn] eq ""} {
  # otherwise provide alias for ad_conn and dummy for ad_get_user_id
  interp alias {} ad_conn {} ns_conn
  ### this is probably not sufficient to do something useful...
}

# Populate the counters
# Initialize from the old counters

set logdir [parameter::get_from_package_key -package_key xotcl-request-monitor \
		-parameter log-dir \
		-default [file dirname [file root [ns_config ns/parameters ServerLog]]]]

set nr_trend_elements [parameter::get_from_package_key -package_key "xotcl-request-monitor" -parameter "trend-elements" -default 48]
incr nr_trend_elements

# Create the file to load. This is per hour = 60*3 + 2 lines
set number_of_lines [expr 182 * $nr_trend_elements]
exec /usr/bin/tail -n $number_of_lines ${logdir}/counter.log >${logdir}/counter-new.log
set f [open $logdir/counter-new.log]

while {-1 != [gets $f line]} {
    regexp {(.*) -- (.*) ::(.*) (.*)} $line match timestamp server label value
    set nr_stats_elements [parameter::get_from_package_key -package_key "xotcl-request-monitor" -parameter "max-stats-elements" -default 48]
    throttle do $label lappend trend $value
    set stats [throttle do $label lappend stats [list $timestamp $value]]
    set stats [lrange [lsort -real -decreasing -index 1 $stats] 0 [expr {$nr_stats_elements - 1}]]
    throttle do $label set stats $stats
}

