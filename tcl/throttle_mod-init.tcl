# we register the following filters only during startup, since
# existing connection threads are not aware of the throttle object.
if {[ns_server connections] == 0 && [info commands ::throttle] ne ""} {
  #
  # Register the filter progs for url statistics.
  # The methods to be called have the name of the filter type.
  #
  ns_register_filter trace GET * throttle
  ns_register_filter trace POST * throttle
  ns_register_filter trace HEAD * throttle  

  #ns_register_filter postauth GET * throttle
  #ns_register_filter postauth POST * throttle
  ad_register_filter -priority 1000 postauth GET * throttle
  ad_register_filter -priority 1000 postauth POST * throttle
  ad_register_filter -priority 1000 postauth HEAD * throttle  
}

#
# Check if we are running under OpenACS; if not, provide
# minimal compatibility code.
#
if {[info commands ad_conn] eq ""} {
  #
  # Otherwise provide alias for "ad_conn"
  #
  interp alias {} ad_conn {} ns_conn
}

#
# When activity tracking is on, and we have a recent version of
# NaviServer, then "::xo::job_dequeue" is defined. We define a
# scheduled proc that runs every 61 seconds the executes the collected
# cmd in the background. This version produces substantially less locks
# on busy servers and reduces latency for the client.
#
set do_track_activity [parameter::get_from_package_key \
                           -package_key "xotcl-request-monitor" \
                           -parameter do_track_activity \
                           -default false]
if {$do_track_activity && [info commands ::xo::job_dequeue] ne ""} {
  nsv_set request_monitor jobs {}
  ad_schedule_proc -thread t 61 ::xo::job_dequeue
}

#
# Launch the pool remapper background job to move requests to the slow
# pool, even before these are finished. This is necessary for jobs
# which are never finishing.
# 
ad_schedule_proc -thread t 59 ::xo::pool_remap_watchdog

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
