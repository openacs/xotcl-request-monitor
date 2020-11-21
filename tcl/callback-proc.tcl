::xo::library doc {
  XOTcl request monitor - Callback procs

  @creation-date 2020-11-20
  @author Gustaf Neumann
}

namespace eval ::request_monitor {
  ad_proc -private upgrade_callback {
    {-from_version_name:required}
    {-to_version_name:required}
  } {

    Callback for upgrading

    @author Gustaf Neumann (neumann@wu-wien.ac.at)
  } {
    ns_log notice "-- UPGRADE xotcl-request-monitor $from_version_name -> $to_version_name"

    set v 0.58
    if {[apm_version_names_compare $from_version_name $v] == -1 &&
        [apm_version_names_compare $to_version_name $v] > -1} {
      ns_log notice "-- upgrading to $v"

      foreach parameter_id [xo::dc list get_old_parameter {
        select parameter_id
        from apm_parameters
        where package_key = 'xotcl-request-monitor'
        and parameter_name not in ('do_slowdown_overactive', 'max-url-stats', 'time-window',
                                   'trend-elements', 'max-stats-elements', 'do_throttle',
                                   'do_track_activity', 'do_slowdown_overactive',
                                   'do_double_click_prevention', 'peer-groups', 'hide-requests')
      }] {
        xo::dc dml delete_value {
          delete from apm_parameter_values where parameter_id = :parameter_id
        }
        xo::dc dml delete_parameter {
          delete from apm_parameters where parameter_id = :parameter_id
        }
        
      }
      
      # apm_parameter_value__delete(id)
    }
  }
}

::xo::library source_dependent

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
