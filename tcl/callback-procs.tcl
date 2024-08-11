::xo::library doc {
  XOTcl request monitor - Callback procs

  Procs to support a simple callback mechanism that allows other
  applications to register callbacks triggered when objects, like
  groups, in the subsite application are created.

  @creation-date 2020-11-20
  @author Gustaf Neumann
}

ad_proc -public -callback subsite::parameter_changed -impl xotcl-request-monitor {
  -package_id:required
  -parameter:required
  -value:required
} {
  Implementation of subsite::parameter_changed for xotcl-request-monitor

  @author Nima Mazloumi (nima.mazloumi@gmx.de)
  @creation-date 2005-08-17

  @param package_id the package_id of the package the parameter was changed for
  @param parameter  the parameter name
  @param value      the new value

  @see package::set_value
} {

  if {$package_id == [apm_package_id_from_key "xotcl-request-monitor"]} {
    ns_log debug "subsite::parameter_changed -impl xotcl-request-monitor changing $parameter to $value"
    #
    # Just update these parameters, which are defined in the throttle thread.
    #
    if {[throttle do info commands $parameter] ne ""} {
      throttle do $parameter update $value
    }
  }
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

    set v 0.60
    if {[apm_version_names_compare $from_version_name $v] == -1 &&
        [apm_version_names_compare $to_version_name $v] > -1} {
      ns_log notice "-- upgrading to $v"

      foreach parameter_id [xo::dc list get_old_parameter {
        select parameter_id
        from apm_parameters
        where package_key = 'xotcl-request-monitor'
        and parameter_name not in (
                                   'do_double_click_prevention',
                                   'do_slowdown_overactive',
                                   'do_slowdown_overactive',
                                   'do_throttle',
                                   'do_track_activity',
                                   'hide-requests',
                                   'max-stats-elements',
                                   'max-url-stats',
                                   'monitor_urls',
                                   'peer-groups',
                                   'time-window',
                                   'trend-elements'
                                   )
      }] {
        xo::dc dml delete_value {
          delete from apm_parameter_values where parameter_id = :parameter_id
        }
        xo::dc dml delete_parameter {
          delete from apm_parameters where parameter_id = :parameter_id
        }
      }
    }
  }

  ad_proc -private after_mount {
    -package_id:required
    -node_id:required
  } {

    Modify default permissions after mount to restrict read access to
    the package from public read to read access for registered users.

  } {
    #ns_log notice "-- After mount callback package_id $package_id node_id $node_id"
    
    #
    # Turn off inheritance from public site
    #
    permission::set_not_inherit -object_id $package_id
    #
    # Allow registered users to read
    #
    permission::grant -party_id [acs_magic_object registered_users] \
        -object_id $package_id \
        -privilege read
  }
}
  
::xo::library source_dependent

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
