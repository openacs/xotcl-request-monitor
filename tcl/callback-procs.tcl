ad_library {

    Procs to support a simple callback mechanism that allows other
    applications to register callbacks triggered when objects, like
    groups, in the subsite application are created.

    @author mbryzek@arsdigita.com
    @creation-date Wed Feb 21 17:10:24 2001
    @cvs-id $Id$

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
	ns_log Debug "subsite::parameter_changed -impl xotcl-request-monitor changing $parameter to $value"
	
	if {$parameter eq "trend-elements"} {
	    set slot_name "nr_trend_elements"
	} elsif {$parameter eq "max-stats-elements"} {
	    set slot_name "nr_stats_elements"
	} else {
	    return
	}

	throttle do eval {
	    ::Counter set $slot_name $value
	    foreach object [Counter allinstances] {$object set $slot_name $value}
	}
    }
}
