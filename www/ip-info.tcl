ad_page_contract {
  Displays information about an IP address

    @author Gustaf Neumann

    @cvs-id $Id$
} -query {
    {ip:token}
} -properties {
    title:onevalue
    context:onevalue
}

set title "IP Lookup"
set context [list $title]

set admin_p [acs_user::site_wide_admin_p]
if {!$admin_p} {
  ad_return_warning "Insufficient Permissions" \
      "Only side wide admins are allowed to view this page!"
  ad_script_abort
}

if {[catch {set dns_name [ns_hostbyaddr $ip]}]} {
  set dns_name "DNS lookup for $ip failed"
}


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
