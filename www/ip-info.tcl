ad_page_contract {
  Displays information about an ip address

    @author Gustaf Neumann 

    @cvs-id $id$
} -query {
    {ip}
} -properties {
    title:onevalue
    context:onevalue
}

set title "IP Lookup"
set context [list $title]

if {[catch {set dns_name [ns_hostbyaddr $ip]}]} { set dns_name "DNS lookup for $ip failed" } 

