ad_page_contract {
  Displays active commnities

    @author Gustaf Neumann

    @cvs-id $Id$
} -query {
  {orderby:token,optional "count,desc"}
} -properties {
  title:onevalue
  context:onevalue
}

set title "Active Communities"
set context [list "Active Communities"]

TableWidget create t1 \
    -columns {
      AnchorField community -label Community -orderby community
      Field count -label Count -orderby count
    }

lassign [split $orderby ,] att order
t1 orderby -order [expr {$order eq "asc" ? "increasing" : "decreasing"}] $att

foreach {community_id users} [throttle users active_communities] {
  if {$community_id eq ""} continue

  if {[info commands ::dotlrn_community::get_community_name] ne ""} {
    set community_name [::dotlrn_community::get_community_name $community_id]
  } else {
    set community_name ""
  }
  if {$community_name eq ""} {
    set community_name [::xo::db::sql::apm_package name -package_id $community_id]
  }
  
  t1 add \
      -community $community_name \
      -community.href [export_vars -base users-in-community {community_id community_name}] \
      -count [llength [lsort -unique [eval concat $users]]]
}
set t1 [t1 asHTML]
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
