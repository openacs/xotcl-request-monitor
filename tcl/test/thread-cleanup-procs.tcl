ad_library {
  Regression test for the throttle destroy notice after NX cache cleanup.
}

aa_register_case -cats {api smoke production_safe} -procs {
  "::xotcl::THREAD instproc destroy" ::tcltrace::before-ns_log
} thread_destroy_after_cache_cleanup {
  A real XOTcl controller destructor, including the throttle destroy notice,
  can run after NX request caches disappear during blueprint replacement.
} {
  set child [interp create]
  try {
    interp eval $child [list set auto_path $::auto_path]
    interp eval $child {
      package require XOTcl
      namespace import -force ::xotcl::*
      namespace eval ::acs {}
      namespace eval ::parameter {}
      namespace eval ::tcltrace {}
      set notices {}; set parameterCalls 0
      proc ns_log {severity message} {lappend ::notices [list $severity $message]}
      proc ns_info {key} {if {$key ne "shutdownpending"} {error unexpected}; return 0}
      proc ::parameter::get_from_package_key {args} {incr ::parameterCalls; return {}}
      ::nx::Object create ::acs::per_request_cache
      Class create ::xotcl::THREAD -parameter {{persistent 1}}
      ::xotcl::Object instproc log args {ns_log notice [join $args]}
    }
    # Use the actual THREAD destructor, not a reduced copy of its logic.
    interp eval $child [list ::xotcl::THREAD instproc destroy {} \
        [::xotcl::THREAD info instbody destroy]]
    set channel [open [file join [acs_package_root_dir acs-tcl] tcl tcltrace-procs.tcl]]
    try {set text [read $channel]} finally {close $channel}
    set start [string first {ad_proc -private before-ns_log } $text]
    set declaration {}
    foreach line [split [string range $text $start end] \n] {
      append declaration $line \n
      if {[info complete $declaration]} break
    }
    interp eval $child [list proc ::tcltrace::before-ns_log {cmd op} [lindex $declaration end]]
    # Keep the monitor's unconditional destroy notice in the regression.
    set channel [open [file join [acs_package_root_dir xotcl-request-monitor] tcl throttle_mod-procs.tcl]]
    try {set text [read $channel]} finally {close $channel}
    set start [string first {throttle proc destroy } $text]
    set declaration {}
    foreach line [split [string range $text $start end] \n] {
      append declaration $line \n
      if {[info complete $declaration]} break
    }
    interp eval $child {
      ::xotcl::THREAD create ::throttle
      trace add execution ns_log enter ::tcltrace::before-ns_log
    }
    interp eval $child [list ::throttle proc destroy {} [lindex $declaration end]]
    interp eval $child {
      ::acs::per_request_cache destroy
      ::throttle destroy
    }
    aa_false "Controller is completely destroyed despite already removed NX cache" \
        [interp eval $child {::nsf::is object ::throttle}]
    aa_equals "Destruction never calls the missing cache through parameter lookup" \
        [interp eval $child {set parameterCalls}] 0
    aa_true "The throttle destroy notice is retained" \
        [interp eval $child {expr {[lsearch -exact $notices \
            {notice {::throttle: destroying object (shutdownpending 0)}}] >= 0}}]
  } finally {interp delete $child}
}
