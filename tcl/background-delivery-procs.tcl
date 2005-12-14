ad_library {

    Routines for background delivery of files

    @author Gustaf Neumann (neumann@wu-wien.ac.at)
    @creation-date 19 Nov 2005
    @cvs-id $Id$
}

::xotcl::THREAD create bgdelivery {
  set ::delivery_count 0

  proc deliver {ch filename context} {
    set fd [open $filename]
    fconfigure $fd -translation binary
    fconfigure $ch -translation binary
    #ns_log notice "--- start of delivery of $filename (running:[array size ::running])"
    fcopy $fd $ch -command [list end-delivery $filename $fd $ch]
    set ::running($ch,$filename) $context
    incr ::delivery_count
  }

  proc end-delivery {filename fd ch bytes args} {
    #ns_log notice "--- end of delivery of $filename, $bytes bytes written $args"
    if {[catch {close $ch} e]} {ns_log notice "delivery, closing channel for $filename, error: $e"}
    if {[catch {close $fd} e]} {ns_log notice "delivery, closing file $filename, error: $e"}
    unset ::running($ch,$filename)
  }
} -persistent 1

bgdelivery ad_forward running {
  Interface to the background delivery thread to query the currently running deliveries.
  @return list of key value pairs of all currently running background processes
} %self do array get running


bgdelivery ad_forward nr_running {
  Interface to the background delivery thread to query the number of currently running deliveries.
  @return number of currently running background deliveries
} %self do array size running


ad_proc -public ad_returnfile_background {statuscode mime_type filename} {
  Deliver the given file to the requestor in the background. This proc uses the
  background delivery thread to send the file in an event-driven manner without
  blocking a request thread. This is especially important when large files are 
  requested over slow (e.g. dial-ip) connections.
} {
  #ns_log notice "statuscode = $statuscode, filename=$filename"
  set size [file size $filename]
  if {[ns_headers xxx $statuscode $mime_type $size]} {
    set ch [ns_conn channel]
    thread::transfer [bgdelivery get_tid] $ch
    throttle get_context
    bgdelivery do -async deliver $ch $filename \
	[list [throttle set requestor],[throttle set url] [ns_conn start]]
    ns_conn contentsentlength $size;      #maybe overly optimistic
  }
}
