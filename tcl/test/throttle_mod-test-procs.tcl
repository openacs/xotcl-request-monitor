ad_library {
    Automated tests for throttle_mod.

    @author Héctor Romojaro <hector.romojaro@gmail.com>
    @creation-date 2019-06-26
}

aa_register_case \
    -cats {api smoke production_safe} \
    -procs {ad_string_truncate_middle} \
    ad_string_truncate_middle {

        Test the ad_string_truncate_middle proc

        @author Hanifa Hasan
} {
    set full_string "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd"
    aa_equals "Empty ellipsis short length" [ad_string_truncate_middle -ellipsis "" -len 5 $full_string ] "sasd"
    aa_equals "Empty ellipsis"              [ad_string_truncate_middle -ellipsis "" $full_string ] "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd" 
    aa_equals "No length"                   [ad_string_truncate_middle -ellipsis ":::::::::::" $full_string ] "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd"
    aa_equals "Short length"                [ad_string_truncate_middle -ellipsis ":::::::::::" -len 10 $full_string ] "sadlf:::::::::::ydfsd"
    aa_equals "Default options"             [ad_string_truncate_middle $full_string ] "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd"
    aa_equals "Length 0, default ellipsis"  [ad_string_truncate_middle -len 0 $full_string ] "..."
    aa_equals "Length 25, default ellipsis" [ad_string_truncate_middle -len 25 $full_string ] "sadlfvnsaödc...4zbrefsydfsd"
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
