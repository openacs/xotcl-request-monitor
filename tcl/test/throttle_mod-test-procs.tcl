ad_library {
    Automated tests for throttle_mod.

    @author Héctor Romojaro <hector.romojaro@gmail.com>
    @creation-date 2019-06-26
}

aa_register_case \
    -cats {api smoke production_safe} \
    -procs {string_truncate_middle} \
    string_truncate_middle {

        Test the string_truncate_middle proc

        @author Hanifa Hasan
} {
    aa_run_with_teardown \
        -rollback \
        -test_code {
            set full_string "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd"
            aa_equals "Empty ellipsis short length" [string_truncate_middle -ellipsis "" -len 5 $full_string ] "sasd"
            aa_equals "Empty ellipsis"              [string_truncate_middle -ellipsis "" $full_string ] "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd" 
            aa_equals "No length"                   [string_truncate_middle -ellipsis ":::::::::::" $full_string ] "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd"
            aa_equals "Short length"                [string_truncate_middle -ellipsis ":::::::::::" -len 10 $full_string ] "sadlf:::::::::::ydfsd"
            aa_equals "Default options"             [string_truncate_middle $full_string ] "sadlfvnsaödcösalcmksadöldcmsasdcsadvwaef4q3t54zbrefsydfsd"
            aa_equals "Length 0, default ellipsis"  [string_truncate_middle -len 0 $full_string ] "..."
            aa_equals "Length 25, default ellipsis" [string_truncate_middle -len 25 $full_string ] "sadlfvnsaödc...4zbrefsydfsd"
        }
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
