# Collect R6 class method names from a package

Scans both exported objects and namespace-internal objects for R6 class
generators, then collects all public method names.

## Usage

``` r
collect_r6_methods(pkg, export_names)
```

## Arguments

- pkg:

  Package name (character scalar).

- export_names:

  Character vector of exported names (from
  [`getNamespaceExports()`](https://rdrr.io/r/base/ns-reflect.html)).

## Value

Character vector of R6 method names.

## Examples

``` r
collect_r6_methods("testthat", getNamespaceExports("testthat"))
#>  [1] "initialize"             "add_result"             "end_reporter"          
#>  [4] "clone"                  "start_context"          "start_reporter"        
#>  [7] "start_file"             "start_test"             "end_test"              
#> [10] "end_context"            "end_file"               "update"                
#> [13] "show_header"            "show_status"            "report_issue"          
#> [16] "get_results"            "is_full"                "local_user_output"     
#> [19] "cat_tight"              "cat_line"               "rule"                  
#> [22] ".start_context"         "end_context_if_started" "elapsed_time"          
#> [25] "reset_suite"            "status_data"            "expectations"          
#> [28] "cr"                     "report_full"            "report_issues"         
#> [31] "should_update"          "show_timing"            "get"                   
#> [34] "set"                    "append"                 "reset"                 
#> [37] "write"                  "delete"                 "variants"              
#> [40] "filename"               "path"                   "take_snapshot"         
#> [43] "take_file_snapshot"     "announce_file_snapshot" "is_active"             
#> [46] "snap_files"             "push"                   "size"                  
#> [49] "as_list"                "sum"                    "public_fun"            
#> [52] "list_tasks"             "get_num_waiting"        "get_num_running"       
#> [55] "get_num_done"           "is_idle"                "poll"                  
```
