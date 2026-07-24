# Cite package and function use in a project

Builds citations from
[`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md)
results. Package collections supply their own citation records and
package-citation policy.

## Usage

``` r
cite_usage(
  usage,
  package_citations = list(),
  function_citations = list(),
  package_citation = utils::citation,
  always_cite = character(),
  format = c("bibtex", "bibentry")
)
```

## Arguments

- usage:

  Results returned by
  [`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md).

- package_citations:

  A named list or environment of package citation entries. Missing
  packages use `package_citation`.

- function_citations:

  A named list or environment of function citation entries, keyed by
  `"pkg::function"`.

- package_citation:

  A function that accepts a package name and returns its citation
  entries. Defaults to
  [`utils::citation()`](https://rdrr.io/r/utils/citation.html).

- always_cite:

  Character vector of packages to cite in addition to the packages found
  by the scan.

- format:

  One of `"bibtex"` or `"bibentry"`.

## Value

A BibTeX character vector or a bibentry object.

## Examples

``` r
path <- tempfile(fileext = ".R")
writeLines("stats::median(1:3)", path)
universe <- build_universe_data(c("stats", "tools"))
usage <- scan_usage(path, universe$packages, universe$export_index, universe$origin_map)
#> ℹ Searching /tmp/Rtmpj0Bjn0/file19f68ae7da.R
cite_usage(usage)
#> @Manual{,
#>   title = {R: A Language and Environment for Statistical Computing},
#>   author = {{R Core Team}},
#>   organization = {R Foundation for Statistical Computing},
#>   address = {Vienna, Austria},
#>   year = {2026},
#>   doi = {10.32614/R.manuals},
#>   url = {https://www.R-project.org/},
#> }
#> 
#> @Manual{,
#>   title = {R: A Language and Environment for Statistical Computing},
#>   author = {{R Core Team}},
#>   organization = {R Foundation for Statistical Computing},
#>   address = {Vienna, Austria},
#>   year = {2026},
#>   doi = {10.32614/R.manuals},
#>   url = {https://www.R-project.org/},
#> }
unlink(path)
```
