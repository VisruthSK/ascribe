# Cite package and function use in a project

Builds citations from
[`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md)
results. Package collections supply their own citation records and
package-citation policy.

## Usage

``` r
cite_usage(
  usage,
  package_citations = new.env(parent = emptyenv(), hash = TRUE),
  function_citations = new.env(parent = emptyenv(), hash = TRUE),
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

  An environment of package citation entries. Missing packages use
  `package_citation`.

- function_citations:

  An environment of function citation entries, keyed by
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
writeLines("cli::cli_alert_info('hi'); fastmatch::fmatch(1, 1:5)", path)
universe <- build_universe_data(c("cli", "fastmatch"))
usage <- scan_usage(path, universe)
#> ℹ Searching /tmp/RtmpRwEiHE/file19c27fa8a36e.R
cite_usage(usage)
#> @Manual{,
#>   title = {cli: Helpers for Developing Command Line Interfaces},
#>   author = {Gábor Csárdi},
#>   year = {2026},
#>   note = {R package version 3.6.6},
#>   url = {https://cli.r-lib.org},
#> }
#> 
#> @Manual{,
#>   title = {fastmatch: Fast 'match()' Function},
#>   author = {Simon Urbanek},
#>   year = {2026},
#>   note = {R package version 1.1-8},
#>   url = {https://www.rforge.net/fastmatch},
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
