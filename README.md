# ascribe

<!-- badges: start -->
[![R-CMD-check](https://github.com/VisruthSK/ascribe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/VisruthSK/ascribe/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/VisruthSK/ascribe/graph/badge.svg)](https://app.codecov.io/gh/VisruthSK/ascribe)
<!-- badges: end -->

ascribe scans R projects for package and function use. Other packages can use the results to generate citations without maintaining their own parser.

## Installation

You can install the development version of ascribe from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("VisruthSK/ascribe")
```

## Example

Build a package universe, then cite the code that uses it:

``` r
library(ascribe)

universe <- build_universe_data("stats")
usage <- scan_usage(path, universe$packages, universe$export_index, universe$origin_map)
cite_usage(usage)
```

## Limitations

- The scanner records function calls, not function references. It ignores `lapply(x, mean)` and `purrr::map(x, sd)`.
- Its built-in Markdown parser recognizes R chunks headed with `{r}`. Plain fenced blocks with an `r` info string are skipped.
- Package attachments are tracked within each file. A setup script that calls `library()` does not establish attachment state for another file.
- Default package citations require the package to be installed. Supply `package_citations` for packages that are unavailable locally.
- Scanning a namespace inspects its bindings for R6 classes. Active bindings can run their getters, and unusually deep generated expressions can exceed R's recursion limits.
