# Find used functions and packages

Statically scans R source files for package attachments and function
calls. It recognizes [`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html),
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), and
[`use()`](https://rdrr.io/r/base/use.html).

## Usage

``` r
scan_usage(
  path = ".",
  universe,
  ignore_unqualified_functions = .stdlib_funs,
  strict = FALSE,
  skip_dirs = .scan_skip_dirs,
  metapackages = NULL,
  use_knitr = FALSE
)
```

## Arguments

- path:

  A single project directory (searched recursively) or a vector of files
  (.R/.Rmd/.qmd).

- universe:

  A universe returned by
  [`build_universe_data()`](https://ascribe.visruth.com/reference/build_universe_data.md).

- ignore_unqualified_functions:

  Defaults to exports from base R packages listed in
  [`stdlib_funs()`](https://ascribe.visruth.com/reference/internal_data.md).
  Character vector of function names to ignore when attributing
  (unqualified) calls. Calls like `pkg::fun()` will NOT be ignored even
  if `fun` is in `ignore_unqualified_functions`, since they are
  namespaced.

- strict:

  If `FALSE` (default), warn on ambiguous function calls whose origin
  cannot be determined exactly. If `TRUE`, abort on ambiguous calls.

- skip_dirs:

  Character vector of directory names to skip when scanning a directory.
  Defaults to
  [`scan_skip_dirs()`](https://ascribe.visruth.com/reference/internal_data.md).

- metapackages:

  Named list mapping attached package names to additional packages that
  should be treated as co-attached for unqualified resolution. Defaults
  to `NULL`.

- use_knitr:

  Logical. If `TRUE`, parse `.Rmd` and `.qmd` files with
  [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html), which
  resolves knitr features the in-house parser ignores, such as `child`
  documents. It also comments out `eval=FALSE` and `purl=FALSE` chunks,
  so usage in them goes unrecorded. Defaults to `FALSE`.

## Value

A list of packages, resolved functions, and ambiguous function calls.

## Details

Explicit package references from
[`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html),
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html),
[`use()`](https://rdrr.io/r/base/use.html), and `pkg::fun` are only
recorded when their package is included in `allowed_packages`. The
scanner attributes an unqualified function only when
[`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html) attached a package
earlier in the same file and the supplied indexes can resolve the call.
`metapackages` can add packages to that attachment set. If several
attached packages export the function, the most recently attached match
wins. The scanner attributes known re-exports to their origin package
and otherwise to the resolved package.

## Examples

``` r
path <- tempfile(fileext = ".R")
writeLines(
  c(
    "# one messy analysis file",
    "library(stats)",
    "requireNamespace(\"utils\")",
    "filter(1:10, rep(1, 3))",
    "utils::head(letters)"
  ),
  path
)
universe <- build_universe_data(c("stats", "utils"))
scan_usage(path, universe, ignore_unqualified_functions = character())
#> ℹ Searching /tmp/RtmpRwEiHE/file19c24195db31.R
#> $packages
#> [1] "stats" "utils"
#> 
#> $functions
#> [1] "stats::filter" "utils::head"  
#> 
#> $ambiguous
#> character(0)
#> 
#> attr(,"class")
#> [1] "scan_usage"
unlink(path)
```
