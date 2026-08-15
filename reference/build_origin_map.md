# Build an origin map for package functions

Given a named list mapping package names to character vectors of
function names, creates a named character vector mapping `"pkg::fun"`
keys to the origin package. Functions whose origin cannot be determined
fall back to the providing package.

## Usage

``` r
build_origin_map(exports)
```

## Arguments

- exports:

  Named list. Names are package names, values are character vectors of
  function names.

## Value

Environment mapping `"pkg::fun"` keys to origin package names.

## Examples

``` r
exports <- list(
  stats = collect_pkg_funs("stats"),
  utils = collect_pkg_funs("utils")
)
build_origin_map(exports)
#> <environment: 0x55c924ac5da8>
```
