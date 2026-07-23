# Build an inverted export index

Given a named list mapping package names to character vectors of
function names (as produced by
[`collect_pkg_funs()`](https://ascribe.visruth.com/reference/collect_pkg_funs.md)),
creates an inverted index mapping function names to character vectors of
packages that export them.

## Usage

``` r
build_export_index(exports)
```

## Arguments

- exports:

  Named list. Names are package names, values are character vectors of
  function names.

## Value

Named list mapping function names to character vectors of package names.

## Examples

``` r
exports <- list(
  pkgA = c("foo", "bar"),
  pkgB = c("foo", "baz")
)
build_export_index(exports)
#> $bar
#> [1] "pkgA"
#> 
#> $baz
#> [1] "pkgB"
#> 
#> $foo
#> [1] "pkgA" "pkgB"
#> 
```
