# Build scanner data for a package universe

Given a character vector of package names, computes the export lists,
inverted export index, origin map, and version snapshot needed by
[`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md).
All packages must be installed.

## Usage

``` r
build_universe_data(packages)
```

## Arguments

- packages:

  Character vector of package names.

## Value

An `ascribe_universe` object with components:

- packages:

  The input package names.

- exports:

  Named list mapping package names to character vectors of exported
  function names (from
  [`collect_pkg_funs()`](https://ascribe.visruth.com/reference/collect_pkg_funs.md)).

- export_index:

  Named list mapping function names to character vectors of packages
  (from
  [`build_export_index()`](https://ascribe.visruth.com/reference/build_export_index.md)).

- origin_map:

  Environment mapping `"pkg::fun"` keys to origin packages (from
  [`build_origin_map()`](https://ascribe.visruth.com/reference/build_origin_map.md)).

- pkg_versions:

  Named list mapping package names to version strings.

## Examples

``` r
build_universe_data(c("stats", "utils"))
#> <ascribe_universe>
#> • stats: 464 indexed functions
#> • utils: 233 indexed functions
```
