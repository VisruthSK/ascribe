# Resolve the origin package of an exported function

Given a package and function name, determines which package the function
actually originates from (handling re-exports).

## Usage

``` r
resolve_origin(pkg, name)
```

## Arguments

- pkg:

  Package name (character scalar).

- name:

  Function name (character scalar).

## Value

The origin package name, or `NA_character_` if undetermined.

## Examples

``` r
resolve_origin("stats", "median")
#> [1] "stats"
```
