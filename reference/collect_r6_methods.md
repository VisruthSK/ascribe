# Collect R6 class method names from a package

Scans exported and namespace-internal objects for R6 class generators,
then collects all public method names.

## Usage

``` r
collect_r6_methods(pkg)
```

## Arguments

- pkg:

  Package name (character scalar).

## Value

Character vector of R6 method names.
