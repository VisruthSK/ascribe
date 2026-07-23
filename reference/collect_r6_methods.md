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
