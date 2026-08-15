# Changelog

## ascribe 0.2.0

### Breaking changes

- [`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md)
  now takes a single `universe` argument, built by
  [`build_universe_data()`](https://ascribe.visruth.com/reference/build_universe_data.md),
  instead of separate `allowed_packages`, `export_index`, and
  `origin_map` arguments.
- [`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md)
  no longer has a `quiet` argument.
- [`build_origin_map()`](https://ascribe.visruth.com/reference/build_origin_map.md)
  returns an environment instead of a named character vector.

### Other changes

- `ascribe_universe` objects gain a
  [`print()`](https://rdrr.io/r/base/print.html) method.
- Fixed a re-export attribution bug that could resolve some functions to
  the wrong origin package.
- The in-house Rmd/qmd parser now keeps usage from chunks that parse
  even when another chunk in the same file fails, instead of dropping
  the whole file.
- [`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md)
  is much faster and uses far less memory.

## ascribe 0.0.0.9000

- Initial CRAN submission.
