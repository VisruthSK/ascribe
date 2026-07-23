# Generate sysdata.rda for a package universe

Computes scanner data for the given packages and saves it to
`sysdata.rda` with variable names prefixed by `prefix`. This is intended
for use in a downstream package's `data-raw/sysdata.R` script.

## Usage

``` r
generate_universe_sysdata(
  packages,
  prefix,
  extra_vars = list(),
  include_scanner_defaults = FALSE,
  file = "R/sysdata.rda"
)
```

## Arguments

- packages:

  Character vector of package names.

- prefix:

  Character scalar used to name the saved objects (e.g., `"stan"`
  produces `.stan_pkgs`).

- extra_vars:

  Named list of additional objects to include in the saved file (e.g.,
  citation environments).

- include_scanner_defaults:

  If `TRUE`, also saves `.stdlib_funs` and `.scan_skip_dirs`. Defaults
  to `FALSE`.

- file:

  Output path. Defaults to `"R/sysdata.rda"`.

## Value

Invisibly returns the result of
[`build_universe_data()`](https://ascribe.visruth.com/reference/build_universe_data.md).

## Details

The generated variables are:

- `.{prefix}_pkgs`:

  Character vector of package names.

- `.{prefix}_exports`:

  Named list of exported functions per package.

- `.{prefix}_export_index`:

  Inverted index: function name to packages.

- `.{prefix}_origin_map`:

  Named character vector: `"pkg::fun"` to origin.

- `.{prefix}_pkg_versions`:

  Named list of version strings.

When `include_scanner_defaults` is `TRUE`, `.stdlib_funs` and
`.scan_skip_dirs` are also saved.

## Examples

``` r
file <- tempfile(fileext = ".rda")
generate_universe_sysdata(c("stats", "utils"), "my", file = file)
#> ✔ Successfully generated /tmp/RtmphVTjdg/file19f46dd22873.rda
unlink(file)
```
