# Build a Citation Scanner

For a complete, real-world implementation of a package citation scanner
built with `ascribe`, see
[stanflow](https://github.com/VisruthSK/stanflow).

`ascribe` separates AST parsing from citation generation. This guide
demonstrates building a citation scanner targeting Stan packages
(`cmdstanr`, `posterior`, `bayesplot`, `rstan`).

## Build the package universe

[`build_universe_data()`](https://ascribe.visruth.com/reference/build_universe_data.md)
extracts exported functions, R6 methods, and origin mappings for
re-exported functions.

``` r

stan_pkgs <- c("cmdstanr", "posterior", "bayesplot", "rstan")
universe <- build_universe_data(stan_pkgs)
names(universe)
```

Run
[`build_universe_data()`](https://ascribe.visruth.com/reference/build_universe_data.md)
in an environment where all target packages are installed.

## Scan source code

[`scan_usage()`](https://ascribe.visruth.com/reference/scan_usage.md)
inspects `.R`, `.Rmd`, and `.qmd` files for package attachments
([`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html)) and function calls
(`pkg::fun()` or unqualified calls resolved by attachment order).

``` r

project <- tempfile("stan-project-")
dir.create(project)
script <- file.path(project, "analysis.R")

writeLines(
  c(
    "library(cmdstanr)",
    "library(posterior)",
    "fit <- cmdstan_model('model.stan')$sample()",
    "draws <- fit$draws()",
    "summarise_draws(draws)",
    "bayesplot::mcmc_hist(draws)"
  ),
  script
)

usage <- scan_usage(
  path = project,
  allowed_packages = universe$packages,
  export_index = universe$export_index,
  origin_map = universe$origin_map,
  strict = TRUE,
  quiet = TRUE
)

usage
unlink(project, recursive = TRUE)
```

`strict = TRUE` warns on and omits calls that cannot be resolved
unambiguously.

## Generate citations

[`cite_usage()`](https://ascribe.visruth.com/reference/cite_usage.md)
formats BibTeX entries for detected packages and functions.

``` r

citations <- cite_usage(usage)
writeLines(citations)
```

Override default package or function citations with `package_citations`
and `function_citations`.

## Save scanner data in sysdata.rda

[`generate_universe_sysdata()`](https://ascribe.visruth.com/reference/generate_universe_sysdata.md)
precompiles universe data into `R/sysdata.rda` for downstream package
distribution.

``` r

# data-raw/sysdata.R
generate_universe_sysdata(
  packages = c("cmdstanr", "posterior", "bayesplot", "rstan"),
  prefix = "stan",
  file = "R/sysdata.rda"
)
```

This saves `.stan_pkgs`, `.stan_exports`, `.stan_export_index`,
`.stan_origin_map`, and `.stan_pkg_versions` for fast loading without
runtime package inspection.
