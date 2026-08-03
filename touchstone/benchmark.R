pkgload::load_all(".", quiet = TRUE)

candidate_pkgs <- c(
  "ggplot2",
  "dplyr",
  "loo",
  "brms",
  "posterior",
  "bayesplot",
  "rstan",
  "stats",
  "utils",
  "graphics",
  "grDevices",
  "methods",
  "grid",
  "tools"
)
pkgs <- candidate_pkgs[vapply(
  candidate_pkgs,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]
universe <- build_universe_data(pkgs)

repos <- c("loo", "brms", "purrr", "posterior", "cmdstanr", "stanflow")
repo_paths <- file.path("C:/Users/visru/Documents/Github", repos)

for (path in repo_paths) {
  invisible(scan_usage(
    path = path,
    allowed_packages = universe$packages,
    export_index = universe$export_index,
    origin_map = universe$origin_map,
    strict = FALSE
  ))
}
