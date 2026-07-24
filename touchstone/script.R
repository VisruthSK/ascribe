library(touchstone)

# GHA has already resolved dependency versions. Do not let Touchstone upgrade them.
local({
  ns <- asNamespace("touchstone")
  unlockBinding("install_missing_deps", ns)
  assign(
    "install_missing_deps",
    \(path_pkg, quiet = FALSE) {
      remotes::install_deps(pkgdir = path_pkg, upgrade = "never", quiet = quiet)
    },
    envir = ns
  )
  lockBinding("install_missing_deps", ns)
})

branch_install()

base_dir <- file.path("touchstone", "sources")
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

n <- 30
repos <- list(
  list(url = "https://github.com/tidyverse/ggplot2.git", ref = "v4.0.2"),
  list(
    url = "https://github.com/ASKurz/Statistical_Rethinking_with_brms_ggplot2_and_the_tidyverse.git",
    ref = "1.4.0"
  ),
  list(url = "https://github.com/stan-dev/loo.git", ref = "v2.9.0")
)

clone_repo <- function(url, ref, dir) {
  repo_path <- file.path(base_dir, dir)
  if (!dir.exists(repo_path)) {
    message("Cloning ", dir, " at ", ref)
    system2("git", c("clone", "--depth", "1", "--branch", ref, url, repo_path))
  }
}

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

expr_before <- quote({
  library(ascribe)
  if (requireNamespace("knitr", quietly = TRUE)) {
    library(knitr)
  }
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
})

for (repo in repos) {
  dir <- sub("\\.git$", "", basename(repo$url))
  repo_path <- file.path(base_dir, dir)
  clone_repo(repo$url, repo$ref, dir)

  if (dir == "loo") {
    benchmark_run(
      expr_before_benchmark = !!expr_before,
      n = n,
      loo := scan_usage(
        path = !!repo_path,
        allowed_packages = universe$packages,
        export_index = universe$export_index,
        origin_map = universe$origin_map,
        strict = FALSE,
        quiet = TRUE
      )
    )
    benchmark_run(
      expr_before_benchmark = !!expr_before,
      n = n,
      loo_knitr := scan_usage(
        path = !!repo_path,
        allowed_packages = universe$packages,
        export_index = universe$export_index,
        origin_map = universe$origin_map,
        strict = FALSE,
        quiet = TRUE,
        use_knitr = TRUE
      )
    )
  } else {
    benchmark_run(
      expr_before_benchmark = !!expr_before,
      n = n,
      !!dir := scan_usage(
        path = !!repo_path,
        allowed_packages = universe$packages,
        export_index = universe$export_index,
        origin_map = universe$origin_map,
        strict = FALSE,
        quiet = TRUE
      )
    )
  }
}

benchmark_analyze()
