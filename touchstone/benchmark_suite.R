library(bench)

main_dir <- file.path(tempdir(), "ascribe-main")
system2("git", c("worktree", "prune"), stdout = FALSE, stderr = FALSE)
if (!dir.exists(main_dir)) {
  system2(
    "git",
    c("worktree", "add", "-f", main_dir, "main"),
    stdout = FALSE,
    stderr = FALSE
  )
}

temp_lib <- file.path(tempdir(), "R_main_lib")
dir.create(temp_lib, recursive = TRUE, showWarnings = FALSE)
withr::with_libpaths(
  c(temp_lib, .libPaths()),
  devtools::install(main_dir, quiet = TRUE, upgrade = FALSE),
  action = "prefix"
)

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
base_sources <- file.path("C:", "Users", "visru", "Documents", "Github")
repo_paths <- setNames(file.path(base_sources, repos), repos)

main_scan <- function(...) {
  withr::with_libpaths(c(temp_lib, .libPaths()), ascribe::scan_usage(...))
}

run_suite <- function(use_knitr = FALSE, iterations = 15) {
  cat(sprintf("\n### use_knitr = %s\n\n", toupper(as.character(use_knitr))))

  df_rows <- list()

  for (repo in repos) {
    path <- repo_paths[[repo]]
    bm <- bench::mark(
      main = main_scan(
        path = path,
        allowed_packages = universe$packages,
        export_index = universe$export_index,
        origin_map = universe$origin_map,
        strict = FALSE,
        quiet = TRUE,
        use_knitr = use_knitr
      ),
      current = ascribe::scan_usage(
        path = path,
        allowed_packages = universe$packages,
        export_index = universe$export_index,
        origin_map = universe$origin_map,
        strict = FALSE,
        quiet = TRUE,
        use_knitr = use_knitr
      ),
      iterations = iterations,
      check = FALSE,
      memory = TRUE
    )

    expr_names <- as.character(bm$expression)
    main_idx <- which(expr_names == "main")
    curr_idx <- which(expr_names == "current")

    main_ms <- as.numeric(bm$median[main_idx], units = "secs") * 1000
    curr_ms <- as.numeric(bm$median[curr_idx], units = "secs") * 1000
    time_pct <- (curr_ms - main_ms) / main_ms * 100

    main_mib <- as.numeric(bm$mem_alloc[main_idx]) / (1024 * 1024)
    curr_mib <- as.numeric(bm$mem_alloc[curr_idx]) / (1024 * 1024)
    mem_pct <- (curr_mib - main_mib) / main_mib * 100

    df_rows[[repo]] <- data.frame(
      repo = repo,
      main_ms = main_ms,
      current_ms = curr_ms,
      time_pct = time_pct,
      main_mib = main_mib,
      current_mib = curr_mib,
      mem_pct = mem_pct,
      stringsAsFactors = FALSE
    )
  }

  df <- do.call(rbind, df_rows)

  cat(
    "| repo | main ms | current ms | time | main MiB | current MiB | memory |\n"
  )
  cat("| :--- | :--- | :--- | :--- | :--- | :--- | :--- |\n")
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    cat(sprintf(
      "| %s | %.1f | %.1f | %.1f%% | %.1f | %.1f | %.1f%% |\n",
      row$repo,
      row$main_ms,
      row$current_ms,
      row$time_pct,
      row$main_mib,
      row$current_mib,
      row$mem_pct
    ))
  }
  cat("\n")
}

run_suite(use_knitr = FALSE, iterations = 15)
run_suite(use_knitr = TRUE, iterations = 15)
