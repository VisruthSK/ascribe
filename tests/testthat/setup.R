options(cli.default_handler = function(msg) invisible(NULL))

# Single fixture installation helper - accepts one or more fixture names
with_fixtures <- function(fixture_names, code) {
  skip_if_not_installed("R6")

  tmp_lib <- tempfile()
  dir.create(tmp_lib)
  old_lib <- .libPaths()

  on.exit(
    {
      # Unload fixture namespaces in reverse order
      for (pkg in rev(fixture_names)) {
        if (isNamespaceLoaded(pkg)) {
          unloadNamespace(pkg)
        }
      }
      .libPaths(old_lib)
      unlink(tmp_lib, recursive = TRUE)
    },
    add = TRUE
  )

  .libPaths(c(tmp_lib, old_lib))

  for (fixture_name in fixture_names) {
    fixture_dir <- testthat::test_path("fixtures", fixture_name)
    utils::install.packages(
      fixture_dir,
      repos = NULL,
      type = "source",
      lib = tmp_lib,
      INSTALL_opts = c("--no-staged-install", "--no-test-load"),
      quiet = TRUE
    )
  }

  code()
}

test_universe <- function(packages, export_index = list(), origin_map = NULL) {
  if (is.null(origin_map)) {
    origin_map <- new.env(parent = emptyenv())
  }
  list(
    packages = packages,
    export_index = export_index,
    origin_map = origin_map
  )
}
