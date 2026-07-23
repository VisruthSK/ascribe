test_that("build_universe_data builds complete scanner data structure", {
  pkgs <- c("stats", "utils")
  data <- build_universe_data(pkgs)

  expect_named(
    data,
    c("packages", "exports", "export_index", "origin_map", "pkg_versions")
  )
  expect_equal(data$packages, pkgs)
  expect_named(data$exports, pkgs)
  expect_true("median" %in% data$exports$stats)
  expect_true("head" %in% data$exports$utils)
  expect_true("median" %in% names(data$export_index))
  expect_true("stats::median" %in% names(data$origin_map))
  expect_named(data$pkg_versions, pkgs)
})

test_that("build_universe_data aborts if package is missing", {
  expect_error(
    build_universe_data(c("stats", "nonexistent_package_xyz_99")),
    "not installed"
  )
})

test_that("generate_universe_sysdata saves prefixed objects to sysdata.rda", {
  tmp_file <- tempfile(fileext = ".rda")
  on.exit(unlink(tmp_file), add = TRUE)

  extra_env <- new.env(parent = emptyenv())
  extra_env$foo <- "bar"

  res <- generate_universe_sysdata(
    packages = c("stats"),
    prefix = "test",
    extra_vars = list(.test_extra = extra_env),
    include_scanner_defaults = TRUE,
    file = tmp_file
  )

  expect_named(
    res,
    c("packages", "exports", "export_index", "origin_map", "pkg_versions")
  )
  expect_true(file.exists(tmp_file))

  env <- new.env(parent = emptyenv())
  load(tmp_file, envir = env)

  expect_true(exists(".test_pkgs", envir = env))
  expect_true(exists(".test_exports", envir = env))
  expect_true(exists(".test_export_index", envir = env))
  expect_true(exists(".test_origin_map", envir = env))
  expect_true(exists(".test_pkg_versions", envir = env))
  expect_true(exists(".stdlib_funs", envir = env))
  expect_true(exists(".scan_skip_dirs", envir = env))
  expect_true(exists(".test_extra", envir = env))
})
