test_that("collect_pkg_funs collects functions from a package", {
  funs <- collect_pkg_funs("stats")
  expect_type(funs, "character")
  expect_true("median" %in% funs)
  expect_true("filter" %in% funs)
})

test_that("collect_r6_methods returns empty vector when no R6 classes exist", {
  res <- collect_r6_methods("stats")
  expect_type(res, "character")
})

test_that("collect_r6_methods finds exported and internal R6 methods", {
  methods <- collect_r6_methods("testthat")
  expect_true("public_fun" %in% methods)
  expect_false(anyNA(methods))
  expect_true(all(nzchar(methods)))
})

test_that("resolve_origin identifies origin of re-exported functions and non-functions", {
  # Base/stats function origin
  origin <- resolve_origin("stats", "median")
  expect_equal(origin, "stats")

  expect_equal(resolve_origin("testthat", "expect_equal"), "testthat")
  expect_true(is.na(resolve_origin("base", "sum")))

  # Non-function or non-existent returns NA
  expect_true(is.na(resolve_origin("stats", "non_existent_function_12345")))
  expect_true(is.na(resolve_origin("datasets", "iris"))) # dataset, not function
  expect_true(is.na(resolve_origin("nonexistent_pkg_xyz_999", "foo")))
})

test_that("build_export_index creates inverted mapping", {
  exports <- list(
    pkgA = c("foo", "bar"),
    pkgB = c("foo", "baz")
  )
  idx <- build_export_index(exports)
  expect_equal(idx$foo, c("pkgA", "pkgB"))
  expect_equal(idx$bar, "pkgA")
  expect_equal(idx$baz, "pkgB")
})

test_that("build_origin_map creates pkg::fun keys mapping to origin", {
  exports <- list(
    stats = c("median", "filter"),
    nonexistent = "foo",
    datasets = "iris"
  )
  omap <- build_origin_map(exports)
  expect_equal(omap[["stats::median"]], "stats")
  expect_equal(omap[["stats::filter"]], "stats")
  expect_equal(omap[["nonexistent::foo"]], "nonexistent")
  expect_equal(omap[["datasets::iris"]], "datasets")
})

test_that("collect_pkg_funs, resolve_origin, and build_origin_map handle pure re-exports", {
  tmp_lib <- tempfile()
  dir.create(tmp_lib)

  up_dir <- tempfile()
  dir.create(file.path(up_dir, "R"), recursive = TRUE)
  writeLines("foo <- function() \"upstream\"", file.path(up_dir, "R", "foo.R"))
  writeLines(
    c(
      "Package: ascribetestupstream",
      "Version: 0.0.1",
      "Title: Test Helper for Ascribe Re-export Handling",
      "Description: Minimal upstream package for testing re-export detection.",
      "Author: Ascribe Tests",
      "Maintainer: Ascribe Tests <tests@ascribe.test>",
      "License: MIT"
    ),
    file.path(up_dir, "DESCRIPTION")
  )
  writeLines("export(foo)", file.path(up_dir, "NAMESPACE"))

  down_dir <- tempfile()
  dir.create(file.path(down_dir, "R"), recursive = TRUE)
  writeLines("NULL", file.path(down_dir, "R", "dummy.R"))
  writeLines(
    c(
      "Package: ascribetestdownstream",
      "Version: 0.0.1",
      "Title: Test Helper for Ascribe Re-export Handling",
      "Description: Minimal downstream package for testing re-export detection.",
      "Author: Ascribe Tests",
      "Maintainer: Ascribe Tests <tests@ascribe.test>",
      "License: MIT",
      "Imports: ascribetestupstream"
    ),
    file.path(down_dir, "DESCRIPTION")
  )
  writeLines(
    c("importFrom(ascribetestupstream, foo)", "export(foo)"),
    file.path(down_dir, "NAMESPACE")
  )

  old_lib <- .libPaths()
  on.exit(
    {
      if (isNamespaceLoaded("ascribetestdownstream")) {
        unloadNamespace("ascribetestdownstream")
      }
      if (isNamespaceLoaded("ascribetestupstream")) {
        unloadNamespace("ascribetestupstream")
      }
      .libPaths(old_lib)
      unlink(c(tmp_lib, up_dir, down_dir), recursive = TRUE)
    },
    add = TRUE
  )

  .libPaths(c(tmp_lib, old_lib))
  utils::install.packages(
    up_dir,
    repos = NULL,
    type = "source",
    lib = tmp_lib,
    quiet = TRUE
  )
  utils::install.packages(
    down_dir,
    repos = NULL,
    type = "source",
    lib = tmp_lib,
    quiet = TRUE
  )

  ns <- asNamespace("ascribetestdownstream")
  expect_false(exists("foo", envir = ns, inherits = FALSE))

  expect_true("foo" %in% collect_pkg_funs("ascribetestdownstream"))
  expect_equal(
    resolve_origin("ascribetestdownstream", "foo"),
    "ascribetestupstream"
  )

  exports <- list(
    ascribetestupstream = collect_pkg_funs("ascribetestupstream"),
    ascribetestdownstream = collect_pkg_funs("ascribetestdownstream")
  )
  omap <- build_origin_map(exports)
  expect_equal(omap[["ascribetestdownstream::foo"]], "ascribetestupstream")
})
