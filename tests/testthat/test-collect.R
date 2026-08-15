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

test_that("collect_r6_methods survives erroring namespace bindings", {
  skip_if_not_installed("R6")
  tmp_lib <- tempfile()
  dir.create(tmp_lib)

  pkg_dir <- tempfile()
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  writeLines(
    c(
      ".onLoad <- function(libname, pkgname) {",
      "  ns <- asNamespace(pkgname)",
      "  makeActiveBinding(\"broken\", function() stop(\"boom\"), env = ns)",
      "}",
      paste0(
        "SomeClass <- R6::R6Class(\"SomeClass\", public = list(",
        "method = function() \"upstream\"))"
      )
    ),
    file.path(pkg_dir, "R", "code.R")
  )
  writeLines(
    c(
      "Package: ascribetestbroken",
      "Version: 0.0.1",
      "Title: Test Helper for Ascribe Broken Namespace Handling",
      "Description: Minimal package with an erroring active binding.",
      "Author: Ascribe Tests",
      "Maintainer: Ascribe Tests <tests@ascribe.test>",
      "License: MIT",
      "Imports: R6"
    ),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines("export(SomeClass)", file.path(pkg_dir, "NAMESPACE"))

  old_lib <- .libPaths()
  on.exit(
    {
      if (isNamespaceLoaded("ascribetestbroken")) {
        unloadNamespace("ascribetestbroken")
      }
      .libPaths(old_lib)
      unlink(c(tmp_lib, pkg_dir), recursive = TRUE)
    },
    add = TRUE
  )

  .libPaths(c(tmp_lib, old_lib))
  utils::install.packages(
    pkg_dir,
    repos = NULL,
    type = "source",
    lib = tmp_lib,
    INSTALL_opts = "--no-test-load",
    quiet = TRUE
  )

  expect_true("method" %in% collect_r6_methods("ascribetestbroken"))
  expect_true("method" %in% collect_pkg_funs("ascribetestbroken"))
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


test_that("collect_pkg_funs finds methods of pure re-exported R6 classes", {
  skip_if_not_installed("R6")
  tmp_lib <- tempfile()
  dir.create(tmp_lib)

  up_dir <- tempfile()
  dir.create(file.path(up_dir, "R"), recursive = TRUE)
  writeLines(
    paste0(
      "SomeClass <- R6::R6Class(\"SomeClass\", public = list(",
      "method = function() \"upstream\"))"
    ),
    file.path(up_dir, "R", "some_class.R")
  )
  writeLines(
    c(
      "Package: ascribetestupstreamr6",
      "Version: 0.0.1",
      "Title: Test Helper for Ascribe R6 Re-export Handling",
      "Description: Minimal upstream package for testing R6 re-export detection.",
      "Author: Ascribe Tests",
      "Maintainer: Ascribe Tests <tests@ascribe.test>",
      "License: MIT",
      "Imports: R6"
    ),
    file.path(up_dir, "DESCRIPTION")
  )
  writeLines("export(SomeClass)", file.path(up_dir, "NAMESPACE"))

  down_dir <- tempfile()
  dir.create(file.path(down_dir, "R"), recursive = TRUE)
  writeLines("NULL", file.path(down_dir, "R", "dummy.R"))
  writeLines(
    c(
      "Package: ascribetestdownstreamr6",
      "Version: 0.0.1",
      "Title: Test Helper for Ascribe R6 Re-export Handling",
      "Description: Minimal downstream package for testing R6 re-export detection.",
      "Author: Ascribe Tests",
      "Maintainer: Ascribe Tests <tests@ascribe.test>",
      "License: MIT",
      "Imports: ascribetestupstreamr6"
    ),
    file.path(down_dir, "DESCRIPTION")
  )
  writeLines(
    c("importFrom(ascribetestupstreamr6, SomeClass)", "export(SomeClass)"),
    file.path(down_dir, "NAMESPACE")
  )

  old_lib <- .libPaths()
  on.exit(
    {
      if (isNamespaceLoaded("ascribetestdownstreamr6")) {
        unloadNamespace("ascribetestdownstreamr6")
      }
      if (isNamespaceLoaded("ascribetestupstreamr6")) {
        unloadNamespace("ascribetestupstreamr6")
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
    INSTALL_opts = "--no-staged-install",
    quiet = TRUE
  )
  utils::install.packages(
    down_dir,
    repos = NULL,
    type = "source",
    lib = tmp_lib,
    INSTALL_opts = "--no-staged-install",
    quiet = TRUE
  )

  ns <- asNamespace("ascribetestdownstreamr6")
  expect_false(exists("SomeClass", envir = ns, inherits = FALSE))

  expect_true("method" %in% collect_r6_methods("ascribetestdownstreamr6"))
  expect_true("method" %in% collect_pkg_funs("ascribetestdownstreamr6"))
})
