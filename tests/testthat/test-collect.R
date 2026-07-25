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
