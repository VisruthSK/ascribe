test_that("build_universe_data builds complete scanner data structure", {
  pkgs <- c("stats", "utils")
  data <- build_universe_data(pkgs)

  expect_s3_class(data, "ascribe_universe")
  expect_named(
    data,
    c(
      "packages",
      "exports",
      "export_index",
      "origin_map",
      "pkg_versions",
      "metapackages",
      "package_citations",
      "function_citations",
      "resolver_index"
    )
  )
  expect_equal(data$packages, pkgs)
  expect_named(data$exports, pkgs)
  expect_true("median" %in% data$exports$stats)
  expect_true("head" %in% data$exports$utils)
  expect_true("median" %in% names(data$export_index))
  expect_true(exists("stats::median", envir = data$origin_map))
  expect_named(data$pkg_versions, pkgs)
  expect_type(data$metapackages, "environment")
  expect_type(data$package_citations, "environment")
  expect_type(data$function_citations, "environment")
  expect_type(data$resolver_index, "list")

  print_data <- structure(
    list(
      packages = c("pkgA", "pkgB"),
      exports = list(pkgA = c("foo", "bar"), pkgB = "baz"),
      export_index = list(foo = "pkgA", bar = "pkgA", baz = "pkgB"),
      origin_map = new.env(parent = emptyenv()),
      pkg_versions = c(pkgA = "1.0", pkgB = "2.0"),
      metapackages = new.env(parent = emptyenv()),
      package_citations = new.env(parent = emptyenv()),
      function_citations = new.env(parent = emptyenv()),
      resolver_index = list()
    ),
    class = "ascribe_universe"
  )
  old_handler <- options(cli.default_handler = NULL)
  on.exit(options(old_handler), add = TRUE)
  expect_snapshot(print(print_data))
})

test_that("build_universe_data aborts if package is missing", {
  expect_error(
    build_universe_data(c("stats", "nonexistent_package_xyz_99")),
    "not installed"
  )
})

test_that("build_universe_data drops invalid package names", {
  expect_warning(
    data <- build_universe_data(c("stats", "stats", "", NA_character_)),
    "Dropping"
  )
  expect_identical(data$packages, "stats")
})

test_that("build_universe_data warns and drops invalid metapackage members", {
  expect_warning(
    expect_warning(
      data <- build_universe_data(
        "stats",
        metapackages = list(meta = c("stats", "stats", "missing"))
      ),
      "unknown packages"
    ),
    "duplicate members"
  )
  expect_identical(data$metapackages$meta, "stats")

  expect_warning(
    data <- build_universe_data(
      "stats",
      metapackages = list(meta = 1)
    ),
    "members must be character"
  )
  expect_false(exists("meta", envir = data$metapackages, inherits = FALSE))
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
    c(
      "packages",
      "exports",
      "export_index",
      "origin_map",
      "pkg_versions",
      "metapackages",
      "package_citations",
      "function_citations",
      "resolver_index"
    )
  )
  expect_true(file.exists(tmp_file))

  env <- new.env(parent = emptyenv())
  load(tmp_file, envir = env)

  expect_true(exists(".test_universe", envir = env))
  expect_true(exists(".stdlib_funs", envir = env))
  expect_true(exists(".scan_skip_dirs", envir = env))
  expect_true(exists(".test_extra", envir = env))
})
