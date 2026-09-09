test_that("coverage edge cases for compact helpers", {
  empty <- new.env(parent = emptyenv())
  expect_type(build_origin_map(list()), "environment")
  expect_identical(.scan_resolver_index(list(), empty), list())
  expect_null(.build_skip_patterns(character()))
  expect_true(.any_pattern_matches(NULL, ""))
  expect_false(.any_pattern_matches("\\bfoo\\b", "bar"))

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp)
  expect_identical(.read_file_lf(tmp), "")
  expect_identical(.extract_code(tmp), "")
  expect_error(.extract_code(sub("R$", "txt", tmp)), "Unsupported")

  universe <- test_universe("stats")
  expect_identical(
    .scan_prepare(universe, character())$skip_patterns,
    .build_skip_patterns("stats")
  )
  expect_identical(
    .scan_finish(
      list(list(
        pkgs = character(),
        keys = character(),
        ambiguous = character()
      )),
      FALSE,
      empty,
      empty
    )$packages,
    character()
  )
})

test_that("branch coverage for universe normalization and status", {
  expect_error(build_universe_data(1), "character vector")
  expect_error(
    build_universe_data("stats", metapackages = 1),
    "named list or environment"
  )
  expect_warning(
    data <- build_universe_data(
      "stats",
      metapackages = setNames(list("stats"), "")
    ),
    "empty name"
  )
  expect_length(ls(data$metapackages), 0L)
  expect_warning(
    data <- build_universe_data("stats", metapackages = list(meta = 1)),
    "members must be character"
  )
  expect_false(exists("meta", data$metapackages, inherits = FALSE))
  expect_error(
    build_universe_data("stats", package_citations = 1),
    "named list or environment"
  )
  expect_error(
    build_universe_data("stats", function_citations = setNames(list(1), "")),
    "names must be non-empty"
  )
  citation_env <- new.env(parent = emptyenv())
  citation_env$stats <- utils::citation("stats")
  expect_s3_class(
    build_universe_data("stats", package_citations = citation_env),
    "ascribe_universe"
  )
  meta_env <- new.env(parent = emptyenv())
  meta_env$meta <- c("stats", "stats", "missing")
  expect_warning(
    data <- build_universe_data("stats", metapackages = meta_env),
    "unknown packages"
  )

  data <- build_universe_data("stats")
  data$pkg_versions[[1L]] <- "0.0.0"
  status <- universe_status(data)
  expect_equal(status$status, "changed")
})

test_that("branch coverage for citation and scan result paths", {
  empty <- new.env(parent = emptyenv())
  usage <- structure(
    list(packages = character(), functions = character()),
    class = "scan_usage"
  )
  expect_identical(cite_usage(usage, universe = list()), character())
  citation_universe <- build_universe_data("stats")
  cited_usage <- structure(
    list(packages = "stats", functions = character()),
    class = "scan_usage"
  )
  expect_type(
    cite_usage(cited_usage, universe = citation_universe, cite_r = FALSE),
    "character"
  )

  universe <- test_universe(character())
  expect_identical(scan_code("1 + 1", universe)$packages, character())
  expect_warning(
    .scan_finish(
      list(list(pkgs = character(), keys = character(), ambiguous = "foo")),
      FALSE,
      empty,
      empty
    ),
    "Ambiguous functions"
  )
  expect_error(
    .scan_finish(
      list(list(pkgs = character(), keys = character(), ambiguous = "foo")),
      TRUE,
      empty,
      empty
    ),
    "Ambiguous functions"
  )
})

test_that("branch coverage for scan progress and resolver helpers", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("stats::median(1:3)", tmp)
  scan_usage(tmp, build_universe_data("stats"), progress = TRUE)
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  file.copy(tmp, file.path(dir, "x.R"))
  scan_usage(dir, build_universe_data("stats"), progress = TRUE)
  expect_equal(.resolve_origin_ns(NULL, "x"), NA_character_)
  active_ns <- new.env(parent = emptyenv())
  makeActiveBinding("x", function() 1, active_ns)
  expect_equal(.resolve_origin_ns(active_ns, "x"), NA_character_)
  expect_null(.scan_resolver_index(list(foo = character()), NULL)$foo)
  expect_equal(
    ascribe:::.scan_resolver_index(list(foo = "a"), NULL)$foo$provider,
    "a"
  )
  expect_equal(
    ascribe:::.scan_resolver_index(list(foo = c("a", "b")), NULL)$foo$provider,
    c("a", "b")
  )
  expect_identical(ascribe:::.scan_resolver_index(NULL, NULL), list())
  expect_equal(
    .scan_resolver_index(list(foo = c("a", "b")), NULL)$foo$provider,
    c("a", "b")
  )

  no_exports <- test_universe("stats", export_index = NULL)
  no_exports$export_index <- NULL
  expect_silent(.scan_prepare(no_exports, character()))
  expect_identical(
    scan_code("1 + 1", build_universe_data("stats"))$packages,
    character()
  )
})
