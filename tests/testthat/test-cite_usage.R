test_that("cite_usage builds citations from a package universe", {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path), add = TRUE)
  writeLines("stats::median(1:3)", path)

  universe <- build_universe_data("stats")
  usage <- scan_usage(
    path,
    universe,
    ignore_unqualified_functions = character()
  )
  citations <- cite_usage(
    usage,
    package_citations = list2env(
      list(
        stats = utils::bibentry(
          bibtype = "Manual",
          key = "stats-package",
          title = "Stats package",
          author = "A",
          year = "2026"
        )
      ),
      parent = emptyenv()
    ),
    function_citations = list2env(
      list(
        "stats::median" = utils::bibentry(
          bibtype = "Manual",
          key = "stats-median",
          title = "Median",
          author = "B",
          year = "2026"
        )
      ),
      parent = emptyenv()
    ),
    package_citation = function(...) fail("Unexpected package citation lookup"),
    format = "bibentry"
  )

  expect_s3_class(citations, "bibentry")
  bibtex <- utils::toBibtex(citations)
  expect_true(any(grepl("Stats package", bibtex, fixed = TRUE)))
  expect_true(any(grepl("Median", bibtex, fixed = TRUE)))

  bibtex <- cite_usage(
    usage,
    package_citations = list2env(
      list(stats = utils::citation("stats")),
      parent = emptyenv()
    ),
    format = "bibtex"
  )
  expect_type(bibtex, "character")
})

test_that("cite_usage can return BibTeX and report no citations", {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path), add = TRUE)
  writeLines("1 + 1", path)

  expect_identical(
    cite_usage(
      scan_usage(
        path,
        test_universe("stats")
      ),
      format = "bibtex"
    ),
    character()
  )
})

test_that("cite_usage defaults to package citations", {
  citations <- cite_usage(
    structure(
      list(packages = "stats", functions = character()),
      class = "scan_usage"
    ),
    format = "bibentry"
  )

  expect_true(any(grepl(
    "R Core Team",
    utils::toBibtex(citations),
    fixed = TRUE
  )))
})

test_that("cite_usage does not duplicate base citation when base is in packages", {
  citations <- cite_usage(
    structure(
      list(packages = c("stats", "base"), functions = character()),
      class = "scan_usage"
    ),
    format = "bibentry"
  )
  expect_length(citations, 1L)
})

test_that("cite_usage deduplicates citations for scanned usage", {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path), add = TRUE)
  writeLines("stats::median(1:3)", path)

  universe <- build_universe_data(c("stats", "tools"))
  usage <- scan_usage(path, universe)
  citations <- cite_usage(usage, format = "bibentry")
  expect_length(citations, 1L)
})

test_that("cite_usage uses custom base citation override when base is not in packages", {
  custom_base <- utils::bibentry(
    bibtype = "Manual",
    key = "custom-base",
    title = "Custom Base",
    author = "R",
    year = "2026"
  )
  citations <- cite_usage(
    structure(
      list(packages = "stats", functions = character()),
      class = "scan_usage"
    ),
    package_citations = list2env(list(base = custom_base), parent = emptyenv()),
    format = "bibentry"
  )
  bibtex <- utils::toBibtex(citations)
  expect_true(any(grepl("Custom Base", bibtex, fixed = TRUE)))
})

test_that("cite_usage handles environment-based citations and fallback branches", {
  pkg_env <- list2env(
    list(
      stats = utils::bibentry(
        bibtype = "Manual",
        key = "stats-env",
        title = "Env Stats",
        author = "A",
        year = "2026"
      ),
      base = utils::bibentry(
        bibtype = "Manual",
        key = "base-env",
        title = "Env Base",
        author = "A",
        year = "2026"
      )
    ),
    parent = emptyenv()
  )
  fun_env <- list2env(
    list(
      "stats::median" = utils::bibentry(
        bibtype = "Manual",
        key = "median-env",
        title = "Env Median",
        author = "B",
        year = "2026"
      )
    ),
    parent = emptyenv()
  )
  usage <- structure(
    list(packages = "stats", functions = "stats::median"),
    class = "scan_usage"
  )
  cits <- cite_usage(
    usage,
    package_citations = pkg_env,
    function_citations = fun_env,
    format = "bibentry"
  )
  expect_length(cits, 3L)

  cits_bibtex <- cite_usage(
    usage,
    package_citations = pkg_env,
    function_citations = fun_env,
    format = "bibtex"
  )
  expect_type(cits_bibtex, "character")
  expect_true(any(grepl("Env Stats", cits_bibtex, fixed = TRUE)))

  # Empty usage or NULL package/function citation branches
  no_usage <- structure(
    list(packages = character(), functions = character()),
    class = "scan_usage"
  )
  expect_identical(
    cite_usage(
      no_usage,
      package_citations = new.env(parent = emptyenv()),
      function_citations = new.env(parent = emptyenv()),
      format = "bibentry"
    ),
    character()
  )
})
