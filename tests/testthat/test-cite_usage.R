test_that("cite_usage builds citations from a package universe", {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path), add = TRUE)
  writeLines("stats::median(1:3)", path)

  universe <- build_universe_data("stats")
  usage <- scan_usage(
    path,
    universe$packages,
    universe$export_index,
    universe$origin_map,
    ignore_unqualified_functions = character(),
    quiet = TRUE
  )
  citations <- cite_usage(
    usage,
    package_citations = list(
      stats = utils::bibentry(
        bibtype = "Manual",
        key = "stats-package",
        title = "Stats package",
        author = "A",
        year = "2026"
      )
    ),
    function_citations = list(
      "stats::median" = utils::bibentry(
        bibtype = "Manual",
        key = "stats-median",
        title = "Median",
        author = "B",
        year = "2026"
      )
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
    package_citations = list(stats = utils::citation("stats")),
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
        "stats",
        list(),
        character(),
        quiet = TRUE
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
  expect_length(citations, 2L)
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
    package_citations = list(base = custom_base),
    format = "bibentry"
  )
  bibtex <- utils::toBibtex(citations)
  expect_true(any(grepl("Custom Base", bibtex, fixed = TRUE)))
})
