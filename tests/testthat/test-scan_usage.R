test_that("stdlib_funs and scan_skip_dirs return precomputed vectors", {
  sf <- stdlib_funs()
  expect_type(sf, "character")
  expect_true("mean" %in% sf)

  ssd <- scan_skip_dirs()
  expect_type(ssd, "character")
  expect_true("renv" %in% ssd)
})

test_that("scan_usage accepts a package universe", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("stats::median(1:3)", tmp)

  universe <- build_universe_data("stats")

  expect_equal(
    scan_usage(tmp, universe),
    structure(
      list(
        packages = "stats",
        functions = "stats::median",
        ambiguous = character()
      ),
      class = "scan_usage"
    )
  )
})

test_that("scan_usage detects library attachments and namespaced calls", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "library(stats)",
      "requireNamespace('utils')",
      "filter(1:10, rep(1, 3))",
      "utils::head(letters)"
    ),
    tmp
  )

  res <- scan_usage(
    path = tmp,
    universe = test_universe(
      c("stats", "utils"),
      list(filter = "stats", head = "utils"),
      list2env(
        list("stats::filter" = "stats", "utils::head" = "utils"),
        parent = emptyenv()
      )
    ),
    ignore_unqualified_functions = character()
  )

  expect_s3_class(res, "scan_usage")
  expect_true("stats" %in% res$packages)
  expect_true("utils" %in% res$packages)
  expect_true("stats::filter" %in% res$functions)
  expect_true("utils::head" %in% res$functions)
  expect_equal(res$ambiguous, character())
})

test_that("scan_usage parses Rmd and Qmd code chunks natively", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "---",
      "title: Test",
      "---",
      "```{r}",
      "library(stats)",
      "stats::median(1:5)",
      "```",
      "~~~{r}",
      "stats::filter(1:5, 1)",
      "~~~",
      "```python",
      "print('ignored')",
      "```"
    ),
    tmp
  )

  res <- scan_usage(
    path = tmp,
    universe = test_universe(
      "stats",
      list(median = "stats", filter = "stats"),
      list2env(
        list("stats::median" = "stats", "stats::filter" = "stats"),
        parent = emptyenv()
      )
    )
  )

  expect_true("stats" %in% res$packages)
  expect_true("stats::median" %in% res$functions)
  expect_true("stats::filter" %in% res$functions)
})

test_that("scan_usage returns empty for Rmd without code fences", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("# Header", "Just prose mentioning stats."), tmp)

  res <- scan_usage(
    path = tmp,
    universe = test_universe(
      "stats",
      list(median = "stats"),
      list2env(list("stats::median" = "stats"), parent = emptyenv())
    )
  )

  expect_equal(res$packages, character())
  expect_equal(res$functions, character())
})

test_that("scan_usage works with use_knitr = TRUE", {
  skip_if_not_installed("knitr")
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "```{r}",
      "library(stats)",
      "stats::median(1:5)",
      "```"
    ),
    tmp
  )

  res <- scan_usage(
    path = tmp,
    universe = test_universe(
      "stats",
      list(median = "stats"),
      list2env(list("stats::median" = "stats"), parent = emptyenv())
    ),
    use_knitr = TRUE
  )

  expect_true("stats" %in% res$packages)
  expect_true("stats::median" %in% res$functions)
})

test_that("use_knitr = TRUE resolves child documents", {
  skip_if_not_installed("knitr")
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  writeLines(
    c("```{r}", "library(stats)", "median(1:10)", "```"),
    file.path(dir, "child.Rmd")
  )
  parent <- file.path(dir, "parent.Rmd")
  writeLines(
    c(
      "```{r}",
      "library(utils)",
      "head(letters)",
      "```",
      "",
      "```{r, child=\"child.Rmd\"}",
      "```"
    ),
    parent
  )

  scan <- function(use_knitr) {
    scan_usage(
      path = parent,
      universe = test_universe(
        c("stats", "utils"),
        list(median = "stats", head = "utils"),
        list2env(
          list("stats::median" = "stats", "utils::head" = "utils"),
          parent = emptyenv()
        )
      ),
      ignore_unqualified_functions = character(),
      use_knitr = use_knitr
    )
  }

  knitted <- scan(TRUE)
  expect_equal(knitted$packages, c("stats", "utils"))
  expect_equal(knitted$functions, c("stats::median", "utils::head"))

  # the in-house parser only sees code written in the file itself
  in_house <- scan(FALSE)
  expect_equal(in_house$packages, "utils")
  expect_equal(in_house$functions, "utils::head")
})

test_that("use_knitr = TRUE resolves children of a file that names no package", {
  skip_if_not_installed("knitr")
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  writeLines(
    c("```{r}", "library(stats)", "median(1:10)", "```"),
    file.path(dir, "child.Rmd")
  )
  parent <- file.path(dir, "parent.Rmd")
  writeLines(c("```{r, child=\"child.Rmd\"}", "```"), parent)

  res <- scan_usage(
    path = parent,
    universe = test_universe(
      "stats",
      list(median = "stats"),
      list2env(list("stats::median" = "stats"), parent = emptyenv())
    ),
    ignore_unqualified_functions = character(),
    use_knitr = TRUE
  )

  expect_equal(res$packages, "stats")
  expect_equal(res$functions, "stats::median")
})

test_that("scan_usage handles metapackages correctly", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "library(meta_pkg)",
      "foo(1)"
    ),
    tmp
  )

  res <- scan_usage(
    path = tmp,
    universe = test_universe(
      "real_pkg",
      list(foo = "real_pkg"),
      list2env(list("real_pkg::foo" = "real_pkg"), parent = emptyenv())
    ),
    metapackages = list(meta_pkg = "real_pkg"),
    ignore_unqualified_functions = character()
  )

  expect_true("real_pkg" %in% res$packages)
  expect_true("real_pkg::foo" %in% res$functions)
})

test_that("scan_usage handles strict mode on ambiguous calls", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "ambiguous_fun(123)",
      "library(pkgA)",
      "library(pkgB)"
    ),
    tmp
  )

  # In strict mode, ambiguous call triggers error
  expect_error(
    scan_usage(
      path = tmp,
      universe = test_universe(
        c("pkgA", "pkgB"),
        list(ambiguous_fun = c("pkgA", "pkgB")),
        list2env(
          list(
            "pkgA::ambiguous_fun" = "pkgA",
            "pkgB::ambiguous_fun" = "pkgB"
          ),
          parent = emptyenv()
        )
      ),
      ignore_unqualified_functions = character(),
      strict = TRUE
    ),
    "Cannot reliably detect"
  )

  # Non-strict mode issues a warning and records ambiguous function
  expect_warning(
    res <- scan_usage(
      path = tmp,
      universe = test_universe(
        c("pkgA", "pkgB"),
        list(ambiguous_fun = c("pkgA", "pkgB")),
        list2env(
          list(
            "pkgA::ambiguous_fun" = "pkgA",
            "pkgB::ambiguous_fun" = "pkgB"
          ),
          parent = emptyenv()
        )
      ),
      ignore_unqualified_functions = character(),
      strict = FALSE
    ),
    "Cannot reliably detect"
  )
  expect_equal(res$ambiguous, "ambiguous_fun")
})

test_that("scan_usage errors on invalid inputs and path combinations", {
  tmp1 <- tempfile(fileext = ".R")
  tmp_dir <- tempfile("dir_")
  dir.create(tmp_dir)
  writeLines("1 + 1", tmp1)
  on.exit(
    {
      unlink(tmp1)
      unlink(tmp_dir, recursive = TRUE)
    },
    add = TRUE
  )

  # Mixed directory and file path error
  expect_error(
    scan_usage(
      path = c(tmp1, tmp_dir),
      universe = test_universe("stats")
    ),
    "must be a single directory or a vector of files"
  )

  # Empty directory error
  empty_dir <- tempfile("empty_dir_")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)
  expect_error(
    scan_usage(
      path = empty_dir,
      universe = test_universe("stats")
    ),
    "No files found"
  )

  # Unsupported extension error
  tmp_txt <- tempfile(fileext = ".txt")
  writeLines("library(stats)", tmp_txt)
  on.exit(unlink(tmp_txt), add = TRUE)
  expect_error(
    scan_usage(
      path = tmp_txt,
      universe = test_universe("stats")
    ),
    "Unsupported file extension"
  )
})

test_that("scan_usage skips specified directories when scanning directory", {
  tmp_dir <- tempfile("test_dir_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  renv_dir <- file.path(tmp_dir, "renv")
  dir.create(renv_dir)

  writeLines("library(stats)", file.path(renv_dir, "ignored.R"))
  writeLines("library(utils)", file.path(tmp_dir, "kept.R"))

  res <- scan_usage(
    path = tmp_dir,
    universe = test_universe(c("stats", "utils")),
    skip_dirs = "renv"
  )

  expect_true("utils" %in% res$packages)
  expect_false("stats" %in% res$packages)
})

test_that("scan_usage handles syntax errors gracefully with warning", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("library(stats)\nthis is invalid syntax {{{", tmp)

  expect_warning(
    res <- scan_usage(
      path = tmp,
      universe = test_universe("stats")
    ),
    "Failed to parse"
  )
  expect_equal(res$packages, character())
})

test_that("an unparseable chunk does not discard the rest of an Rmd", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "```{r stan-code, eval=FALSE}",
      "real normal_lpdf(vector y, real sigma) {",
      "  return 0.5 * dot_self(y);",
      "}",
      "```",
      "",
      "```{r}",
      "library(stats)",
      "median(1:10)",
      "```"
    ),
    tmp
  )

  expect_warning(
    res <- scan_usage(
      path = tmp,
      universe = test_universe(
        "stats",
        list(median = "stats"),
        list2env(list("stats::median" = "stats"), parent = emptyenv())
      ),
      ignore_unqualified_functions = character()
    ),
    "Failed to parse"
  )
  expect_equal(res$packages, "stats")
  expect_equal(res$functions, "stats::median")
})

test_that("scan_usage handles member calls, slot calls, and use() calls", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    c(
      "library(stats)",
      "obj$filter()",
      "obj@filter()",
      "((obj$filter)())",
      "use(stats, c('filter'))"
    ),
    tmp
  )

  res <- scan_usage(
    path = tmp,
    universe = test_universe(
      "stats",
      list(filter = "stats"),
      list2env(list("stats::filter" = "stats"), parent = emptyenv())
    )
  )

  expect_true("stats" %in% res$packages)
  expect_true("stats::filter" %in% res$functions)
})

test_that("scan_usage returns empty results when allowed_packages is empty or not in file", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("1 + 1", tmp)

  res <- scan_usage(
    path = tmp,
    universe = test_universe("stats")
  )
  expect_equal(res$packages, character())
  expect_equal(res$functions, character())

  res_empty_allowed <- scan_usage(
    path = tmp,
    universe = test_universe(character())
  )
  expect_equal(res_empty_allowed$packages, character())
})

test_that("full coverage tests for all scan_usage.R branches", {
  # .scan_dir_files with non-code directory
  nocode_only_dir <- tempfile("nocode_only_")
  dir.create(nocode_only_dir)
  writeLines("text only", file.path(nocode_only_dir, "file.txt"))
  on.exit(unlink(nocode_only_dir, recursive = TRUE), add = TRUE)
  expect_equal(
    .scan_dir_files(nocode_only_dir, character(0)),
    character(0)
  )

  # .scan_dir_files with nested dirs
  tmp_dir <- tempfile("cov_dir_")
  dir.create(tmp_dir)
  sub_dir <- file.path(tmp_dir, "subdir")
  dir.create(sub_dir)
  writeLines("non code content", file.path(sub_dir, "notes.txt"))
  writeLines("library(stats)", file.path(sub_dir, "script.R"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  files <- .scan_dir_files(tmp_dir, skip_dirs = character())
  expect_true(any(grepl("script.R$", files)))

  # .scan_dir_walk walking nested directory
  nested_dir <- tempfile("nested_dir_")
  dir.create(nested_dir)
  sub_dir2 <- file.path(nested_dir, "sub2")
  dir.create(sub_dir2)
  writeLines("library(stats)", file.path(sub_dir2, "script.R"))
  on.exit(unlink(nested_dir, recursive = TRUE), add = TRUE)

  res_nested <- scan_usage(
    path = nested_dir,
    universe = test_universe("stats")
  )
  expect_true("stats" %in% res_nested$packages)

  # .extract_markdown_code edge cases
  expect_equal(.extract_markdown_code(character(0)), "")
  expect_equal(.extract_markdown_code(c("no fence")), "")
  expect_equal(
    .extract_markdown_code(c("```{python}", "x = 1", "```")),
    ""
  )
  md_with_inner_fence <- c("```{r}", "x <- 1", "~~~", "```")
  expect_true(nzchar(.extract_markdown_code(md_with_inner_fence)))

  make_walker_stub <- function() {
    .make_ast_walker(
      ignore_unqualified_functions = character(),
      allowed_packages = character(),
      use_heads = .scan_use_heads,
      ignore_heads = .scan_ignore_heads,
      export_names = character(),
      metapackages = NULL
    )
  }

  # .scan_tokens with syntax error when file_path is NULL/empty
  expect_warning(
    hits <- .scan_tokens(
      "invalid syntax {{{",
      allowed_packages = character(),
      resolver_index = list(),
      metapackages = NULL,
      walker = make_walker_stub(),
      file_path = NULL
    ),
    "Failed to parse"
  )
  expect_equal(hits$pkgs, character())

  expect_warning(
    hits_empty_path <- .scan_tokens(
      "invalid syntax {{{",
      allowed_packages = character(),
      resolver_index = list(),
      metapackages = NULL,
      walker = make_walker_stub(),
      file_path = ""
    ),
    "Failed to parse"
  )
  expect_equal(hits_empty_path$pkgs, character())

  hits_no_exports <- .scan_tokens(
    "1 + 1",
    allowed_packages = character(),
    resolver_index = list(),
    metapackages = NULL,
    walker = make_walker_stub()
  )
  expect_equal(hits_no_exports$pkgs, character())

  res_idx <- .scan_resolver_index(
    list(filter = "stats"),
    list2env(list("stats::filter" = "stats"), parent = emptyenv())
  )
  walker_stats <- .make_ast_walker(
    ignore_unqualified_functions = character(),
    allowed_packages = "stats",
    use_heads = .scan_use_heads,
    ignore_heads = .scan_ignore_heads,
    export_names = "filter",
    metapackages = NULL
  )
  hits_with_resolver <- .scan_tokens(
    "library(stats)\nfilter(1)",
    allowed_packages = "stats",
    resolver_index = res_idx,
    metapackages = NULL,
    walker = walker_stats
  )
  expect_true("stats" %in% hits_with_resolver$pkgs)

  # .make_ast_walker edge cases (replaced .ast_walk after closure refactor)
  ignore <- ascribe::stdlib_funs()

  make_acc <- function() {
    acc <- new.env(parent = emptyenv())
    acc$visit_idx <- 0L
    acc$lib_pkgs <- character()
    acc$lib_visit_idx <- integer()
    acc$lib_is_attach <- logical()
    acc$ns_pkgs <- character()
    acc$ns_keys <- character()
    acc$unqual_funs <- character()
    acc$unqual_visit_idx <- integer()
    acc
  }

  walk_empty <- .make_ast_walker(
    ignore_unqualified_functions = ignore,
    allowed_packages = "stats",
    use_heads = .scan_use_heads,
    ignore_heads = .scan_ignore_heads,
    export_names = character(),
    metapackages = NULL
  )
  walk_median <- .make_ast_walker(
    ignore_unqualified_functions = ignore,
    allowed_packages = "stats",
    use_heads = .scan_use_heads,
    ignore_heads = .scan_ignore_heads,
    export_names = "median",
    metapackages = NULL
  )

  # NULL, expression, pairlist, list, atom
  expect_invisible(walk_empty(NULL, make_acc()))
  expect_invisible(walk_median(expression(stats::median(1)), make_acc()))
  expect_invisible(walk_median(list(quote(stats::median(1))), make_acc()))
  expect_invisible(walk_median(
    pairlist(a = quote(stats::median(1))),
    make_acc()
  ))
  expect_invisible(walk_median(quote(atom), make_acc()))

  # .ast_member_fun edge cases
  expect_equal(.ast_member_fun(quote((1 + 1)$foo)), "foo")
  expect_equal(.ast_member_fun(quote((obj$member))), "member")
  expect_null(.ast_member_fun(as.call(list(123))))
  expect_null(.ast_member_fun(quote((mean))))

  # .ast_get_lib_pkg with named arguments (package = ..., pkg = ...)
  expect_equal(
    .ast_get_lib_pkg(quote(library(package = "stats"))),
    "stats"
  )
  expect_equal(
    .ast_get_lib_pkg(as.call(list(as.name("library"), pkg = "stats"))),
    "stats"
  )
  expect_null(.ast_get_lib_pkg(quote(library())))

  # .ast_collect_use_funs and .ast_get_use_funs edge cases
  expect_equal(.ast_collect_use_funs(NULL, "c"), character())
  expect_equal(.ast_collect_use_funs(quote(c()), "c"), character())
  expect_equal(.ast_collect_use_funs(quote(1 + 1), "c"), character())
  expect_equal(
    .ast_get_use_funs(quote(use("stats")), "c"),
    character()
  )
  expect_equal(
    .ast_get_use_funs(
      quote(use(pkg = "stats", funs = "filter")),
      "c"
    ),
    "filter"
  )
  expect_equal(
    .ast_get_use_funs(
      quote(use(package = "stats", funs = "filter")),
      "c"
    ),
    "filter"
  )

  # .scan_resolver_index with empty provider list
  idx <- .scan_resolver_index(
    list(foo = character()),
    new.env(parent = emptyenv())
  )
  expect_null(idx$foo)
  idx_empty_origin <- .scan_resolver_index(
    list(foo = "pkgA"),
    new.env(parent = emptyenv())
  )
  expect_equal(idx_empty_origin$foo$origin, "pkgA")

  # .resolve_meta when resolver_index entry is NULL or keep is all FALSE or origin_allowed is all FALSE
  expect_null(.resolve_meta(
    "non_existent",
    list(pkg = "pkgA"),
    "pkgA",
    list()
  ))

  attached_pkgA <- list(visit_idx = 1L, pkg = "pkgA")
  meta_res1 <- .resolve_meta(
    "foo",
    attached_pkgA,
    allowed_packages = "pkgA",
    resolver_index = list(foo = list(provider = "pkgB", origin = "pkgB"))
  )
  expect_null(meta_res1)

  meta_res2 <- .resolve_meta(
    "foo",
    attached_pkgA,
    allowed_packages = "pkgA",
    resolver_index = list(
      foo = list(provider = "pkgA", origin = "disallowed_pkg")
    )
  )
  expect_null(meta_res2)

  # .resolve_calls with multiple allowed origins and fallback origin
  attached_multi <- list(visit_idx = c(1L, 2L), pkg = c("pkgA", "pkgB"))
  attached_rows <- list(pkgA = 1L, pkgB = 2L)

  resolved_calls <- .resolve_calls(
    meta = list(
      provider = c("pkgA", "pkgB"),
      origin = c("pkgA", "pkgB"),
      origin_allowed = c(TRUE, TRUE)
    ),
    attached = attached_multi,
    attached_rows = attached_rows,
    visit_idx = c(3L),
    allowed_packages = c("pkgA", "pkgB")
  )
  expect_equal(resolved_calls, "pkgB")

  resolved_calls_fallback <- .resolve_calls(
    meta = list(
      provider = c("pkgA", "pkgB"),
      origin = c("pkgA", "disallowed_origin"),
      origin_allowed = c(TRUE, TRUE)
    ),
    attached = attached_multi,
    attached_rows = attached_rows,
    visit_idx = c(3L),
    allowed_packages = c("pkgA", "pkgB")
  )
  expect_equal(resolved_calls_fallback, "pkgB")

  # .resolve_candidates edge cases
  expect_equal(
    .resolve_candidates(
      list(funs = "foo", idx = 1L),
      NULL,
      "pkgA",
      .scan_resolver_index(list(foo = "pkgA"), NULL)
    )$pkgs,
    character()
  )
  expect_equal(
    .resolve_candidates(
      list(funs = character(), idx = integer()),
      NULL,
      "pkgA",
      list()
    )$pkgs,
    character()
  )
  expect_equal(
    .resolve_candidates(
      list(funs = "foo", idx = 1L),
      data.frame(
        visit_idx = 1L,
        pkg = "pkgA",
        is_attach = TRUE,
        stringsAsFactors = FALSE
      ),
      "pkgA",
      list()
    )$pkgs,
    character()
  )

  # .resolve_candidates with multiple attached packages (triggers order())
  lib_df <- data.frame(
    visit_idx = c(2L, 1L),
    pkg = c("pkgB", "pkgA"),
    is_attach = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  unqual_data <- list(funs = "foo", idx = 3L)
  export_idx <- list(foo = c("pkgA", "pkgB"))
  origin_map <- list2env(
    list("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB"),
    parent = emptyenv()
  )

  cand_res <- .resolve_candidates(
    unqual_data,
    lib_df,
    allowed_packages = c("pkgA", "pkgB"),
    resolver_index = .scan_resolver_index(export_idx, origin_map)
  )
  expect_equal(cand_res$pkgs, "pkgB")

  testthat::with_mocked_bindings(
    requireNamespace = \(pkg, quietly) FALSE,
    .package = "ascribe",
    {
      tmp_rmd <- tempfile(fileext = ".Rmd")
      writeLines("```{r}\nlibrary(stats)\n```", tmp_rmd)
      on.exit(unlink(tmp_rmd), add = TRUE)
      expect_error(
        .extract_code(
          tmp_rmd,
          skip_pattern = "\\b(stats)\\b",
          use_knitr = TRUE
        ),
        "Package knitr is required"
      )
    }
  )

  if (requireNamespace("knitr", quietly = TRUE)) {
    tmp_rmd_knitr <- tempfile(fileext = ".Rmd")
    writeLines("```{r}\nlibrary(stats)\n```", tmp_rmd_knitr)
    on.exit(unlink(tmp_rmd_knitr), add = TRUE)
    res_knitr_code <- .extract_code(
      tmp_rmd_knitr,
      skip_pattern = "\\b(stats)\\b",
      use_knitr = TRUE
    )
    expect_match(res_knitr_code, "library(stats)", fixed = TRUE)
  }
})

test_that(".scan_tokens accepts a precomputed AST walker", {
  walker <- .make_ast_walker(
    ignore_unqualified_functions = character(),
    allowed_packages = "stats",
    use_heads = .scan_use_heads,
    ignore_heads = .scan_ignore_heads,
    export_names = "median",
    metapackages = NULL
  )

  res_idx <- .scan_resolver_index(
    list(median = "stats"),
    list2env(list("stats::median" = "stats"), parent = emptyenv())
  )

  hits <- .scan_tokens(
    "library(stats)\nmedian(1:5)",
    allowed_packages = "stats",
    resolver_index = res_idx,
    metapackages = NULL,
    walker = walker
  )

  expect_equal(hits$pkgs, c("stats", "stats"))
  expect_equal(hits$keys, "stats::median")
})

test_that(".scan_resolver_index handles empty provider list, missing origin map, and unmapped multi-provider functions", {
  idx_empty <- list(foo = character())
  res_empty <- .scan_resolver_index(idx_empty, NULL)
  expect_null(res_empty$foo)

  idx_multi <- list(single = "pkgA", multi = c("pkgA", "pkgB"))
  res_null <- .scan_resolver_index(idx_multi, NULL)
  expect_equal(res_null$single, list(provider = "pkgA", origin = "pkgA"))
  expect_equal(
    res_null$multi,
    list(provider = c("pkgA", "pkgB"), origin = c("pkgA", "pkgB"))
  )

  origin_map <- list2env(list("pkgA::multi" = "originA"), parent = emptyenv())
  res_map <- .scan_resolver_index(idx_multi, origin_map)
  expect_equal(res_map$multi$origin, c("originA", "pkgB"))
})

test_that("full coverage for .scan_dir_files skip_dirs and .extract_code skip_pattern", {
  tmp_dir <- tempfile("skip_dir_test_")
  dir.create(tmp_dir)
  skip_dir <- file.path(tmp_dir, "skipme")
  dir.create(skip_dir)
  writeLines("library(stats)", file.path(skip_dir, "file.R"))
  keep_dir <- file.path(tmp_dir, "keepme")
  dir.create(keep_dir)
  writeLines("library(stats)", file.path(keep_dir, "file.R"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  files <- .scan_dir_files(tmp_dir, skip_dirs = "skipme")
  expect_false(any(grepl("skipme", files)))
  expect_true(any(grepl("keepme", files)))

  ancestor_skip_dir <- file.path(tmp_dir, "skipme", "project")
  dir.create(ancestor_skip_dir, recursive = TRUE)
  writeLines("library(stats)", file.path(ancestor_skip_dir, "proj_file.R"))
  files_ancestor <- .scan_dir_files(ancestor_skip_dir, skip_dirs = "skipme")
  expect_length(files_ancestor, 1L)

  # .extract_code skip_pattern no match
  tmp_r <- tempfile(fileext = ".R")
  writeLines("x <- 1", tmp_r)
  on.exit(unlink(tmp_r), add = TRUE)
  expect_equal(.extract_code(tmp_r, skip_pattern = "nonexistent_pkg"), "")

  tmp_rmd <- tempfile(fileext = ".Rmd")
  writeLines("```{r}\nx <- 1\n```", tmp_rmd)
  on.exit(unlink(tmp_rmd), add = TRUE)
  expect_equal(.extract_code(tmp_rmd, skip_pattern = "nonexistent_pkg"), "")

  # 4-arg function call to trigger n > 3L AST loop
  tmp_4arg <- tempfile(fileext = ".R")
  writeLines("library(pkgA)\nmyfun(1, 2, 3, 4)", tmp_4arg)
  on.exit(unlink(tmp_4arg), add = TRUE)
  res_4arg <- scan_usage(
    tmp_4arg,
    test_universe(
      "pkgA",
      list(myfun = "pkgA"),
      list2env(list("pkgA::myfun" = "pkgA"), parent = emptyenv())
    )
  )
  expect_true("pkgA::myfun" %in% res_4arg$functions)

  walker_stub <- .make_ast_walker(
    ignore_unqualified_functions = character(),
    allowed_packages = character(),
    use_heads = .scan_use_heads,
    ignore_heads = .scan_ignore_heads,
    export_names = character(),
    metapackages = NULL
  )
  tokens_no_pkg <- .scan_tokens(
    "x <- 1 + 2",
    allowed_packages = "allowedPkg",
    resolver_index = list(),
    metapackages = NULL,
    walker = walker_stub
  )
  expect_equal(tokens_no_pkg$pkgs, character(0))
})

test_that(".scan_dir_files preserves exact file ordering, empty skip_dirs, and handles deep trees", {
  tmp_dir <- tempfile("order_dir_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  b_dir <- file.path(tmp_dir, "b_dir")
  a_dir <- file.path(tmp_dir, "a_dir")
  dir.create(b_dir)
  dir.create(a_dir)

  file_b <- file.path(b_dir, "file_b.R")
  file_a <- file.path(a_dir, "file_a.R")
  writeLines("library(stats)", file_b)
  writeLines("library(stats)", file_a)

  files <- .scan_dir_files(tmp_dir, skip_dirs = character(0))
  expect_equal(files, sort(files))
  expect_length(files, 2L)

  # Deep directory tree (50 levels deep)
  deep_path <- tmp_dir
  for (i in 1:50) {
    deep_path <- file.path(deep_path, paste0("level_", i))
  }
  dir.create(deep_path, recursive = TRUE)
  deep_file <- file.path(deep_path, "deep.R")
  writeLines("library(stats)", deep_file)
  norm_deep_file <- normalizePath(deep_file, winslash = "/", mustWork = TRUE)

  deep_files <- .scan_dir_files(tmp_dir, skip_dirs = character(0))
  expect_true(norm_deep_file %in% deep_files)
})

test_that("package universes of 200, 500, and 1,000 packages scale chunked regex prefilter", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("pkg999::myfun()", tmp)

  for (n_pkgs in c(200, 500, 1000)) {
    pkgs <- unique(c(paste0("pkg", seq_len(n_pkgs)), "pkg999"))
    export_idx <- stats::setNames(as.list(pkgs), paste0("fun", seq_along(pkgs)))
    export_idx$myfun <- "pkg999"

    res <- scan_usage(
      path = tmp,
      universe = test_universe(pkgs, export_idx)
    )
    expect_true("pkg999" %in% res$packages)
    expect_true("pkg999::myfun" %in% res$functions)
  }
})

test_that(".extract_code reads empty files", {
  tmp_empty <- tempfile(fileext = ".R")
  file.create(tmp_empty)
  on.exit(unlink(tmp_empty), add = TRUE)
  expect_equal(.extract_code(tmp_empty), "")

  res_empty <- scan_usage(
    path = tmp_empty,
    universe = test_universe("stats")
  )
  expect_equal(res_empty$packages, character())
})

test_that("scanner results are identical before and after cleanup", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("library(stats)", "median(1:5)", "utils::head(letters)"), tmp)

  res <- scan_usage(
    path = tmp,
    universe = test_universe(
      c("stats", "utils"),
      list(median = "stats", head = "utils"),
      list2env(
        list("stats::median" = "stats", "utils::head" = "utils"),
        parent = emptyenv()
      )
    ),
    ignore_unqualified_functions = character()
  )

  expect_equal(res$packages, c("stats", "utils"))
  expect_equal(res$functions, c("stats::median", "utils::head"))
  expect_equal(res$ambiguous, character(0))
})
