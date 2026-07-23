test_that("stdlib_funs and scan_skip_dirs return precomputed vectors", {
  sf <- stdlib_funs()
  expect_type(sf, "character")
  expect_true("mean" %in% sf)

  ssd <- scan_skip_dirs()
  expect_type(ssd, "character")
  expect_true("renv" %in% ssd)
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
    allowed_packages = c("stats", "utils"),
    export_index = list(filter = "stats", head = "utils"),
    origin_map = c("stats::filter" = "stats", "utils::head" = "utils"),
    ignore_unqualified_functions = character(),
    quiet = TRUE
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
    allowed_packages = "stats",
    export_index = list(median = "stats", filter = "stats"),
    origin_map = c("stats::median" = "stats", "stats::filter" = "stats"),
    quiet = TRUE
  )

  expect_true("stats" %in% res$packages)
  expect_true("stats::median" %in% res$functions)
  expect_true("stats::filter" %in% res$functions)
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
    allowed_packages = "stats",
    export_index = list(median = "stats"),
    origin_map = c("stats::median" = "stats"),
    use_knitr = TRUE,
    quiet = TRUE
  )

  expect_true("stats" %in% res$packages)
  expect_true("stats::median" %in% res$functions)
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
    allowed_packages = "real_pkg",
    export_index = list(foo = "real_pkg"),
    origin_map = c("real_pkg::foo" = "real_pkg"),
    metapackages = list(meta_pkg = "real_pkg"),
    ignore_unqualified_functions = character(),
    quiet = TRUE
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
      allowed_packages = c("pkgA", "pkgB"),
      export_index = list(ambiguous_fun = c("pkgA", "pkgB")),
      origin_map = c(
        "pkgA::ambiguous_fun" = "pkgA",
        "pkgB::ambiguous_fun" = "pkgB"
      ),
      ignore_unqualified_functions = character(),
      strict = TRUE,
      quiet = TRUE
    ),
    "Cannot reliably detect"
  )

  # Non-strict mode issues a warning and records ambiguous function
  expect_warning(
    res <- scan_usage(
      path = tmp,
      allowed_packages = c("pkgA", "pkgB"),
      export_index = list(ambiguous_fun = c("pkgA", "pkgB")),
      origin_map = c(
        "pkgA::ambiguous_fun" = "pkgA",
        "pkgB::ambiguous_fun" = "pkgB"
      ),
      ignore_unqualified_functions = character(),
      strict = FALSE,
      quiet = TRUE
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
      allowed_packages = "stats",
      export_index = list(),
      origin_map = character(),
      quiet = TRUE
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
      allowed_packages = "stats",
      export_index = list(),
      origin_map = character(),
      quiet = TRUE
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
      allowed_packages = "stats",
      export_index = list(),
      origin_map = character(),
      quiet = TRUE
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
    allowed_packages = c("stats", "utils"),
    export_index = list(),
    origin_map = character(),
    skip_dirs = "renv",
    quiet = TRUE
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
      allowed_packages = "stats",
      export_index = list(),
      origin_map = character(),
      quiet = TRUE
    ),
    "Failed to parse"
  )
  expect_equal(res$packages, character())
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
    allowed_packages = "stats",
    export_index = list(filter = "stats"),
    origin_map = c("stats::filter" = "stats"),
    quiet = TRUE
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
    allowed_packages = "stats",
    export_index = list(),
    origin_map = character(),
    quiet = TRUE
  )
  expect_equal(res$packages, character())
  expect_equal(res$functions, character())

  res_empty_allowed <- scan_usage(
    path = tmp,
    allowed_packages = character(),
    export_index = list(),
    origin_map = character(),
    quiet = TRUE
  )
  expect_equal(res_empty_allowed$packages, character())
})

test_that("full coverage tests for all scan_usage.R branches", {
  # .scan_skip_regex with empty skip_dirs
  expect_equal(.scan_skip_regex(character(0)), "(^|/)(?:)(/|$)")

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
    allowed_packages = "stats",
    export_index = list(),
    origin_map = character(),
    quiet = TRUE
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

  # .scan_tokens with syntax error when file_path is NULL/empty
  expect_warning(
    hits <- .scan_tokens(
      "invalid syntax {{{",
      ignore_unqualified_functions = character(),
      file_path = NULL
    ),
    "Failed to parse"
  )
  expect_equal(hits$pkgs, character())

  expect_warning(
    hits_empty_path <- .scan_tokens(
      "invalid syntax {{{",
      ignore_unqualified_functions = character(),
      file_path = ""
    ),
    "Failed to parse"
  )
  expect_equal(hits_empty_path$pkgs, character())

  # .scan_tokens when export_index names is NULL / empty or resolver_index is NULL
  hits_no_exports <- .scan_tokens(
    "1 + 1",
    ignore_unqualified_functions = character(),
    export_index = list(),
    resolver_index = NULL
  )
  expect_equal(hits_no_exports$pkgs, character())

  hits_null_resolver <- .scan_tokens(
    "library(stats)\nfilter(1)",
    ignore_unqualified_functions = character(),
    allowed_packages = "stats",
    export_index = list(filter = "stats"),
    origin_map = c("stats::filter" = "stats"),
    resolver_index = NULL
  )
  expect_true("stats" %in% hits_null_resolver$pkgs)

  # .ast_walk edge cases
  ast_walk <- .ast_walk
  ignore <- ascribe::stdlib_funs()
  lib_funs <- .scan_lib_funs
  ns_ops <- .scan_ns_ops
  use_heads <- .scan_use_heads
  ignore_heads <- .scan_ignore_heads

  acc <- new.env(parent = emptyenv())
  acc$visit_idx <- 0L
  acc$lib_pkgs <- character()
  acc$lib_visit_idx <- integer()
  acc$lib_is_attach <- logical()
  acc$ns_pkgs <- character()
  acc$ns_keys <- character()
  acc$unqual_funs <- character()
  acc$unqual_visit_idx <- integer()

  # .ast_walk NULL, expression, pairlist, list, atom
  expect_invisible(ast_walk(
    NULL,
    acc,
    ignore,
    lib_funs,
    "stats",
    ns_ops,
    use_heads,
    ignore_heads,
    character(),
    NULL
  ))
  expect_invisible(ast_walk(
    expression(stats::median(1)),
    acc,
    ignore,
    lib_funs,
    "stats",
    ns_ops,
    use_heads,
    ignore_heads,
    "median",
    NULL
  ))
  expect_invisible(ast_walk(
    list(quote(stats::median(1))),
    acc,
    ignore,
    lib_funs,
    "stats",
    ns_ops,
    use_heads,
    ignore_heads,
    "median",
    NULL
  ))
  expect_invisible(ast_walk(
    pairlist(a = quote(stats::median(1))),
    acc,
    ignore,
    lib_funs,
    "stats",
    ns_ops,
    use_heads,
    ignore_heads,
    "median",
    NULL
  ))
  expect_invisible(ast_walk(
    quote(atom),
    acc,
    ignore,
    lib_funs,
    "stats",
    ns_ops,
    use_heads,
    ignore_heads,
    "median",
    NULL
  ))

  # .ast_member_fun edge cases
  expect_equal(.ast_member_fun(quote((1 + 1)$foo)), "foo")
  expect_equal(.ast_member_fun(quote((obj$member))), "member")
  expect_null(.ast_member_fun(as.call(list(123))))

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
  idx <- .scan_resolver_index(list(foo = character()), character())
  expect_null(idx$foo)
  idx_empty_origin <- .scan_resolver_index(
    list(foo = "pkgA"),
    character()
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
      list(foo = "pkgA"),
      c("pkgA::foo" = "pkgA")
    )$pkgs,
    character()
  )
  expect_equal(
    .resolve_candidates(
      list(funs = character(), idx = integer()),
      NULL,
      "pkgA",
      list(),
      character()
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
      list(),
      character()
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
  origin_map <- c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")

  cand_res <- .resolve_candidates(
    unqual_data,
    lib_df,
    allowed_packages = c("pkgA", "pkgB"),
    export_index = export_idx,
    origin_map = origin_map
  )
  expect_equal(cand_res$pkgs, "pkgB")

  testthat::with_mocked_bindings(
    .ascribe_require_namespace = \(pkg, quietly) FALSE,
    .package = "ascribe",
    {
      tmp_rmd <- tempfile(fileext = ".Rmd")
      writeLines("```{r}\nlibrary(stats)\n```", tmp_rmd)
      on.exit(unlink(tmp_rmd), add = TRUE)
      expect_error(
        .extract_code(tmp_rmd, allowed_packages = "stats", use_knitr = TRUE),
        "Package knitr is required"
      )
    }
  )
})
