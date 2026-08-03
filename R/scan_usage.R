#' Find used functions and packages
#'
#' Statically scans R source files for package attachments and function calls.
#' It recognizes `library()`, `require()`, `requireNamespace()`, and `use()`.
#'
#' Explicit package references from `library()`, `require()`,
#' `requireNamespace()`, `use()`, and `pkg::fun` are only recorded when their
#' package is included in `allowed_packages`. The scanner attributes an
#' unqualified function only when `library()` or `require()` attached a package
#' earlier in the same file and the supplied indexes can resolve the call.
#' `metapackages` can add packages to that attachment set. If several attached
#' packages export the function, the most recently attached match wins. The
#' scanner attributes known re-exports to their origin package and otherwise to
#' the resolved package.
#'
#' @param path A single project directory (searched recursively) or a vector of
#'   files (.R/.Rmd/.qmd).
#' @param allowed_packages Character vector of package namespaces to attribute
#'   calls to.
#' @param export_index Named list mapping function names to packages.
#' @param origin_map Environment mapping `pkg::fun` keys to the origin package,
#'   as returned by [build_origin_map()].
#' @param ignore_unqualified_functions Defaults to exports from base R packages
#'   listed in `stdlib_funs()`. Character vector of function names to ignore when
#'   attributing (unqualified) calls. Calls like `pkg::fun()` will NOT be ignored
#'   even if `fun` is in `ignore_unqualified_functions`, since they are
#'   namespaced.
#' @param strict If `FALSE` (default), warn on ambiguous function calls whose
#'   origin cannot be determined exactly. If `TRUE`, abort on ambiguous calls.
#' @param skip_dirs Character vector of directory names to skip when scanning a
#'   directory. Defaults to `scan_skip_dirs()`.
#' @param metapackages Named list mapping attached package names to additional
#'   packages that should be treated as co-attached for unqualified resolution.
#'   Defaults to `NULL`.
#' @param use_knitr Logical. If `TRUE`, parse `.Rmd` and `.qmd` files with
#'   `knitr::purl()`, which resolves knitr features the in-house parser ignores,
#'   such as `child` documents. It is also roughly an order of magnitude slower
#'   and comments out `eval=FALSE` and `purl=FALSE` chunks, so usage in them
#'   goes unrecorded. Defaults to `FALSE`.
#' @return A list of packages, resolved functions, and ambiguous function calls.
#' @export
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines(
#'   c(
#'     "# one messy analysis file",
#'     "library(stats)",
#'     "requireNamespace(\"utils\")",
#'     "filter(1:10, rep(1, 3))",
#'     "utils::head(letters)"
#'   ),
#'   path
#' )
#' scan_usage(
#'   path,
#'   allowed_packages = c("stats", "utils"),
#'   export_index = list(filter = "stats"),
#'   origin_map = list2env(list("stats::filter" = "stats"), parent = emptyenv()),
#'   ignore_unqualified_functions = character()
#' )
#' unlink(path)
scan_usage <- function(
  path = ".",
  allowed_packages,
  export_index,
  origin_map,
  ignore_unqualified_functions = .stdlib_funs,
  strict = FALSE,
  skip_dirs = .scan_skip_dirs,
  metapackages = NULL,
  use_knitr = FALSE
) {
  resolver_index <- .scan_resolver_index(export_index, origin_map)
  metapackages <- .normalize_metapackages(metapackages, allowed_packages)
  export_names <- names(export_index)
  if (is.null(export_names)) {
    export_names <- character()
  }
  walker <- .make_ast_walker(
    ignore_unqualified_functions = ignore_unqualified_functions,
    allowed_packages = allowed_packages,
    use_heads = .scan_use_heads,
    ignore_heads = .scan_ignore_heads,
    export_names = export_names,
    metapackages = metapackages
  )

  paths <- normalizePath(path, winslash = "/", mustWork = TRUE)
  dir_flags <- dir.exists(paths)

  files <- if (length(paths) == 1L && dir_flags) {
    dir_path <- paths[[1L]]
    cli::cli_alert_info("Searching directory {.path {dir_path}}")
    .scan_dir_files(dir_path, skip_dirs)
  } else {
    if (any(dir_flags)) {
      cli::cli_abort(c(
        "{.arg path} must be a single directory or a vector of files.",
        "x" = "Mixed directories and files or multiple directories are not supported."
      ))
    }
    lapply(
      paths,
      \(file_path) cli::cli_alert_info("Searching {.path {file_path}}")
    )
    paths
  }

  if (!length(files)) {
    cli::cli_abort(c(
      "No files found.",
      "i" = "Check the {.arg path} and {.arg skip_dirs} arguments."
    ))
  }

  # Built once here (not per file) and reused by both .extract_code (on the
  # raw file text) and .scan_tokens (on the post-extraction code), so files
  # are never rescanned for the same package names with two different
  # mechanisms.
  skip_patterns <- .build_skip_patterns(c(
    allowed_packages,
    names(metapackages)
  ))

  hits <- lapply(
    unique(files),
    \(file) {
      code_str <- .extract_code(
        file,
        skip_patterns = skip_patterns,
        use_knitr = use_knitr
      )
      is_r <- grepl("\\.r$", file, ignore.case = TRUE)
      .scan_tokens(
        code_str,
        allowed_packages = allowed_packages,
        resolver_index = resolver_index,
        metapackages = metapackages,
        walker = walker,
        file_path = file,
        skip_patterns = if (is_r) NULL else skip_patterns
      )
    }
  )

  ambiguous <- .collect_unique(hits, "ambiguous")
  if (length(ambiguous)) {
    msg <- c(
      "Cannot reliably detect which packages some functions are from.",
      "x" = paste0(
        "Ambiguous functions: ",
        paste0("{.fun ", ambiguous, "}", collapse = ", ")
      ),
      "i" = "Please namespace them ({.code pkg::function()}) and rerun or set {.code strict = FALSE}."
    )

    if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
  }

  structure(
    list(
      packages = .collect_unique(hits, "pkgs"),
      functions = .collect_unique(hits, "keys"),
      ambiguous = ambiguous
    ),
    class = "scan_usage"
  )
}

.scan_dir_files <- function(dir_path, skip_dirs) {
  dir_path <- normalizePath(dir_path, winslash = "/", mustWork = TRUE)

  # BFS queue as an index cursor over plain vectors grown by out-of-bounds
  # indexed assignment (`x[i] <- v`). R (>= 3.4.0) overallocates on such
  # extension, so appends are amortized O(1); `c(x, v)` always copies.
  dirs_to_visit <- dir_path
  n_dirs <- 1L
  matching_files <- character()
  n_files <- 0L

  i <- 1L
  while (i <= n_dirs) {
    curr <- dirs_to_visit[[i]]
    i <- i + 1L

    entries <- list.files(
      curr,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    if (!length(entries)) {
      next
    }
    entries <- chartr("\\", "/", entries)

    is_source_file <- grepl("\\.(R|Rmd|Qmd)$", entries, ignore.case = TRUE)
    maybe_dir_idx <- which(!is_source_file)
    is_dir <- logical(length(entries))
    if (length(maybe_dir_idx)) {
      is_dir[maybe_dir_idx] <- dir.exists(entries[maybe_dir_idx])
    }

    sub_dirs <- entries[is_dir]
    files <- entries[is_source_file]

    if (length(files)) {
      k <- length(files)
      matching_files[(n_files + 1L):(n_files + k)] <- files
      n_files <- n_files + k
    }

    if (length(sub_dirs)) {
      if (length(skip_dirs)) {
        sub_dirs <- sub_dirs[!basename(sub_dirs) %in% skip_dirs]
      }
      k <- length(sub_dirs)
      if (k) {
        dirs_to_visit[(n_dirs + 1L):(n_dirs + k)] <- sub_dirs
        n_dirs <- n_dirs + k
      }
    }
  }

  sort(matching_files[seq_len(n_files)])
}

.collect_unique <- function(hits, field) {
  hits |>
    lapply(`[[`, field) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()
}

# Chunks pkgs into word-boundary alternation regexes (PCRE limits how many
# alternatives a single pattern can hold), so a package-name prefilter
# stays a handful of grepl() calls even for large universes.
.build_skip_patterns <- function(pkgs) {
  u_pkgs <- unique(pkgs)
  if (!length(u_pkgs)) {
    return(NULL)
  }
  chunk_size <- 200L
  n_chunks <- ceiling(length(u_pkgs) / chunk_size)
  chunks <- split(
    u_pkgs,
    rep(seq_len(n_chunks), each = chunk_size, length.out = length(u_pkgs))
  )
  vapply(
    chunks,
    \(chk) {
      escaped <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", chk)
      paste0("\\b(", paste(escaped, collapse = "|"), ")\\b")
    },
    character(1)
  )
}

.any_pattern_matches <- function(patterns, text) {
  if (is.null(patterns)) {
    return(TRUE)
  }
  for (pat in patterns) {
    if (any(grepl(pat, text, perl = TRUE, useBytes = TRUE))) {
      return(TRUE)
    }
  }
  FALSE
}

.normalize_metapackages <- function(metapackages, allowed_packages) {
  if (is.null(metapackages)) {
    return(NULL)
  }

  lapply(
    metapackages,
    \(pkgs) unique(pkgs[!is.na(fastmatch::fmatch(pkgs, allowed_packages))])
  )
}

.read_file_lf <- function(file) {
  raw <- brio::read_file(file)
  if (!length(raw)) "" else gsub("\r\n", "\n", raw, fixed = TRUE)
}

.extract_code <- function(
  file,
  skip_patterns = NULL,
  use_knitr = FALSE
) {
  ext <- file |>
    sub(".*\\.", "", x = _) |>
    tolower()

  if (!ext %in% c("r", "rmd", "qmd")) {
    cli::cli_abort(c(
      "Unsupported file extension: {.val {ext}}.",
      "i" = "Supported extensions are {.file .R}, {.file .Rmd}, and {.file .qmd}."
    ))
  }

  code_raw <- .read_file_lf(file)

  if (!.any_pattern_matches(skip_patterns, code_raw)) {
    return("")
  }

  if (ext == "r") {
    return(code_raw)
  }

  if (!grepl("(?m)^\\s*[`~]{3,}", code_raw, perl = TRUE, useBytes = TRUE)) {
    return("")
  }

  if (use_knitr) {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg knitr} is required to parse R Markdown ({.file .Rmd}) or Quarto ({.file .qmd}) files when {.code use_knitr = TRUE}.",
        "i" = "Install it with {.code install.packages('knitr')} or use the default in-house parser."
      ))
    }

    tmp <- tempfile(fileext = ".R")
    on.exit(unlink(tmp), add = TRUE)
    knitr::purl(file, tmp, quiet = TRUE, documentation = 0)
    .read_file_lf(tmp)
  } else {
    .extract_markdown_code(strsplit(code_raw, "\n", fixed = TRUE)[[1]])
  }
}

.extract_markdown_code <- function(lines) {
  n <- length(lines)
  if (!n) {
    return("")
  }
  fence_rows <- grep("^\\s*[`~]{3,}", lines, perl = TRUE)
  if (!length(fence_rows)) {
    return("")
  }

  fence_lines <- lines[fence_rows]
  caps <- regmatches(
    fence_lines,
    regexec(
      "^\\s*([`~]{3,})\\s*\\{\\s*[rR]\\b[^}]*\\}\\s*$",
      fence_lines,
      perl = TRUE
    )
  )

  chunks <- vector("list", length(fence_rows))
  j <- 0L
  k <- 1L
  n_fences <- length(fence_rows)

  while (k <= n_fences) {
    cap <- caps[[k]]
    if (!length(cap)) {
      k <- k + 1L
      next
    }

    fence <- cap[[2L]]
    fence_char <- substr(fence, 1L, 1L)
    escaped_char <- if (fence_char == "`") "\\`" else "~"
    close_pat <- paste0("^\\s*", escaped_char, "{", nchar(fence), ",}\\s*$")

    i <- fence_rows[[k]]
    start <- i + 1L
    k <- k + 1L
    close_row <- n + 1L

    while (k <= n_fences) {
      if (grepl(close_pat, fence_lines[[k]], perl = TRUE)) {
        close_row <- fence_rows[[k]]
        break
      }
      k <- k + 1L
    }

    if (close_row > start) {
      j <- j + 1L
      chunks[[j]] <- c(lines[start:(close_row - 1L)], "")
    }
    k <- k + 1L
  }

  if (!j) {
    return("")
  }
  vapply(chunks[seq_len(j)], paste, character(1), collapse = "\n")
}

.scan_use_heads <- c("c", "list")
.scan_ignore_heads <- c(
  "if",
  "for",
  "while",
  "repeat",
  "function",
  "return",
  "next",
  "break",
  "{",
  "(",
  "<-",
  "<<-",
  "->",
  "->>",
  "=",
  "+",
  "-",
  "*",
  "/",
  "^",
  "%%",
  "%/%",
  "%*%",
  "%>%",
  ":",
  "|",
  "&",
  "||",
  "&&",
  "!",
  "~",
  "|>",
  "$",
  "@",
  "[",
  "[["
)

.scan_tokens <- function(
  code,
  allowed_packages,
  resolver_index,
  metapackages,
  walker,
  file_path,
  skip_patterns = .build_skip_patterns(c(allowed_packages, names(metapackages)))
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!any(nzchar(code))) {
    return(empty)
  }

  if (!.any_pattern_matches(skip_patterns, code)) {
    return(empty)
  }

  expr <- tryCatch(
    parse(text = code, keep.source = FALSE),
    error = function(e) NULL
  )
  if (is.null(expr)) {
    expr <- do.call(
      c,
      lapply(
        code,
        \(chunk) {
          tryCatch(
            parse(text = chunk, keep.source = FALSE),
            error = function(e) NULL
          )
        }
      )
    )
    path_label <- if (
      length(file_path) > 0L &&
        !is.null(file_path[[1L]]) &&
        nzchar(file_path[[1L]])
    ) {
      file_path[[1L]]
    } else {
      "<unknown file>"
    }
    msg <- c(
      "Failed to parse {.path {path_label}}.",
      "x" = "Syntax error in file."
    )
    cli::cli_warn(msg)
  }

  acc <- new.env(parent = emptyenv())
  acc$visit_idx <- 0L
  acc$lib_pkgs <- character()
  acc$lib_visit_idx <- integer()
  acc$lib_is_attach <- logical()
  acc$ns_pkgs <- character()
  acc$ns_keys <- character()
  acc$unqual_funs <- character()
  acc$unqual_visit_idx <- integer()

  for (i in seq_along(expr)) {
    walker(expr[[i]], acc)
  }

  lib_data <- if (length(acc$lib_pkgs)) {
    list(
      visit_idx = acc$lib_visit_idx,
      pkg = acc$lib_pkgs,
      is_attach = acc$lib_is_attach
    )
  } else {
    NULL
  }

  if (is.null(lib_data) || !any(lib_data$is_attach)) {
    return(list(
      pkgs = c(acc$lib_pkgs, acc$ns_pkgs),
      keys = acc$ns_keys,
      ambiguous = character()
    ))
  }

  resolved <- .resolve_candidates(
    unqual = list(funs = acc$unqual_funs, idx = acc$unqual_visit_idx),
    lib_data = lib_data,
    allowed_packages = allowed_packages,
    resolver_index = resolver_index
  )

  list(
    pkgs = c(acc$lib_pkgs, acc$ns_pkgs, resolved$pkgs),
    keys = c(acc$ns_keys, resolved$keys),
    ambiguous = resolved$ambiguous
  )
}

.make_ast_walker <- function(
  ignore_unqualified_functions,
  allowed_packages,
  use_heads,
  ignore_heads,
  export_names,
  metapackages
) {
  make_env <- function(vec, value = TRUE) {
    if (!length(vec)) {
      return(new.env(parent = emptyenv(), hash = TRUE))
    }
    vals <- rep.int(list(value), length(vec))
    names(vals) <- vec
    list2env(vals, parent = emptyenv(), hash = TRUE)
  }

  allowed_pkgs_env <- make_env(allowed_packages)
  export_names_env <- make_env(export_names)

  head_kind_env <- make_env(
    setdiff(export_names, ignore_unqualified_functions),
    6L
  )
  head_kind_env[["::"]] <- 2L
  head_kind_env[[":::"]] <- 2L
  head_kind_env[["library"]] <- 3L
  head_kind_env[["require"]] <- 3L
  head_kind_env[["requireNamespace"]] <- 4L
  head_kind_env[["use"]] <- 5L
  for (nm in ignore_heads) {
    head_kind_env[[nm]] <- 1L
  }

  walk <- function(x, acc) {
    if (is.null(x)) {
      return(invisible(NULL))
    }

    if (is.call(x)) {
      acc$visit_idx <- acc$visit_idx + 1L

      head <- x[[1L]]

      if (is.symbol(head)) {
        head_name <- as.character(head)
        kind <- head_kind_env[[head_name]]
        if (is.null(kind) || kind == 1L) {
          # Not in the export index, or a language keyword/operator/subset.
        } else if (kind == 6L) {
          acc$unqual_funs <- c(acc$unqual_funs, head_name)
          acc$unqual_visit_idx <- c(acc$unqual_visit_idx, acc$visit_idx)
        } else if (kind == 2L) {
          if (length(x) >= 3L) {
            pkg <- .ast_lit_name(x[[2L]])
            fun <- .ast_lit_name(x[[3L]])
            if (
              !is.null(pkg) &&
                !is.null(fun) &&
                !is.null(allowed_pkgs_env[[pkg]])
            ) {
              acc$ns_pkgs <- c(acc$ns_pkgs, pkg)
              acc$ns_keys <- c(acc$ns_keys, paste0(pkg, "::", fun))
            }
          }
        } else if (kind == 3L || kind == 4L) {
          pkg <- .ast_get_lib_pkg(x)
          if (!is.null(pkg)) {
            is_allowed <- !is.null(allowed_pkgs_env[[pkg]])
            is_attach <- kind == 3L

            if (is_allowed) {
              acc$lib_pkgs <- c(acc$lib_pkgs, pkg)
              acc$lib_visit_idx <- c(acc$lib_visit_idx, acc$visit_idx)
              acc$lib_is_attach <- c(acc$lib_is_attach, is_attach)
            }

            if (is_attach && !is.null(metapackages)) {
              expanded_pkgs <- metapackages[[pkg]]
              if (length(expanded_pkgs)) {
                acc$lib_pkgs <- c(acc$lib_pkgs, expanded_pkgs)
                acc$lib_visit_idx <- c(
                  acc$lib_visit_idx,
                  rep.int(acc$visit_idx, length(expanded_pkgs))
                )
                acc$lib_is_attach <- c(
                  acc$lib_is_attach,
                  rep.int(TRUE, length(expanded_pkgs))
                )
              }
            }
          }
        } else {
          pkg <- .ast_get_lib_pkg(x)
          if (
            !is.null(pkg) &&
              !is.null(allowed_pkgs_env[[pkg]])
          ) {
            acc$ns_pkgs <- c(acc$ns_pkgs, pkg)
            funs <- .ast_get_use_funs(x, use_heads)
            if (length(funs)) {
              acc$ns_keys <- c(acc$ns_keys, paste0(pkg, "::", funs))
            }
          }
        }
      } else if (is.call(head)) {
        member_fun <- .ast_member_fun(head)
        if (
          !is.null(member_fun) &&
            !is.null(export_names_env[[member_fun]])
        ) {
          acc$unqual_funs <- c(acc$unqual_funs, member_fun)
          acc$unqual_visit_idx <- c(acc$unqual_visit_idx, acc$visit_idx)
        }
        walk(head, acc)
      }

      n <- length(x)
      if (n == 2L) {
        walk(x[[2L]], acc)
      } else if (n == 3L) {
        walk(x[[2L]], acc)
        walk(x[[3L]], acc)
      } else if (n > 3L) {
        for (i in 2L:n) {
          walk(x[[i]], acc)
        }
      }
      return(invisible(NULL))
    }

    if (is.expression(x) || is.pairlist(x) || is.list(x)) {
      for (i in seq_along(x)) {
        walk(x[[i]], acc)
      }
      return(invisible(NULL))
    }

    invisible(NULL)
  }
  walk
}

.ast_lit_name <- function(x) {
  if (is.symbol(x)) {
    return(as.character(x))
  }
  if (is.character(x) && length(x) == 1L) {
    return(x)
  }
  NULL
}

.ast_member_fun <- function(head) {
  if (!is.call(head) || !length(head)) {
    return(NULL)
  }

  op <- head[[1L]]
  if (!is.symbol(op)) {
    return(NULL)
  }

  op_name <- as.character(op)

  if (op_name %in% c("$", "@") && length(head) >= 3L) {
    return(.ast_lit_name(head[[3L]]))
  }

  if (op_name == "(" && length(head) >= 2L) {
    return(.ast_member_fun(head[[2L]]))
  }

  NULL
}

.ast_get_lib_pkg <- function(call) {
  n <- length(call)
  if (n <= 1L) {
    return(NULL)
  }

  nms <- names(call)
  arg_nms <- if (!is.null(nms) && n >= 2L) nms[-1L] else NULL
  pkg_i <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("package", arg_nms)
  } else {
    NA_integer_
  }
  pkg_j <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("pkg", arg_nms)
  } else {
    NA_integer_
  }

  arg_idx <- if (!is.na(pkg_i)) {
    pkg_i + 1L
  } else if (!is.na(pkg_j)) {
    pkg_j + 1L
  } else {
    2L
  }

  .ast_lit_name(call[[arg_idx]])
}

.ast_collect_use_funs <- function(x, use_heads) {
  if (is.null(x)) {
    return(character())
  }

  lit <- .ast_lit_name(x)
  if (!is.null(lit)) {
    return(lit)
  }

  if (is.call(x)) {
    head <- x[[1L]]
    head_name <- if (is.symbol(head)) as.character(head) else NULL
    if (
      !is.null(head_name) && !is.na(fastmatch::fmatch(head_name, use_heads))
    ) {
      n <- length(x)
      if (n <= 1L) {
        return(character())
      }
      out <- vector("list", n - 1L)
      for (i in 2L:n) {
        out[[i - 1L]] <- .ast_collect_use_funs(x[[i]], use_heads = use_heads)
      }
      return(unlist(out, use.names = FALSE))
    }
  }

  character()
}

.ast_get_use_funs <- function(call, use_heads) {
  n <- length(call)
  if (n <= 2L) {
    return(character())
  }

  nms <- names(call)
  arg_nms <- if (!is.null(nms)) nms[-1L] else NULL
  pkg_i <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("pkg", arg_nms)
  } else {
    NA_integer_
  }
  pkg_j <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("package", arg_nms)
  } else {
    NA_integer_
  }

  pkg_idx <- if (!is.na(pkg_i)) {
    pkg_i
  } else if (!is.na(pkg_j)) {
    pkg_j
  } else {
    1L
  }

  out <- vector("list", n - 2L)
  j <- 0L
  for (i in 2L:n) {
    arg_idx <- i - 1L
    if (arg_idx == pkg_idx) {
      next
    }
    j <- j + 1
    out[[j]] <- .ast_collect_use_funs(call[[i]], use_heads = use_heads)
  }
  funs <- unlist(out, use.names = FALSE)
  funs[nzchar(funs)]
}

.scan_resolver_index <- function(
  export_index,
  origin_map
) {
  funs <- names(export_index)
  if (is.null(funs) || length(funs) == 0L) {
    return(list())
  }

  lens <- lengths(export_index)
  n_funs <- length(funs)
  has_map <- !is.null(origin_map) && length(origin_map) > 0L
  res <- vector("list", n_funs)

  get_map_val <- function(key) {
    get0(key, envir = origin_map, ifnotfound = NULL)
  }

  # Single provider functions (>95% of cases)
  single_idx <- which(lens == 1L)
  if (length(single_idx) > 0L) {
    s_funs <- funs[single_idx]
    s_provs <- unlist(export_index[single_idx], use.names = FALSE)
    if (has_map) {
      s_keys <- paste0(s_provs, "::", s_funs)
      for (k in seq_along(single_idx)) {
        i <- single_idx[[k]]
        p <- s_provs[[k]]
        v <- get_map_val(s_keys[[k]])
        orig <- if (is.null(v) || !nzchar(v)) p else v
        res[[i]] <- list(provider = p, origin = orig)
      }
    } else {
      for (k in seq_along(single_idx)) {
        i <- single_idx[[k]]
        p <- s_provs[[k]]
        res[[i]] <- list(provider = p, origin = p)
      }
    }
  }

  # Multi-provider functions
  other_idx <- which(lens > 1L)
  if (length(other_idx) > 0L) {
    for (i in other_idx) {
      providers <- export_index[[i]]
      n <- length(providers)
      fun <- funs[[i]]
      origins <- character(n)
      for (j in seq_len(n)) {
        p <- providers[[j]]
        v <- if (has_map) get_map_val(paste0(p, "::", fun)) else NULL
        origins[[j]] <- if (is.null(v) || !nzchar(v)) p else v
      }
      res[[i]] <- list(provider = providers, origin = origins)
    }
  }

  names(res) <- funs
  res
}

.resolve_meta <- function(
  fun,
  attached,
  allowed_packages,
  resolver_index
) {
  meta <- resolver_index[[fun]]
  if (is.null(meta) || !length(meta$provider)) {
    return(NULL)
  }

  keep <- !is.na(fastmatch::fmatch(meta$provider, allowed_packages)) &
    !is.na(fastmatch::fmatch(meta$provider, attached$pkg))
  if (!any(keep)) {
    return(NULL)
  }

  origin <- meta$origin[keep]
  origin_allowed <- !is.na(fastmatch::fmatch(origin, allowed_packages))
  if (!any(origin_allowed)) {
    return(NULL)
  }

  list(
    provider = meta$provider[keep],
    origin = origin,
    origin_allowed = origin_allowed
  )
}

.resolve_calls <- function(
  meta,
  attached,
  attached_rows,
  visit_idx,
  allowed_packages
) {
  allowed_origins <- unique(meta$origin[meta$origin_allowed])
  if (length(allowed_origins) == 1L) {
    return(rep.int(allowed_origins[[1L]], length(visit_idx)))
  }

  attached_match_idx <- do.call(
    cbind,
    lapply(
      meta$provider,
      \(pkg) {
        provider_rows <- attached_rows[[pkg]]
        hits <- findInterval(visit_idx, attached$visit_idx[provider_rows])
        out <- rep.int(-1L, length(visit_idx))
        matched <- hits > 0L
        out[matched] <- provider_rows[hits[matched]]
        out
      }
    )
  )

  best_provider <- max.col(attached_match_idx, ties.method = "first")
  matched <- attached_match_idx[
    cbind(seq_along(best_provider), best_provider)
  ]
  resolved <- rep.int("", length(visit_idx))
  keep <- matched > 0L
  if (!any(keep)) {
    return(resolved)
  }

  res_orig <- meta$origin[best_provider[keep]]
  res_prov <- meta$provider[best_provider[keep]]
  unallowed <- is.na(fastmatch::fmatch(res_orig, allowed_packages))
  res_val <- res_orig
  res_val[unallowed] <- res_prov[unallowed]
  resolved[keep] <- res_val
  resolved
}

.resolve_candidates <- function(
  unqual,
  lib_data,
  allowed_packages,
  resolver_index
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!length(unqual$funs) || !length(allowed_packages)) {
    return(empty)
  }
  if (is.null(lib_data) || !any(lib_data$is_attach)) {
    return(empty)
  }

  attached <- list(
    visit_idx = lib_data$visit_idx[lib_data$is_attach],
    pkg = lib_data$pkg[lib_data$is_attach]
  )
  if (length(attached$visit_idx) > 1L) {
    ord <- order(attached$visit_idx, seq_along(attached$visit_idx))
    attached$visit_idx <- attached$visit_idx[ord]
    attached$pkg <- attached$pkg[ord]
  }
  attached_rows <- split(seq_along(attached$pkg), attached$pkg)

  resolved_pkgs <- rep.int("", length(unqual$funs))
  considered <- logical(length(unqual$funs))
  call_groups <- split(seq_along(unqual$funs), unqual$funs)
  for (fun in names(call_groups)) {
    idx <- call_groups[[fun]]
    meta <- .resolve_meta(
      fun = fun,
      attached = attached,
      allowed_packages = allowed_packages,
      resolver_index = resolver_index
    )
    if (is.null(meta)) {
      next
    }

    considered[idx] <- TRUE
    resolved_pkgs[idx] <- .resolve_calls(
      meta = meta,
      attached = attached,
      attached_rows = attached_rows,
      visit_idx = unqual$idx[idx],
      allowed_packages = allowed_packages
    )
  }
  if (!any(considered)) {
    return(empty)
  }

  resolved <- nzchar(resolved_pkgs)
  list(
    pkgs = resolved_pkgs[resolved],
    keys = if (any(resolved)) {
      paste0(resolved_pkgs[resolved], "::", unqual$funs[resolved])
    } else {
      character()
    },
    ambiguous = if (all(!considered | resolved)) {
      character()
    } else {
      sort(unique(unqual$funs[considered & !resolved]))
    }
  )
}

#' Ignored functions/directories used by scanner
#'
#' @name internal_data
#' @rdname internal_data
#' @keywords internal
NULL

#' Default ignored functions
#'
#' Vector of functions to be ignored when parsing.
#' Generated in `data-raw/sysdata.R` from exports of base R packages.
#'
#' @rdname internal_data
#' @return A character vector of function names to ignore.
#' @export
#' @examples
#' head(stdlib_funs())
stdlib_funs <- function() {
  .stdlib_funs
}

#' Default skip directories
#'
#' Vector of directories skipped when recursively searching
#' a project. Generated in `data-raw/sysdata.R`.
#'
#' @rdname internal_data
#' @return A character vector of directory names to skip.
#' @export
#' @examples
#' scan_skip_dirs()
scan_skip_dirs <- function() {
  .scan_skip_dirs
}
