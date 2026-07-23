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
#'   `knitr::purl()`. This is more accurate for knitr/quarto chunk handling
#'   but much slower than the default in-house parser. Defaults to `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses status messages. Defaults to
#'   `FALSE`.
#' @param resolver_index Optional precomputed index mapping function names to
#'   provider packages and origins (as computed by `.scan_resolver_index()`).
#'   Defaults to `NULL`.
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
#'   ignore_unqualified_functions = character(),
#'   quiet = TRUE
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
  use_knitr = FALSE,
  quiet = FALSE,
  resolver_index = NULL
) {
  if (quiet) {
    old_options <- options(cli.default_handler = \(msg) invisible(NULL))
    on.exit(options(old_options), add = TRUE)
  }
  resolver_index <- .scan_resolver_index(export_index, origin_map)
  metapackages <- .normalize_metapackages(metapackages, allowed_packages)
  export_names <- names(export_index)
  if (is.null(export_names)) {
    export_names <- character()
  }
  walker <- .make_ast_walker(
    ignore_unqualified_functions = ignore_unqualified_functions,
    lib_funs = .scan_lib_funs,
    allowed_packages = allowed_packages,
    ns_ops = .scan_ns_ops,
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

  # Build skip_pattern once here rather than once per file inside .extract_code
  skip_pkgs <- c(allowed_packages, names(metapackages))
  u_skip_pkgs <- unique(skip_pkgs)
  skip_pattern <- if (length(u_skip_pkgs) > 0L && length(u_skip_pkgs) <= 200L) {
    escaped <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", u_skip_pkgs)
    paste0("\\b(", paste(escaped, collapse = "|"), ")\\b")
  } else {
    NULL
  }

  hits <- lapply(
    unique(files),
    \(file) {
      code_str <- .extract_code(
        file,
        skip_pattern = skip_pattern,
        use_knitr = use_knitr
      )
      .scan_tokens(
        code_str,
        ignore_unqualified_functions = ignore_unqualified_functions,
        allowed_packages = allowed_packages,
        export_index = export_index,
        origin_map = origin_map,
        resolver_index = resolver_index,
        metapackages = metapackages,
        walker = walker,
        file_path = file
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

.scan_skip_regex <- function(skip_dirs) {
  escaped <- vapply(
    skip_dirs,
    \(x) gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x),
    character(1)
  )
  paste0("(^|/)(?:", paste(escaped, collapse = "|"), ")(/|$)")
}

.scan_dir_files <- function(dir_path, skip_dirs) {
  dir_path <- normalizePath(dir_path, winslash = "/", mustWork = TRUE)
  files <- list.files(
    dir_path,
    pattern = "\\.(R|Rmd|Qmd)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (!length(files)) {
    return(character())
  }
  files <- chartr("\\", "/", files)
  if (length(skip_dirs)) {
    pat <- .scan_skip_regex(skip_dirs)
    files <- files[!grepl(pat, files, perl = TRUE)]
  }
  files
}

.collect_unique <- function(hits, field) {
  hits |>
    lapply(`[[`, field) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()
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

.extract_code <- function(file, skip_pattern = NULL, use_knitr = FALSE) {
  ext <- file |>
    sub(".*\\.", "", x = _) |>
    tolower()

  if (!ext %in% c("r", "rmd", "qmd")) {
    cli::cli_abort(c(
      "Unsupported file extension: {.val {ext}}.",
      "i" = "Supported extensions are {.file .R}, {.file .Rmd}, and {.file .qmd}."
    ))
  }

  lines <- readLines(file, warn = FALSE)
  code_raw <- paste(lines, collapse = "\n")

  if (
    !is.null(skip_pattern) &&
      !grepl(skip_pattern, code_raw, perl = TRUE, useBytes = TRUE)
  ) {
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
    paste(readLines(tmp, warn = FALSE), collapse = "\n")
  } else {
    .extract_markdown_code(lines)
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
  paste(unlist(chunks[seq_len(j)], use.names = FALSE), collapse = "\n")
}

.scan_lib_funs <- c("library", "require", "requireNamespace")
.scan_ns_ops <- c("::", ":::")
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
  ignore_unqualified_functions,
  allowed_packages = character(),
  export_index = list(),
  origin_map = NULL,
  resolver_index = NULL,
  metapackages = NULL,
  walker = NULL,
  file_path = NULL
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!nzchar(code)) {
    return(empty)
  }

  relevant_pkgs <- unique(c(allowed_packages, names(metapackages)))
  if (length(relevant_pkgs) > 0L) {
    has_pkg <- FALSE
    for (p in relevant_pkgs) {
      if (grepl(p, code, fixed = TRUE, useBytes = TRUE)) {
        has_pkg <- TRUE
        break
      }
    }
    if (!has_pkg) {
      return(empty)
    }
  }

  expr <- tryCatch(
    parse(text = code, keep.source = FALSE),
    error = function(e) NULL
  )
  if (is.null(expr)) {
    path_label <- if (!is.null(file_path) && nzchar(file_path)) {
      file_path
    } else {
      "<unknown file>"
    }
    msg <- c(
      "Failed to parse {.path {path_label}}.",
      "x" = "Syntax error in file."
    )
    cli::cli_warn(msg)
    return(empty)
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

  export_names <- names(export_index)
  if (is.null(export_names)) {
    export_names <- character()
  }
  if (is.null(resolver_index)) {
    resolver_index <- .scan_resolver_index(export_index, origin_map)
  }

  if (is.null(walker)) {
    walker <- .make_ast_walker(
      ignore_unqualified_functions = ignore_unqualified_functions,
      lib_funs = .scan_lib_funs,
      allowed_packages = allowed_packages,
      ns_ops = .scan_ns_ops,
      use_heads = .scan_use_heads,
      ignore_heads = .scan_ignore_heads,
      export_names = export_names,
      metapackages = metapackages
    )
  }

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
    list(funs = acc$unqual_funs, idx = acc$unqual_visit_idx),
    lib_data,
    allowed_packages,
    export_index,
    origin_map,
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
  lib_funs,
  allowed_packages,
  ns_ops,
  use_heads,
  ignore_heads,
  export_names,
  metapackages
) {
  make_env <- function(vec) {
    e <- new.env(parent = emptyenv(), hash = TRUE)
    if (length(vec)) {
      for (x in vec) {
        e[[x]] <- TRUE
      }
    }
    e
  }

  ignore_unqual_env <- make_env(ignore_unqualified_functions)
  allowed_pkgs_env <- make_env(allowed_packages)
  ignore_heads_env <- make_env(ignore_heads)
  export_names_env <- make_env(export_names)
  use_heads_env <- make_env(use_heads)

  walk <- function(x, acc) {
    if (is.null(x)) {
      return(invisible(NULL))
    }

    if (is.call(x)) {
      acc$visit_idx <- acc$visit_idx + 1L

      head <- x[[1L]]
      head_is_call <- is.call(head)

      if (is.symbol(head)) {
        head_name <- as.character(head)
        if (!is.null(ignore_heads_env[[head_name]])) {
          # Skip language keywords, operators, and subsetting.
        } else if (head_name == "::" || head_name == ":::") {
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
        } else if (
          head_name == "library" ||
            head_name == "require" ||
            head_name == "requireNamespace"
        ) {
          pkg <- .ast_get_lib_pkg(x)
          if (!is.null(pkg)) {
            is_allowed <- !is.null(allowed_pkgs_env[[pkg]])
            is_attach <- head_name != "requireNamespace"

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
        } else if (head_name == "use") {
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
        } else if (is.null(export_names_env[[head_name]])) {
          # Ignore calls not in the export index.
        } else if (is.null(ignore_unqual_env[[head_name]])) {
          acc$unqual_funs <- c(acc$unqual_funs, head_name)
          acc$unqual_visit_idx <- c(acc$unqual_visit_idx, acc$visit_idx)
        }
      } else if (head_is_call) {
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
  export_index = list(),
  origin_map = NULL
) {
  funs <- names(export_index)
  if (is.null(funs)) {
    return(list())
  }

  has_map <- !is.null(origin_map)

  res <- lapply(
    seq_along(export_index),
    \(i) {
      providers <- export_index[[i]]
      fun <- funs[[i]]
      n <- length(providers)
      if (n == 0L) {
        return(NULL)
      }

      if (n == 1L) {
        orig <- if (has_map) {
          origin_map[[paste0(providers, "::", fun)]]
        } else {
          NULL
        }
        if (is.null(orig) || !nzchar(orig)) {
          orig <- providers
        }
        return(list(provider = providers, origin = orig))
      }

      keys <- paste0(providers, "::", fun)
      origins <- if (has_map) {
        vapply(
          keys,
          \(k) {
            v <- origin_map[[k]]
            if (is.null(v)) "" else v
          },
          character(1),
          USE.NAMES = FALSE
        )
      } else {
        character(n)
      }
      missing <- !nzchar(origins)
      origins[missing] <- providers[missing]

      list(provider = providers, origin = origins)
    }
  )

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
  allowed_packages = character(),
  export_index = list(),
  origin_map = NULL,
  resolver_index = NULL
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!length(unqual$funs) || !length(allowed_packages)) {
    return(empty)
  }
  if (is.null(resolver_index)) {
    resolver_index <- .scan_resolver_index(export_index, origin_map)
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
