#' Collect exported functions and R6 methods from a package
#'
#' Returns a character vector of function names exported by `pkg`,
#' including methods of exported and namespace-internal R6 classes.
#'
#' @param pkg Package name (character scalar).
#' @return Character vector of function/method names.
#' @export
#' @examples
#' collect_pkg_funs("stats")
collect_pkg_funs <- function(pkg) {
  ns <- asNamespace(pkg)

  getNamespaceExports(pkg) |>
    Filter(
      \(x) {
        is.function(tryCatch(getExportedValue(ns, x), error = function(e) NULL))
      },
      x = _
    ) |>
    c(collect_r6_methods(pkg)) |>
    unique()
}

#' Collect R6 class method names from a package
#'
#' Scans exported and namespace-internal objects for R6 class generators,
#' then collects all public method names.
#'
#' @param pkg Package name (character scalar).
#' @return Character vector of R6 method names.
#' @keywords internal
collect_r6_methods <- function(pkg) {
  ns <- asNamespace(pkg)

  namespace_r6 <- ls(ns, all.names = TRUE) |>
    lapply(
      \(name) {
        obj <- tryCatch(
          get0(name, envir = ns, inherits = FALSE),
          error = function(e) NULL
        )
        if (inherits(obj, "R6ClassGenerator")) obj else NULL
      }
    ) |>
    Filter(Negate(is.null), x = _)

  exported_r6 <- getNamespaceExports(ns) |>
    lapply(
      \(name) {
        obj <- tryCatch(
          getExportedValue(ns, name),
          error = function(e) NULL
        )
        if (inherits(obj, "R6ClassGenerator")) obj else NULL
      }
    ) |>
    Filter(Negate(is.null), x = _)

  c(namespace_r6, exported_r6) |>
    lapply(\(gen) names(gen$public_methods)) |>
    unlist(use.names = FALSE) |>
    as.character() |>
    (\(methods) methods[!is.na(methods) & nzchar(methods)])() |>
    unique()
}

.resolve_origin_ns <- function(ns, name) {
  if (is.null(ns)) {
    return(NA_character_)
  }
  obj <- tryCatch(getExportedValue(ns, name), error = function(e) NULL)
  if (!is.function(obj)) {
    return(NA_character_)
  }

  env <- environment(obj)
  origin <- if (is.null(env)) "" else environmentName(env)
  origin <- sub("^namespace:", "", origin)
  if (!nzchar(origin)) {
    return(NA_character_)
  }
  origin
}

#' Resolve the origin package of an exported function
#'
#' Given a package and function name, determines which package the
#' function actually originates from (handling re-exports).
#'
#' @param pkg Package name (character scalar).
#' @param name Function name (character scalar).
#' @return The origin package name, or `NA_character_` if undetermined.
#' @export
#' @examples
#' resolve_origin("stats", "median")
resolve_origin <- function(pkg, name) {
  .resolve_origin_ns(
    tryCatch(asNamespace(pkg), error = function(e) NULL),
    name
  )
}

#' Build an inverted export index
#'
#' Given a named list mapping package names to character vectors of
#' function names (as produced by [collect_pkg_funs()]), creates an
#' inverted index mapping function names to character vectors of
#' packages that export them.
#'
#' @param exports Named list. Names are package names, values are
#'   character vectors of function names.
#' @return Named list mapping function names to character vectors of
#'   package names.
#' @export
#' @examples
#' exports <- list(
#'   pkgA = c("foo", "bar"),
#'   pkgB = c("foo", "baz")
#' )
#' build_export_index(exports)
build_export_index <- function(exports) {
  all_funs <- unlist(exports, use.names = FALSE)
  split(
    rep(names(exports), lengths(exports)),
    factor(all_funs, levels = unique(all_funs))
  )
}

#' Build an origin map for package functions
#'
#' Given a named list mapping package names to character vectors of
#' function names, creates a named character vector mapping
#' `"pkg::fun"` keys to the origin package. Functions whose origin
#' cannot be determined fall back to the providing package.
#'
#' @param exports Named list. Names are package names, values are
#'   character vectors of function names.
#' @return Environment mapping `"pkg::fun"` keys to origin package names.
#' @export
#' @examples
#' exports <- list(
#'   stats = collect_pkg_funs("stats"),
#'   utils = collect_pkg_funs("utils")
#' )
#' build_origin_map(exports)
build_origin_map <- function(exports) {
  all_funs <- unlist(exports, use.names = FALSE)
  all_pkgs <- rep(names(exports), lengths(exports))
  keys <- paste0(all_pkgs, "::", all_funs)

  u_pkgs <- unique(all_pkgs)
  ns_list <- stats::setNames(
    lapply(u_pkgs, \(p) tryCatch(asNamespace(p), error = function(e) NULL)),
    u_pkgs
  )

  resolve_origin_fast <- function(pkg, name) {
    .resolve_origin_ns(ns_list[[pkg]], name)
  }

  origins <- mapply(resolve_origin_fast, all_pkgs, all_funs, USE.NAMES = FALSE)

  # If origin is undetermined (NA), assume it is the provider package
  origins[is.na(origins)] <- all_pkgs[is.na(origins)]
  names(origins) <- keys
  list2env(as.list(origins), parent = emptyenv(), hash = TRUE)
}
