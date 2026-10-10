#' Cite package and function use in a project
#'
#' Builds citations from [scan_usage()] results. Package collections supply
#' their own citation records and package-citation policy.
#'
#' @param usage Results returned by [scan_usage()] or [scan_code()].
#' @param package_citations An environment of package citation entries. Missing
#'   packages use `package_citation`. If `universe` is provided and this is `NULL`,
#'   uses `universe$package_citations`. Explicit argument overrides universe.
#' @param function_citations An environment of function citation entries,
#'   keyed by `"pkg::function"`. If `universe` is provided and this is `NULL`,
#'   uses `universe$function_citations`. Explicit argument overrides universe.
#' @param package_citation A function that accepts a package name and returns
#'   its citation entries. Defaults to [utils::citation()].
#' @param always_cite Character vector of packages to cite in addition to the
#'   packages found by the scan.
#' @param format One of `"bibtex"` or `"bibentry"`.
#' @param cite_r Whether to include an automatic R base citation. Defaults
#'   to `TRUE`.
#' @param universe An `ascribe_universe` object returned by [build_universe_data()].
#'   If provided, citation mappings stored in the universe are used as defaults.
#'   Defaults to `NULL`.
#' @return A BibTeX character vector or a bibentry object.
#' @export
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines("cli::cli_alert_info('hi'); fastmatch::fmatch(1, 1:5)", path)
#' universe <- build_universe_data(c("cli", "fastmatch"))
#' usage <- scan_usage(path, universe)
#' cite_usage(usage)
#' cite_usage(usage, cite_r = FALSE)
#' unlink(path)
cite_usage <- function(
  usage,
  package_citations = NULL,
  function_citations = NULL,
  package_citation = utils::citation,
  always_cite = character(),
  format = c("bibtex", "bibentry"),
  cite_r = TRUE,
  universe = NULL
) {
  pkgs <- unique(c(usage$packages, always_cite))
  if (!length(pkgs) && !length(usage$functions)) {
    return(character())
  }

  if (!is.null(universe)) {
    if (is.null(package_citations)) {
      package_citations <- universe$package_citations
    }
    if (is.null(function_citations)) {
      function_citations <- universe$function_citations
    }
  }

  if (is.null(package_citations)) {
    package_citations <- usage$package_citations
  }
  if (is.null(function_citations)) {
    function_citations <- usage$function_citations
  }

  if (is.null(package_citations)) {
    package_citations <- new.env(parent = emptyenv(), hash = TRUE)
  }
  if (is.null(function_citations)) {
    function_citations <- new.env(parent = emptyenv(), hash = TRUE)
  }

  base_pkgs <- if (cite_r) "base" else character()
  entries <- c(
    lapply(unique(c(pkgs, base_pkgs)), function(pkg) {
      entry <- get0(
        pkg,
        envir = package_citations,
        inherits = FALSE,
        ifnotfound = NULL
      )
      if (is.null(entry)) {
        if (pkg == "base") utils::citation("base") else package_citation(pkg)
      } else {
        entry
      }
    }),
    lapply(usage$functions, function(fun) {
      get0(fun, envir = function_citations, inherits = FALSE, ifnotfound = NULL)
    })
  )

  entries <- unique(do.call(c, Filter(Negate(is.null), entries)))

  if (match.arg(format) == "bibentry") entries else utils::toBibtex(entries)
}
