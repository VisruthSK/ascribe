#' Cite package and function use in a project
#'
#' Builds citations from [scan_usage()] results. Package collections supply
#' their own citation records and package-citation policy.
#'
#' @param usage Results returned by [scan_usage()].
#' @param package_citations An environment of package citation entries. Missing
#'   packages use `package_citation`.
#' @param function_citations An environment of function citation entries,
#'   keyed by `"pkg::function"`.
#' @param package_citation A function that accepts a package name and returns
#'   its citation entries. Defaults to [utils::citation()].
#' @param always_cite Character vector of packages to cite in addition to the
#'   packages found by the scan.
#' @param format One of `"bibtex"` or `"bibentry"`.
#' @return A BibTeX character vector or a bibentry object.
#' @export
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines("cli::cli_alert_info('hi'); fastmatch::fmatch(1, 1:5)", path)
#' universe <- build_universe_data(c("cli", "fastmatch"))
#' usage <- scan_usage(
#'   path,
#'   universe$packages,
#'   universe$export_index,
#'   universe$origin_map
#' )
#' cite_usage(usage)
#' unlink(path)
cite_usage <- function(
  usage,
  package_citations = new.env(parent = emptyenv(), hash = TRUE),
  function_citations = new.env(parent = emptyenv(), hash = TRUE),
  package_citation = utils::citation,
  always_cite = character(),
  format = c("bibtex", "bibentry")
) {
  pkgs <- unique(c(usage$packages, always_cite))
  if (!length(pkgs) && !length(usage$functions)) {
    return(character())
  }

  entries <- c(
    lapply(unique(c(pkgs, "base")), \(pkg) {
      entry <- get0(pkg, envir = package_citations, ifnotfound = NULL)
      if (is.null(entry)) {
        if (pkg == "base") utils::citation("base") else package_citation(pkg)
      } else {
        entry
      }
    }),
    lapply(usage$functions, \(fun) {
      get0(fun, envir = function_citations, ifnotfound = NULL)
    })
  ) |>
    Filter(Negate(is.null), x = _) |>
    do.call(c, args = _) |>
    unique()

  if (match.arg(format) == "bibentry") entries else utils::toBibtex(entries)
}
