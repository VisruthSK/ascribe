#' Cite package and function use in a project
#'
#' Builds citations from [scan_usage()] results. Package collections supply
#' their own citation records and package-citation policy.
#'
#' @param usage Results returned by [scan_usage()].
#' @param package_citations A named list or environment of package citation
#'   entries. Missing packages use `package_citation`.
#' @param function_citations A named list or environment of function citation
#'   entries, keyed by `"pkg::function"`.
#' @param package_citation A function that accepts a package name and returns
#'   its citation entries. Defaults to [utils::citation()].
#' @param always_cite Character vector of packages to cite in addition to the
#'   packages found by the scan.
#' @param format One of `"bibtex"` or `"bibentry"`.
#' @return A BibTeX character vector or a bibentry object.
#' @export
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines("stats::median(1:3)", path)
#' universe <- build_universe_data(c("stats", "tools"))
#' usage <- scan_usage(path, universe$packages, universe$export_index, universe$origin_map)
#' cite_usage(usage)
#' unlink(path)
cite_usage <- function(
  usage,
  package_citations = list(),
  function_citations = list(),
  package_citation = utils::citation,
  always_cite = character(),
  format = c("bibtex", "bibentry")
) {
  entries <- c(
    lapply(unique(c(usage$packages, always_cite)), \(pkg) {
      entry <- package_citations[[pkg]]
      if (is.null(entry)) {
        package_citation(pkg)
      } else {
        entry
      }
    }),
    lapply(usage$functions, \(fun) function_citations[[fun]])
  ) |>
    Filter(Negate(is.null), x = _)

  if (!length(entries)) {
    return(character())
  }

  entries <- do.call(c, entries) |> c(utils::citation("base"))
  if (match.arg(format) == "bibentry") entries else utils::toBibtex(entries)
}
