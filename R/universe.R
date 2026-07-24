#' Build scanner data for a package universe
#'
#' Given a character vector of package names, computes the export
#' lists, inverted export index, origin map, and version snapshot
#' needed by [scan_usage()]. All packages must be installed.
#'
#' @param packages Character vector of package names.
#' @return A named list with components:
#'   \describe{
#'     \item{packages}{The input package names.}
#'     \item{exports}{Named list mapping package names to character
#'       vectors of exported function names (from [collect_pkg_funs()]).}
#'     \item{export_index}{Named list mapping function names to
#'       character vectors of packages (from [build_export_index()]).}
#'     \item{origin_map}{Environment mapping `"pkg::fun"` keys
#'       to origin packages (from [build_origin_map()]).}
#'     \item{pkg_versions}{Named list mapping package names to version
#'       strings.}
#'   }
#' @export
#' @examples
#' build_universe_data(c("stats", "utils"))
build_universe_data <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    cli::cli_abort("The following packages are not installed: {.pkg {missing}}")
  }

  exports <- stats::setNames(lapply(packages, collect_pkg_funs), packages)
  export_index <- build_export_index(exports)
  origin_map <- build_origin_map(exports)

  pkg_versions <- stats::setNames(
    lapply(packages, \(p) as.character(utils::packageVersion(p))),
    packages
  )

  list(
    packages = packages,
    exports = exports,
    export_index = export_index,
    origin_map = origin_map,
    pkg_versions = pkg_versions
  )
}

#' Generate sysdata.rda for a package universe
#'
#' Computes scanner data for the given packages and saves it to
#' `sysdata.rda` with variable names prefixed by `prefix`. This is
#' intended for use in a downstream package's `data-raw/sysdata.R`
#' script.
#'
#' The generated variables are:
#' \describe{
#'   \item{`.{prefix}_pkgs`}{Character vector of package names.}
#'   \item{`.{prefix}_exports`}{Named list of exported functions per package.}
#'   \item{`.{prefix}_export_index`}{Inverted index: function name to packages.}
#'   \item{`.{prefix}_origin_map`}{Environment: `"pkg::fun"` keys to origin.}
#'   \item{`.{prefix}_pkg_versions`}{Named list of version strings.}
#' }
#'
#' When `include_scanner_defaults` is `TRUE`, `.stdlib_funs` and
#' `.scan_skip_dirs` are also saved.
#'
#' @param packages Character vector of package names.
#' @param prefix Character scalar used to name the saved objects
#'   (e.g., `"stan"` produces `.stan_pkgs`).
#' @param extra_vars Named list of additional objects to include in
#'   the saved file (e.g., citation environments).
#' @param include_scanner_defaults If `TRUE`, also saves `.stdlib_funs`
#'   and `.scan_skip_dirs`. Defaults to `FALSE`.
#' @param file Output path. Defaults to `"R/sysdata.rda"`.
#' @return Invisibly returns the result of [build_universe_data()].
#' @export
#' @examples
#' file <- tempfile(fileext = ".rda")
#' generate_universe_sysdata(c("stats", "utils"), "my", file = file)
#' unlink(file)
generate_universe_sysdata <- function(
  packages,
  prefix,
  extra_vars = list(),
  include_scanner_defaults = FALSE,
  file = "R/sysdata.rda"
) {
  data <- build_universe_data(packages)

  vars <- list()
  vars[[paste0(".", prefix, "_pkgs")]] <- data$packages
  vars[[paste0(".", prefix, "_exports")]] <- data$exports
  vars[[paste0(".", prefix, "_export_index")]] <- data$export_index
  vars[[paste0(".", prefix, "_origin_map")]] <- data$origin_map
  vars[[paste0(".", prefix, "_pkg_versions")]] <- data$pkg_versions

  if (include_scanner_defaults) {
    vars[[".stdlib_funs"]] <- stdlib_funs()
    vars[[".scan_skip_dirs"]] <- scan_skip_dirs()
  }

  if (length(extra_vars) > 0) {
    for (nm in names(extra_vars)) {
      vars[[nm]] <- extra_vars[[nm]]
    }
  }

  save_env <- list2env(vars, parent = emptyenv())
  save(
    list = names(vars),
    envir = save_env,
    file = file,
    compress = "xz"
  )

  cli::cli_alert_success("Successfully generated {.file {file}}")
  invisible(data)
}
