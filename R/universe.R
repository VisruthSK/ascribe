#' Build scanner data for a package universe
#'
#' Given a character vector of package names, computes the export
#' lists, inverted export index, origin map, and version snapshot
#' needed by [scan_usage()]. All packages must be installed.
#'
#' @param packages Character vector of package names.
#' @param metapackages Named list or environment mapping metapackage names to
#'   character vectors of member packages. Every member must exist in `packages`.
#'   Defaults to `NULL`.
#' @param package_citations Named list or environment mapping package names to
#'   citation entries (bibentry objects). Defaults to empty.
#' @param function_citations Named list or environment mapping `"pkg::fun"` keys
#'   to citation entries. Defaults to empty.
#' @return An `ascribe_universe` object with components:
#'   \describe{
#'     \item{packages}{The input package names.}
#'     \item{exports}{Named list mapping package names to character
#'       vectors of exported function names (from [collect_pkg_funs()]).}
#'     \item{export_index}{Named list mapping function names to
#'       character vectors of packages (from [build_export_index()]).}
#'     \item{origin_map}{Environment mapping `"pkg::fun"` keys
#'       to origin packages (from [build_origin_map()]).}
#'     \item{pkg_versions}{Named character vector mapping package names to version
#'       strings.}
#'     \item{metapackages}{Environment mapping metapackage names to member packages.}
#'     \item{package_citations}{Environment mapping package names to citation entries.}
#'     \item{function_citations}{Environment mapping `"pkg::fun"` keys to citation entries.}
#'     \item{resolver_index}{Precomputed resolver index for function resolution.}
#'   }
#' @export
#' @examples
#' build_universe_data(c("stats", "utils"))
build_universe_data <- function(
  packages,
  metapackages = NULL,
  package_citations = NULL,
  function_citations = NULL
) {
  if (!is.character(packages)) {
    cli::cli_abort("{.arg packages} must be a character vector")
  }
  dropped <- is.na(packages) | !nzchar(packages) | duplicated(packages)
  if (any(dropped)) {
    cli::cli_warn("Dropping missing, empty, or duplicate package names")
  }
  packages <- unique(packages[!is.na(packages) & nzchar(packages)])

  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    cli::cli_abort("The following packages are not installed: {.pkg {missing}}")
  }

  exports <- stats::setNames(lapply(packages, collect_pkg_funs), packages)
  export_index <- build_export_index(exports)
  origin_map <- build_origin_map(exports)

  pkg_versions <- vapply(
    packages,
    function(p) as.character(utils::packageVersion(p)),
    character(1)
  )

  metapackages <- .normalize_metapackages_env(metapackages, packages)
  package_citations <- .normalize_citations_env(package_citations)
  function_citations <- .normalize_citations_env(function_citations)

  resolver_index <- .scan_resolver_index(export_index, origin_map)
  structure(
    list(
      packages = packages,
      exports = exports,
      export_index = export_index,
      origin_map = origin_map,
      pkg_versions = pkg_versions,
      metapackages = metapackages,
      package_citations = package_citations,
      function_citations = function_citations,
      resolver_index = resolver_index
    ),
    class = "ascribe_universe"
  )
}

#' @export
print.ascribe_universe <- function(x, ...) {
  cli::cli_text("<ascribe_universe>")
  n_metapkgs <- length(ls(x$metapackages, all.names = TRUE))
  cli::cli_bullets(c(
    "v" = "Packages: {length(x$packages)}",
    "v" = "Metapackages: {n_metapkgs}"
  ))
  invisible(x)
}

.normalize_metapackages_env <- function(metapackages, allowed_packages) {
  env <- new.env(parent = emptyenv(), hash = TRUE)
  if (is.null(metapackages)) {
    return(env)
  }

  if (is.environment(metapackages)) {
    nms <- ls(metapackages, all.names = TRUE)
  } else if (is.list(metapackages) && !is.null(names(metapackages))) {
    nms <- names(metapackages)
  } else {
    cli::cli_abort("{.arg metapackages} must be a named list or environment")
  }

  for (nm in nms) {
    if (!nzchar(nm)) {
      cli::cli_warn("Dropping metapackage with an empty name")
      next
    }
    members <- metapackages[[nm]]
    if (!is.character(members)) {
      cli::cli_warn(
        "Dropping metapackage {.pkg {nm}}: members must be character"
      )
      next
    }
    bad <- setdiff(members, allowed_packages)
    if (length(bad) > 0) {
      cli::cli_warn(
        "Dropping unknown packages from metapackage {.pkg {nm}}: {.pkg {bad}}"
      )
      members <- members[!members %in% bad]
    }
    if (anyDuplicated(members)) {
      cli::cli_warn("Dropping duplicate members from metapackage {.pkg {nm}}")
      members <- unique(members)
    }
    env[[nm]] <- members
  }
  env
}

.normalize_citations_env <- function(citations) {
  env <- new.env(parent = emptyenv(), hash = TRUE)
  if (is.null(citations)) {
    return(env)
  }

  if (is.environment(citations)) {
    nms <- ls(citations, all.names = TRUE)
  } else if (is.list(citations) && !is.null(names(citations))) {
    nms <- names(citations)
  } else {
    cli::cli_abort("Citations must be a named list or environment")
  }

  for (nm in nms) {
    if (!nzchar(nm)) {
      cli::cli_abort("Citation names must be non-empty")
    }
    env[[nm]] <- citations[[nm]]
  }
  env
}

#' Generate sysdata.rda for a package universe
#'
#' Computes scanner data for the given packages and saves it to
#' `sysdata.rda` with variable names prefixed by `prefix`. This is
#' intended for use in a downstream package's `data-raw/sysdata.R`
#' script.
#'
#' The generated variable is:
#' \describe{
#'   \item{`.{prefix}_universe`}{Complete `ascribe_universe` object.}
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
#' @param file Output path (required).
#' @param metapackages Named list or environment mapping metapackage names to
#'   member packages. Defaults to `NULL`.
#' @param package_citations Named list or environment of package citation entries.
#'   Defaults to empty.
#' @param function_citations Named list or environment of function citation entries.
#'   Defaults to empty.
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
  file,
  metapackages = NULL,
  package_citations = NULL,
  function_citations = NULL
) {
  data <- build_universe_data(
    packages,
    metapackages = metapackages,
    package_citations = package_citations,
    function_citations = function_citations
  )

  vars <- list()
  vars[[paste0(".", prefix, "_universe")]] <- data

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

#' Check universe freshness against installed packages
#'
#' Compares the package versions stored in the universe against
#' currently installed versions.
#'
#' @param universe An `ascribe_universe` object.
#' @return A data frame with columns:
#'   \describe{
#'     \item{package}{Package name.}
#'     \item{indexed_version}{Version stored in the universe.}
#'     \item{installed_version}{Currently installed version, or `NA` if not installed.}
#'     \item{status}{One of `current`, `changed`, `unavailable`.}
#'   }
#' @export
universe_status <- function(universe) {
  pkgs <- universe$packages
  indexed_versions <- universe$pkg_versions

  installed_versions <- vapply(
    pkgs,
    function(p) {
      if (requireNamespace(p, quietly = TRUE)) {
        as.character(utils::packageVersion(p))
      } else {
        NA_character_
      }
    },
    character(1)
  )

  status <- ifelse(
    is.na(installed_versions),
    "unavailable",
    ifelse(installed_versions == indexed_versions, "current", "changed")
  )

  data.frame(
    package = pkgs,
    indexed_version = indexed_versions,
    installed_version = installed_versions,
    status = status,
    stringsAsFactors = FALSE
  )
}
