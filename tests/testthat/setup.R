options(cli.default_handler = function(msg) invisible(NULL))

# Single fixture installation helper - accepts one or more fixture names
with_fixtures <- function(fixture_names, code) {
  skip_if_not_installed("R6")

  tmp_lib <- tempfile()
  dir.create(tmp_lib)
  old_lib <- .libPaths()

  on.exit(
    {
      # Unload fixture namespaces in reverse order
      for (pkg in rev(fixture_names)) {
        if (isNamespaceLoaded(pkg)) {
          unloadNamespace(pkg)
        }
      }
      .libPaths(old_lib)
      unlink(tmp_lib, recursive = TRUE)
    },
    add = TRUE
  )

  .libPaths(c(tmp_lib, old_lib))

  for (fixture_name in fixture_names) {
    fixture_dir <- testthat::test_path("fixtures", fixture_name)
    utils::install.packages(
      fixture_dir,
      repos = NULL,
      type = "source",
      lib = tmp_lib,
      INSTALL_opts = c("--no-staged-install", "--no-test-load"),
      quiet = TRUE
    )
  }

  code()
}

# Internal helpers copied from ascribe for test_universe
.scan_resolver_index <- function(export_index, origin_map) {
  funs <- names(export_index)
  if (is.null(funs) || length(funs) == 0L) {
    return(list())
  }

  lens <- lengths(export_index)
  n_funs <- length(funs)
  has_map <- !is.null(origin_map) && length(origin_map) > 0L
  res <- vector("list", n_funs)

  get_map_val <- function(key) {
    get0(key, envir = origin_map, inherits = FALSE, ifnotfound = NULL)
  }

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

# Test universe builder - creates a minimal ascribe_universe for testing
test_universe <- function(
  packages,
  export_index = list(),
  origin_map = NULL,
  metapackage_values = NULL
) {
  if (is.null(origin_map)) {
    origin_map <- new.env(parent = emptyenv())
  }

  exports <- stats::setNames(
    lapply(packages, function(p) character()),
    packages
  )
  for (fun in names(export_index)) {
    for (pkg in export_index[[fun]]) {
      exports[[pkg]] <- unique(c(exports[[pkg]], fun))
    }
  }

  # Build export_index from exports if not provided
  if (length(export_index) == 0) {
    export_index <- build_export_index(exports)
  }

  # Build origin_map if not provided
  if (length(ls(origin_map, all.names = TRUE)) == 0) {
    origin_map <- build_origin_map(exports)
  }

  # Create minimal ascribe_universe structure
  pkg_versions <- stats::setNames(rep("0.0.0", length(packages)), packages)
  metapackages <- new.env(parent = emptyenv(), hash = TRUE)
  if (!is.null(metapackage_values)) {
    list2env(metapackage_values, envir = metapackages)
  }
  package_citations <- new.env(parent = emptyenv(), hash = TRUE)
  function_citations <- new.env(parent = emptyenv(), hash = TRUE)
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
