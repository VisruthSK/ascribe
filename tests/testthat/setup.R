options(cli.default_handler = function(msg) invisible(NULL))

test_universe <- function(packages, export_index = list(), origin_map = NULL) {
  if (is.null(origin_map)) {
    origin_map <- new.env(parent = emptyenv())
  }
  # Build resolver_index from export_index and origin_map
  resolver_index <- .scan_resolver_index(export_index, origin_map)
  # Build walker_envs
  export_names <- names(export_index)
  if (is.null(export_names)) {
    export_names <- character()
  }
  walker_envs <- .make_walker_envs(packages, export_names)

  list(
    packages = packages,
    export_index = export_index,
    origin_map = origin_map,
    resolver_index = resolver_index,
    walker_envs = walker_envs
  )
}
