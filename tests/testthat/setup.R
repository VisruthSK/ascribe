options(cli.default_handler = function(msg) invisible(NULL))

test_universe <- function(packages, export_index = list(), origin_map = NULL) {
  if (is.null(origin_map)) {
    origin_map <- new.env(parent = emptyenv())
  }
  list(
    packages = packages,
    export_index = export_index,
    origin_map = origin_map
  )
}
