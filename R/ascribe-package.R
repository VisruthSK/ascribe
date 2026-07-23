#' @keywords internal
"_PACKAGE"

# Allows testthat to replace namespace availability checks.
.ascribe_require_namespace <- function(package, quietly = FALSE) {
  base::requireNamespace(package, quietly = quietly)
}

## mockable bindings: start
## mockable bindings: end
NULL
