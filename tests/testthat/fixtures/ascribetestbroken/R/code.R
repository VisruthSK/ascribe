# jarl-ignore unused_function: package load hook called by R
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  makeActiveBinding("broken", function() stop("boom"), env = ns)
}

SomeClass <- R6::R6Class(
  "SomeClass",
  public = list(
    method = function() "upstream"
  )
)
