ExportedClass <- R6::R6Class(
  "ExportedClass",
  public = list(
    exported_method = function() "exported"
  )
)

InternalClass <- R6::R6Class(
  "InternalClass",
  public = list(
    internal_method = function() "internal"
  )
)
