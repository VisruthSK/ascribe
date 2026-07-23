# Generate sysdata.rda for ascribe
#
# This script precomputes:
# - .stdlib_funs: exports from base R packages to ignore by default
# - .scan_skip_dirs: directory names to skip when scanning projects

# Precompute standard library functions
.stdlib_funs <- lapply(
  c("base", "stats", "utils", "graphics", "grDevices", "methods"),
  getNamespaceExports
) |>
  unlist(use.names = FALSE) |>
  unique() |>
  sort()

# Default skip directories
.scan_skip_dirs <- c(
  "renv",
  "packrat",
  "rv",
  ".Rcheck",
  "revdep",
  "_site",
  "_book",
  "_bookdown_files",
  "_freeze",
  ".quarto",
  ".quarto_cache",
  ".knitr_cache",
  "_cache",
  ".cache"
)

save(
  .stdlib_funs,
  .scan_skip_dirs,
  file = "R/sysdata.rda",
  compress = "xz"
)

message("Saved sysdata.rda")
