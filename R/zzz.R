.onLoad <- function (libname, pkgname) {
  op <- options()
  op.sourcoise <- list(
    sourcoise.force_exec = FALSE,
    sourcoise.prevent_exec = FALSE,
    sourcoise.cache_rep = ".data",
    sourcoise.hash = TRUE,
    sourcoise.metadata = FALSE,
    sourcoise.lapse = "never",
    sourcoise.src_in = "project",
    sourcoise.wd = "file",
    sourcoise.unfreeze = FALSE
    )

  toset <- !(names(op.sourcoise) %in% names(op))
  if (any(toset)) options(op.sourcoise[toset])
}
