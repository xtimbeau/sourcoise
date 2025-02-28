.onLoad <- function (libname, pkgname) {
  op <- options()
  op.sourcoise <- list(
    sourcoise.force_exec = FALSE,
    sourcoise.prevent_exec = FALSE,
    sourcoise.cache_rep = ".sourcoise",
    sourcoise.hash = TRUE,
    sourcoise.metadata = FALSE,
    sourcoise.lapse = "never",
    sourcoise.src_in = "project",
    sourcoise.wd = "file",
    sourcoise.unfreeze = FALSE,
    sourcoise.log = "OFF",
    sourcoise.grow_cache = 3,
    sourcoise.limit_mb = 50,
    sourcoise.nthreads = 1
    )

  toset <- !(names(op.sourcoise) %in% names(op))
  if (any(toset)) options(op.sourcoise[toset])
}
