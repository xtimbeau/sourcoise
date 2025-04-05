.onLoad <- function (libname, pkgname) {
  op <- options()
  op.sourcoise <- list(
    sourcoise.force_exec = FALSE,
    sourcoise.prevent_exec = FALSE,
    sourcoise.metadata = FALSE,
    sourcoise.init_fn = NULL,
    sourcoise.lapse = "never",
    sourcoise.src_in = "project",
    sourcoise.wd = "file",
    sourcoise.unfreeze = FALSE,
    sourcoise.log = "OFF",
    sourcoise.root = NULL,
    sourcoise.nocache = FALSE,
    sourcoise.grow_cache = 5,
    sourcoise.limit_mb = 50,
    sourcoise.nthreads = 1,
    sourcoise.memoize = TRUE,
    sourcoise.encoding = "UTF-8"
  )

  toset <- !(names(op.sourcoise) %in% names(op))
  if (any(toset)) options(op.sourcoise[toset])

  if(rlang::is_installed("memoise"))
    read_data_from_cache <<- memoise::memoise(read_data_from_cache)
}
