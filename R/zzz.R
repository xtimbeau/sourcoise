.onLoad <- function (libname, pkgname) {
  op <- options()
  op.sourcoise <- list(
    sourcoise.force_exec = FALSE,
    sourcoise.prevent_exec = FALSE,
    sourcoise.metadata = FALSE,
    sourcoise.init_fn = NULL,
    sourcoise.src_in = "project",
    sourcoise.wd = "file",
    sourcoise.unfreeze = FALSE,
    sourcoise.log = "OFF",
    sourcoise.quiet = FALSE,
    sourcoise.root = NULL,
    sourcoise.nocache = FALSE,
    sourcoise.grow_cache = Inf,
    sourcoise.limit_mb = 50,
    sourcoise.nthreads = 1,
    sourcoise.memoize = TRUE,
    sourcoise.encoding = "UTF-8"
  )

  toset <- !(names(op.sourcoise) %in% names(op))
  if (any(toset)) options(op.sourcoise[toset])

  if(rlang::is_installed("memoise")) {
    read_meta1_valid <<- memoise::memoise(read_meta1_valid)
  }
}

utils::globalVariables(c("json_file", "index", "short_json_file", "uid",  "src", "cache_rep", "upcache_rep", "data_file",
                         "full_cache_rep", "ffn", "cur_src_hash", "track", "lapse", "cur_arg_hash",
                         "cur_track_hash", "log_file", "src_hash", "arg_hash", "track_hash", "valid_src",
                         "valid_args", "valid_track", "src_exist", "data_exist", "valid", "priority",
                         "cc", "timing", "size", "wd",  "qmd_file", "src_in", "data_date", "file_size",
                         "data_hash", "uid", "argid", "src",  "json_file", "name", "last_call", "last_update"))

utils::globalVariables(c(".data"))

.datatable.aware <- TRUE
