# Fresh, isolated sourcoise project (temp dir used as root) for the calling test
local_project <- function(name = "proj", env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  dir <- file.path(base, name)
  dir.create(dir)
  old <- options(sourcoise.root = NULL)
  withr::defer(options(old), envir = env)
  set_sourcoise_root(dir)
  as.character(fs::path_norm(dir))
}

write_script <- function(dir, file, code) {
  writeLines(code, file.path(dir, file))
}

cache_files <- function(dir, pattern) {
  list.files(file.path(dir, ".sourcoise"), pattern = pattern, recursive = TRUE)
}
