#' Cleans sourcoise cache
#'
#' removes every json and qs2 files found by `sourcoise_status()` unless a specific tibble (filtered from `sourcoise_status()`) is passed as an argument.
#'
#' @param what (--) a tibble such as the one obtained by `sourcoise_status()`, possibly filtered
#' @param root to force root, not recommended (expert use)
#'
#' @family sourcoise
#'
#' @return list of cleared files, plus a side-effect as specified cache files are deleted (no undo possible)
#' @export
#' @examplesIf rlang::is_installed("insee")
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#' fs::file_copy(
#'     fs::path_package("sourcoise", "some_data.R"),
#'     dir,
#'     overwrite = TRUE)
#' # Force execution
#' data <- sourcoise("some_data.R", force_exec = TRUE)
#' # we then clear all caches
#' sourcoise_clear()
#' sourcoise_status()

sourcoise_clear <- function(
    what = sourcoise_status(root=root, prune=FALSE),
    root = NULL) {

  root <- try_find_root(root)

  sure_delete <- function(fn) {
    if(fs::file_exists(fn))
      fs::file_delete(fn)
  }

  purrr::pmap_chr(what, function(src, json_file, data_file, root, ...) {
    sure_delete( fs::path_join(c(root, json_file) ))
    sure_delete( fs::path_join(c(root, data_file) ))
    src
  })
}

#' Resets sourcoise
#'
#' Removes all `.sourcoise` folders found under the project root.
#'
#' @param root to force root (expert use)
#'
#' @family sourcoise
#'
#' @return No return, effect is through removal of .sourcoise folders (this is a side effect, no undo possible)
#' @export
#' @examplesIf rlang::is_installed("insee")
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#' fs::file_copy(
#'    fs::path_package("sourcoise", "some_data.R"),
#'    dir,
#'    overwrite = TRUE)
#' data <- sourcoise("some_data.R", force_exec = TRUE)
#' sourcoise_reset()

sourcoise_reset <- function(
    root = NULL) {

  root <- try_find_root(root)

  caches_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$", all = TRUE, recurse = TRUE)

  purrr::walk(caches_reps, ~fs::dir_delete(.x))

}
