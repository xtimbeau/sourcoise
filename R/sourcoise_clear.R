#' CLeans cache
#'
#' removes every json and qs2 files found by `sourcoise_status()` unless a specfiic tibble (filtered from `sourcoise_status()`) is passed as an argument.
#'
#' @param what (--) a tibble such as the one obtained by `sourcoise_status()`, possibly filtered
#' @param root to force root, not recommanded (expert use)
#'
#' @family sourcoise
#'
#' @return list of cleared files
#' @export

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

#' Reset sourcoise
#'
#' Removes all `.sourcoise` folders found under the project root.
#'
#' @param root to force root (expert use)
#'
#' @family sourcoise
#'
#' @return NULL
#' @export

sourcoise_reset <- function(
    root = NULL) {

  root <- try_find_root(root)

  caches_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$", all = TRUE, recurse = TRUE)

  purrr::walk(caches_reps, ~fs::dir_delete(.x))

}


