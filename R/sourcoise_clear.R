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
#' @examples
#'   fs::file_copy(
#'     fs::path_package("sourcoise", "ipch", "prix_insee.r"),
#'     "/tmp/prix_insee.r",
#'     overwrite = TRUE)
#'   # Force execution (root is set explicitly here, it is normally deduced from project)
#'   data <- sourcoise("prix_insee.r", root = "/tmp/", force_exec = TRUE)
#'   # we then clear all caches
#'   sourcoise_clear(root = "/tmp")
#'   sourcoise_status(root = "/tmp")

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
#' @examples
#'   fs::file_copy(
#'     fs::path_package("sourcoise", "ipch", "prix_insee.r"),
#'     "/tmp/prix_insee.r",
#'     overwrite = TRUE)
#'   data <- sourcoise("prix_insee.r", root = "/tmp/", force_exec = TRUE)
#'   sourcoise_reset(root = "/tmp/")

sourcoise_reset <- function(
    root = NULL) {

  root <- try_find_root(root)

  caches_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$", all = TRUE, recurse = TRUE)

  purrr::walk(caches_reps, ~fs::dir_delete(.x))

}


