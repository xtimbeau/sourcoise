#' Vide le cache
#'
#' @param what (--) un tibble issu de source_data, éventuellement filtré
#' @param cache_rep le répertoire de cache
#' @param root pour forcer le root (non recommandé)
#'
#' @family sourcoise
#'
#' @return la liste des fichiers supprimés
#' @export
#'
#'
#'
sourcoise_clear <- function(
    what = sourcoise_status(root=root, prune=FALSE),
    cache_rep = NULL,
    root = NULL) {

  root <- find_project_root()$project_path

  root <- fs::path_norm(root)

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
