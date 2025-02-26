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
    what = sourcoise_status(root=root),
    cache_rep = NULL,
    root = NULL) {

  safe_find_root <- purrr::safely(rprojroot::find_root)
  root <- safe_find_root(rprojroot::is_quarto_project | rprojroot::is_r_package | rprojroot::is_rstudio_project)

  if(is.null(root$error))
    root <- root$result
  else {
    cli::cli_alert_warning("{root$error}")
    return(NULL)
  }
  root <- fs::path_norm(root)
  abs_cache_rep <- fs::path_join(c(root, cache_rep)) |> fs::path_norm()

  purrr::pmap_chr(what, function(src, id, ...) {
    fn <- fs::path_join(c(abs_cache_rep, src)) |>
      fs::path_ext_remove() |>
      stringr::str_c("_", id)
    fs::file_delete(fn |> fs::path_ext_set("json"))
    fs::file_delete(fn |> fs::path_ext_set("qs"))
    fn
  })
}
