#' Etat du cache de sourcoise
#'
#' Donne des informations sur le cache de source_data sous la forme d'un tibble
#'
#' @param src_in est ce que les données sont avec les qmd ?
#' @param quiet (boléen) TRUE par défaut, ne dit rien
#' @param root force le root -- à ne pas utiliser sauf expert
#' @param cache_rep le répertoire de cache -- à ne pas utiliser sauf expert
#' @param prune (boléen) limite la liste au dernier cache (TRUE par défaut)
#' @family sourcoise
#'
#' @return tibble des fichiers en cache avec les informations des scripts appelants
#' @export
#'

sourcoise_status <- function(
    cache_rep = NULL,
    quiet = TRUE,
    root = NULL,
    src_in = getOption("sourcoise.src_in") %||% "project",
    prune = TRUE) {

  root <- try_find_root(root, src_in)
  a_caches <- list()
  z_caches <- list()
  if(is.null(cache_rep))
    cache_rep <- fs::path_join(c(root, ".data"))

  if(!quiet)
    cli::cli_alert_info("répertoire cache {.file {cache_rep}}")

  if(fs::dir_exists(cache_rep))
    a_caches <- fs::dir_ls(path = cache_rep, glob = "*.json", recurse = TRUE)

  if(length(a_caches)>0)
      a_caches <- rlang::set_names(list(a_caches), root)

  qmds <- fs::dir_ls(root, glob = "*.qmd", recurse = TRUE)
  qmds_folders <- unique(qmds |> fs::path_dir()) |> setdiff(root)

  folders <- qmds_folders |>
    purrr::discard(~stringr::str_detect(.x, "^_|/_")) |>
    purrr::keep(~fs::dir_exists(fs::path_join(c(.x, ".data"))))
  folders <- rlang::set_names(folders)

  if(length(folders)>0)
    z_caches <- purrr::imap(
      folders,
      ~fs::dir_ls(path = fs::path_join(c(.x, ".data")), glob = "*.json", recurse = TRUE))

  caches <- c(a_caches, z_caches)

  if(length(caches)>0) {
    cached <- purrr::map_dfr(names(caches), \(root) {
      purrr::map_dfr(caches[[root]], ~{
        dd <- jsonlite::read_json(.x) |>
          purrr::map( ~if(length(.x)>1) purrr::list_flatten(.x) else unlist(.x))
        valid <- valid_meta4meta(dd, root = root)

        tibble::tibble(
          src = dd$src,
          date = lubridate::as_datetime(dd$date),
          valid = valid$valid,
          id = dd$id,
          uid = dd$uid,
          index = dd$cc |> as.numeric(),
          timing = dd$timing,
          size = scales::label_bytes()(dd$size),
          lapse = dd$lapse |> as.character(),
          wd = dd$wd,
          args = list(dd$args),
          json_file = fs::path_rel(.x, root),
          qmd_file = list(dd$qmd_file),
          src_in = dd$src_in,
          data_file = dd$data_file,
          root =  dd$root %||% ".",
          src_hash = dd$src_hash,
          track_hash = list(dd$track_hash),
          track = list(dd$track),
          args_hash = dd$args_hash,
          data_hash = dd$data_hash)
      })
    }) |>
      dplyr::arrange(src, dplyr::desc(date))
    if(prune)
      cached <- cached |>
        dplyr::group_by(tolower(src)) |>
        dplyr::filter(date == max(date)) |>
        dplyr::ungroup()
    return(cached)
  } else {
    if(!quiet)
      cli::cli_alert_danger("Pas de cache trouvé")
    return(tibble::tibble())
  }
}
