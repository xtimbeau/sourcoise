#' Etat du cache de sourcoise
#'
#' Donne des informations sur le cache de source_data sous la forme d'un tibble
#'
#' @param src_in est ce que les données sont avec les qmd ?
#' @param quiet (boléen) TRUE par défaut, ne dit rien
#' @param root force le root -- à ne pas utiliser sauf expert
#' @param prune (boléen) limite la liste au dernier cache (TRUE par défaut)
#' @family sourcoise
#'
#' @return tibble des fichiers en cache avec les informations des scripts appelants
#' @export
#'

sourcoise_status <- function(
    quiet = TRUE,
    root = NULL,
    src_in = getOption("sourcoise.src_in") %||% "project",
    prune = TRUE) {
  root <- try_find_root(root, src_in)
  caches_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$", all = TRUE, recurse = TRUE)
  roots <- fs::path_dir(caches_reps)
  caches_reps <- set_names(caches_reps, roots)

  jsons <- map(caches_reps,
                ~fs::dir_ls(.x, glob = "*.json", recurse = TRUE))
  qs2 <- map(caches_reps,
             ~fs::dir_ls(.x, glob = "*.qs2", recurse = TRUE))

  if(length(roots)>0) {
    cached <- purrr::map_dfr(roots, \(a_root) {
      purrr::map_dfr(jsons[[a_root]], ~{
        dd <- jsonlite::read_json(.x) |>
          purrr::map( ~if(length(.x)>1) purrr::list_flatten(.x) else unlist(.x))
        valid <- valid_meta4meta(dd, root = a_root)

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
          json_file = fs::path_rel(.x, a_root),
          qmd_file = list(dd$qmd_files),
          src_in = dd$src_in,
          data_file = dd$data_file,
          root =  a_root,
          src_hash = dd$src_hash,
          track_hash = list(dd$track_hash),
          track = list(dd$track),
          args_hash = dd$args_hash,
          data_hash = dd$data_hash)
      })
    }) |>
      dplyr::arrange(src, dplyr::desc(date))

    qs2_jsoned <- purrr::pmap_chr(cached, \(root, data_file, ...) {fs::path_join(c(root, data_file))})
    qs2_orphed <- setdiff(qs2 |> purrr::list_c(), qs2_jsoned)
    purrr::walk(qs2_orphed, fs::file_delete)

    if(prune)
      cached <- cached |>
        dplyr::mutate(src = tolower(src)) |>
        dplyr::group_by(src) |>
        dplyr::filter(date == max(date)) |>
        dplyr::ungroup()
    return(cached)
  } else {
    if(!quiet)
      cli::cli_alert_danger("Pas de cache trouvé")
    return(tibble::tibble())
  }
}
