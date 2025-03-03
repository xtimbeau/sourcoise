# refresh -----------------------

#' Exécute les sources sélectionnés
#'
#' @param what un tibble issu de source_data (tout par défaut)
#' @param cache_rep le répertoire de cache si il n'est pas évident
#' @param force_exec (boléen) Si `TRUE` alors le code est exécuté (FALSE par défaut)
#' @param hash (boléen) (`TRUE` par défaut) vérifie les hashs
#' @param unfreeze (boléen) (`TRUE` par défaut) essaye de unfreezé et uncaché les qmd dont les données ont été rafraichies
#' @param quiet reste silencieux
#' @param init_qmd (`TRUE` par défaut) exécute `ofce::init_qmd()`
#' @param root (`NULL` par défaut) essaye de trouver le root à partir du point d'exécution ou utilise celui fournit
#' @param log ("INFO" par défaut) niveau de log (voir `logger::log_threshold()`)
#' @param .progress (TRUE par défaut) affiche une barre de progression
#'
#' @family sourcoise
#'
#' @return la liste des sources exécutés
#' @export
#'
sourcoise_refresh <- function(
    what = NULL,
    cache_rep = NULL,
    force_exec = TRUE,
    hash = TRUE,
    unfreeze = TRUE,
    quiet = FALSE,
    init_qmd = TRUE,
    root = NULL,
    log = "INFO",
    .progress = TRUE) {

  refresh_start <- Sys.time()

  if(is.null(what))
    what <- sourcoise_status(root = root, quiet = quiet)

  if(!force_exec)
    what <- what |>
      dplyr::group_by(src) |>
      dplyr::filter(!any(valid)) |>
      dplyr::ungroup()

  if(nrow(what)==0)
    return(list())

  # on en garde qu'un
  what <- what |>
    dplyr::group_by(src) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  if(nrow(what)==0)
    return(list())

  if(init_qmd&rlang::is_installed("ofce"))
    ofce::init_qmd()
  total_time <- ceiling(sum(what$timing, na.rm=TRUE))
  cwd <- getwd() |> fs::path_abs()
  if(.progress)
    idpgr <- cli::cli_progress_bar("refreshing", total = total_time)

  res <- purrr::pmap(what, function(src, wd, lapse, args, root, track, qmd_file, src_in, timing, log_file, ...) {
    exec_wd <- getwd()
    if(wd=="project")
      exec_wd <- root |> fs::path_norm()
    if(wd=="file")
      exec_wd <- fs::path_join(c(root, fs::path_dir(src))) |> fs::path_norm()
    if(wd=="qmd")
      exec_wd <- fs::path_join(c(root, fs::path_dir(qmd_file[[1]]))) |> fs::path_norm()

    # if(src_in %in% c("file", "qmd"))
    #   root <- fs::path_join(c(sroot, root)) |> fs::path_norm()
    # if(src_in %in% c("project"))
    #   root <- sroot

    src_data <- sourcoise(
      path = src,
      force_exec = force_exec,
      hash = hash,
      track = track,
      args = args,
      wd = wd,
      lapse = lapse,
      metadata = TRUE,
      quiet = TRUE,
      src_in = src_in,
      root = root,
      log = log)

    if(.progress)
      cli::cli_progress_update(inc = timing, id = idpgr)

    msrc <- fs::path_join(c(root, src)) |> fs::path_rel(cwd)
    if( src_data$ok == "exec" ) {
      cli::cli_alert_success(
        "{msrc} exécuté avec succès en {round(src_data$timing)} s. pour {scales::label_bytes()(src_data$size)}" )
    } else {
      log_dir <- fs::path_join(c(root, ".sourcoise", "log")) |> fs::path_rel(cwd)
      cli::cli_alert_danger(
        "{msrc} retourne une erreur (voir le log {log_file})" )
    }

    if(unfreeze)
      purrr::walk(src_data$qmd_file, ~{
        if(src_data$ok == "exec") {
          unfreeze(.x, root, quiet = TRUE)
          uncache(.x, root, quiet = TRUE)
        }
      })
    if(!is.null(src_data$error))
      list(src = fs::path_join(c(root, src)), ok = "error", timing = NA, size = NA)
    else
      list(src = fs::path_join(c(root, src)), ok = src_data$ok, timing = src_data$timing, size = src_data$size)
  }
  )

  if(.progress)
    cli::cli_process_done(id = idpgr)

  res <- purrr::transpose(res)

  dt <- difftime(Sys.time(), refresh_start, units = "secs") |> as.numeric() |> round()
  if(!quiet)
    cli::cli_alert_info("Refresh en {dt} secondes")

  invisible(res)
}
