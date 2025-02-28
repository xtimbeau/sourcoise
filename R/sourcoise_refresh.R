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
    quiet = TRUE,
    init_qmd = TRUE) {

  start <- Sys.time()
  if(is.null(what))
    what <- sourcoise_status(cache_rep = cache_rep, root = root, quiet = quiet)

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

  res <- purrr::pmap(what, function(src, wd, lapse, args, root, track, qmd_file, src_in, ...) {

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

    src_data <- sourcoise(path = src,
                            force_exec = force_exec,
                            hash = hash,
                            track = track,
                            args = args,
                            wd = wd,
                            lapse = lapse,
                            metadata = TRUE,
                            quiet = quiet,
                            src_in = src_in,
                            root = root)

    if(unfreeze)
      purrr::walk(src_data$qmd_file, ~{
        if(src_data$ok == "exec") {
          unfreeze(.x, root, quiet = quiet)
          uncache(.x, root, quiet = quiet)
        }
      })
    list(src = src_data$src, ok = src_data$ok)
  }
  )

  cli::cli_alert_success("Refresh en {round(as.numeric(Sys.time()-start))} s.")

  res <- purrr::transpose(res)
  res$src[res$ok == "exec"] |> purrr::list_c()
}
