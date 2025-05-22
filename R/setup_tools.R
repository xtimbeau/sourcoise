# calcule les différents chemins et trouve les fichiers/répertoire dont on a besoin

setup_context <- function(path, root, src_in, exec_wd, wd, track, args,
                          lapse, nocache, limit_mb, grow_cache, log, priority,
                          metadata, inform=FALSE, quiet=TRUE) {
  ctxt <- list()

  if(is.null(track))
    ctxt$track <- list()
  else
    ctxt$track <- track

  if(is.null(args))
    ctxt$args <- list()
  else
    ctxt$args <- args

  ctxt$argid <- digest::digest(args, algo = "crc32")

  ctxt$lapse <- lapse
  ctxt$quiet <- quiet
  ctxt$inform <- inform
  ctxt$wd <- wd
  ctxt$src_in <- src_in
  ctxt$nocache <- nocache
  ctxt$grow_cache <- grow_cache
  ctxt$limit_mb <- limit_mb
  ctxt$priority <- priority
  ctxt$metadata <- metadata

  # on trouve le fichier
  ctxt$name <- remove_ext(path)
  ctxt$paths <- find_project_root()
  ctxt$root <- try_find_root(root, src_in)

  ctxt$uid <- digest::digest(ctxt$root, algo = "crc32")

  ctxt <- startup_log(log, ctxt)

  ctxt[["src"]] <- find_src(ctxt$root, ctxt$name, ctxt$paths)
  if(is.null(ctxt[["src"]])) {
    ctxt[["src"]] <- try_find_src(ctxt$root, ctxt$name)
    if(length(ctxt[["src"]])==0)
      return(NULL)
    n_src <- length(ctxt[["src"]])
    if(n_src>1) {
      l_src <- purrr::map(ctxt[["src"]], ~stringr::str_count(.x |> fs::path_rel(getwd()), "/"))
      ctxt[["src"]] <- ctxt[["src"]][[which.min(l_src)]]
      cli::cli_alert_warning("{n_src} sources detected, choosing {ctxt[['src']]}, the closest to wd.")
    }
  }

  ctxt$basename <- fs::path_file(ctxt$name) |>
    stringr::str_c(ctxt$argid, sep = "-")
  ctxt$relname <- fs::path_rel(ctxt$src, ctxt$root)
  ctxt$reldirname <- fs::path_dir(ctxt$relname)

  if(src_in == "project") {
    ctxt$root_cache_rep <- fs::path_join(c(ctxt$root, ".sourcoise")) |>
      fs::path_norm()
    ctxt$full_cache_rep <- fs::path_join(c(ctxt$root_cache_rep, ctxt$reldirname)) |>
      fs::path_norm()
  }
  if(src_in == "file") {
    file_path <- fs::path_dir(ctxt$src)
    ctxt$root_cache_rep <- fs::path_join(c(file_path, ".sourcoise")) |>
      fs::path_norm()
    ctxt$full_cache_rep <- ctxt$root_cache_rep
    wd <- "file"
  }

  ctxt$qmd_path <- ctxt$paths$doc_path
  if(Sys.getenv("QUARTO_DOCUMENT_PATH") != "") {
    ctxt$qmd_file <- fs::path_join(c(ctxt$qmd_path, knitr::current_input())) |>
      stringr::str_c(".qmd") |>
      fs::path_norm()
  } else {
    ctxt$qmd_file <- NULL
  }

  if(!is.null(ctxt$qmd_file))
    logger::log_debug("qmd: {ctxt[['qmd_file']]}")
  logger::log_debug("source: {ctxt[['src']]}")
  logger::log_debug("root: {ctxt[['root']]}")
  logger::log_debug("cache: {ctxt[['full_cache_rep']]}")

  ctxt$exec_wd <- exec_wd
  if(is.null(exec_wd)) {
    ctxt$exec_wd <- getwd()
    if(wd=="project")
      ctxt$exec_wd <- ctxt$root
    if(wd=="file")
      ctxt$exec_wd <- fs::path_dir(ctxt$src)
    if(wd=="qmd") {
      if(!is.null(ctxt$qmd_path)) {
        ctxt$exec_wd <- ctxt$qmd_path
      } else {
        cli::cli_alert_warning("No quarto project and wd is set to 'qmd', check your setup.")
        ctxt$exec_wd <- fs::path_dir(ctxt$src)
      }
    }
  }
  logger::log_debug("wd: {ctxt[['exec_wd']]}")

  ctxt <- ctxt |>
    hash_context()

  return(ctxt)
}

# calcule les hashs et ajoute les métadonnées au contexte

hash_context <- function(ctxt) {

  ctxt$src_hash <- hash_file(ctxt$src)
  ctxt$arg_hash <- digest::digest(ctxt$args, "crc32")
  ctxt$track_hash <- 0

  if(length(ctxt$track) > 0) {
    track_files <- purrr::map(ctxt$track, ~fs::path_join(c(ctxt$root, .x)))
    ok_files <- purrr::map_lgl(track_files, fs::file_exists)
    if(any(ok_files))
      ctxt$track_hash <- hash_file(as.character(track_files[ok_files]))
    else {
      cli::cli_alert_warning("Tracked files not found ({track_files[[!ok_files]]}), check your paths.")
    }
  }

  ctxt$meta_datas <- get_mdatas(ctxt$basename, ctxt$full_cache_rep)

  ctxt$qmds <- purrr::map(ctxt$meta_datas, "qmd_file") |>
    purrr::list_flatten() |>
    purrr::discard(is.null) |>
    unlist() |>
    unique()
  ctxt$new_qmds <- unique(c(ctxt$qmds, ctxt$qmd_file))

  return(ctxt)
}

startup_log <- function(log, ctxt) {
  if(log==FALSE)
    log <- "OFF"
  log_dir <- fs::path_join(c(ctxt$root,".sourcoise", "logs"))
  logger::log_threshold(log)

  if(!ctxt$quiet)
    logger::log_appender(logger::appender_stdout)

  if(log == "OFF") {
    if(!ctxt$quiet)
      logger::log_threshold("INFO")
    return(ctxt)
  }

  if(!fs::dir_exists(log_dir))
    fs::dir_create(log_dir)
  log_fn <- fs::path_join(c(log_dir, stringr::str_c(ctxt$uid, "_", lubridate::today() |> as.character()))) |>
    fs::path_ext_set("log")

  logger::log_appender(logger::appender_file(log_fn))

  ctxt$log_file <- fs::path_rel(log_fn, getwd() |> fs::path_abs())

  return(ctxt)
}
