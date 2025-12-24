# calcule les différents chemins et trouve les fichiers/répertoire dont on a besoin
setup_context <- function(path, root, src_in="project", exec_wd=NULL, wd="file",
                          track = NULL, args = NULL,
                          lapse="never", nocache=FALSE, limit_mb=50, grow_cache=Inf, log="OFF",
                          metadata=FALSE, inform=FALSE, quiet=TRUE, hash = TRUE) {
  ctxt <- list()
  if(is.null(track))
    ctxt$track <- list()
  else
    ctxt$track <- track

  if(length(args)==0)
    ctxt$args <- list()
  else
    ctxt$args <- args

  ctxt$argid <- digest::digest(ctxt$args, algo = "crc32")

  ctxt$lapse <- lapse
  ctxt$quiet <- quiet
  ctxt$inform <- inform
  ctxt$wd <- wd
  ctxt$src_in <- src_in
  ctxt$nocache <- nocache
  ctxt$grow_cache <- grow_cache
  ctxt$limit_mb <- limit_mb
  ctxt$metadata <- metadata

  # on trouve le fichier
  ctxt$name <- remove_ext(path)
  ctxt$paths <- find_project_root(root=root)
  ctxt$root <- ctxt$paths$root

  ctxt$uid <- digest::digest(as.character(ctxt$root), algo = "crc32")

  ctxt$log_file <- startup_log2(log, ctxt$root, ctxt$uid, inform = inform)
  ctxt[["src"]] <- find_src(ctxt$root, ctxt$name, ctxt$paths)
  if(is.null(ctxt[["src"]])) {
    ctxt[["src"]] <- try_find_src(ctxt$root, ctxt$name)
    if(length(ctxt[["src"]])==0)
      return(NULL)
    n_src <- length(ctxt[["src"]])
    if(n_src>1) {
      l_src <- purrr::map(ctxt[["src"]], ~stringr::str_count(.x |> fs::path_rel(getwd()), "/"))
      ctxt[["src"]] <- ctxt[["src"]][[which.min(l_src)]]
      if(!quiet)
        cli::cli_alert_warning(
          "{n_src} sources detected, choosing {ctxt[['src']]}, the closest to wd.")
    }
  }
  logger::log_info("sourcoising {ctxt$src}")

  ctxt$relname <- fs::path_rel(ctxt$src, ctxt$root)
  ctxt$basename <- ctxt$relname |>
    fs::path_ext_remove() |>
    stringr::str_c(ctxt$argid, sep = "-")
  ctxt$reldirname <- fs::path_dir(ctxt$relname)
  ctxt$cachename <- ctxt$relname |> fs::path_ext_remove()
  ctxt$cachebasename <- ctxt$basename
  if(src_in == "project") {
    ctxt$root_cache_rep <- fs::path_join(c(ctxt$root, ".sourcoise")) |>
      fs::path_norm()
    ctxt$full_cache_rep <- fs::path_join(c(ctxt$root_cache_rep, ctxt$reldirname)) |>
      fs::path_norm()
    # vérifier que .sourcoise n'est pas dans le répertoire du source,
    # dans ce cas, c'est celui là qu'on prend
    sourcoise_file_rep <- fs::path_join(c(ctxt$root, ctxt$reldirname, ".sourcoise")) |>
      fs::path_norm()
    if(sourcoise_file_rep != ctxt$full_cache_rep && dir.exists(sourcoise_file_rep)) {
      istheresourcoise <- length(
        fs::dir_ls(sourcoise_file_rep,
                   pattern = fs::path_file(ctxt$src) |> fs::path_ext_remove()))>0
      if(istheresourcoise) {
        file_path <- fs::path_dir(ctxt$src)
        ctxt$root_cache_rep <- fs::path_join(c(file_path, ".sourcoise")) |>
          fs::path_norm()
        ctxt$full_cache_rep <- ctxt$root_cache_rep
        wd <- "file"
        ctxt$cachename <- fs::path_file(ctxt$relname) |> fs::path_ext_remove()
        ctxt$cachebasename <- ctxt$basename |>
          fs::path_file()
        logger::log_warn(
          "scr_in is 'project' but cache files found in file folder, switching to src_in='file'.")
        logger::log_warn("If it is not what you want, please delete {ctxt$full_cache_rep}")
        if(!quiet)
          cli::cli_alert_warning(
            "scr_in is 'project' but cache files found in file folder, switching to src_in='file'.")
      }
    }
  }
  if(src_in == "file") {
    file_path <- fs::path_dir(ctxt$src)
    ctxt$root_cache_rep <- fs::path_join(c(file_path, ".sourcoise")) |>
      fs::path_norm()
    ctxt$full_cache_rep <- ctxt$root_cache_rep
    wd <- "file"
    ctxt$cachename <- fs::path_file(ctxt$relname) |> fs::path_ext_remove()
    ctxt$cachebasename <- ctxt$basename |>
      fs::path_file()
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

  if(hash) {
    ctxt <- ctxt |>
      hash_context()
    ctxt$priority <- ctxt$meta1$priority %||% 10
  }
  return(ctxt)
}

# calcule les hashs et ajoute les métadonnées au contexte

hash_context <- function(ctxt) {
  ctxt$src_hash <- hash_file(ctxt$src)
  ctxt$arg_hash <- ctxt$argid
  mm <- get_mdatas(ctxt$cachebasename, ctxt$root_cache_rep, ctxt$root)
  ctxt$meta1 <- mm$meta1
  ctxt$metas <- mm$metas

  ctxt$track <- c(ctxt$track, ctxt$meta1$track) |>
    unlist() |>
    unique()
  ctxt$track_hash <- 0
  if(length(ctxt$track) > 0) {
    tt <- ctxt$track |>
      purrr::map(~c(ctxt$root, .x)) |>
      fs::path_join()
    oktt <- tt |>
      file.exists()
    ctxt$track <- ctxt$track[oktt]
    if(length(ctxt$track)>0)
      ctxt$track_hash <- hash_file(as.character(tt[oktt])) |>
      digest::digest(algo = "sha1")
    else {
      logger::log_info("Tracked files not found, check your paths.")
    }
  }

  ctxt$qmd_file <- c(ctxt$meta1$qmd_file, ctxt$qmd_file) |>
    unlist() |>
    unique()
  if(ctxt$paths$in_quarto & length(ctxt$qmd_file)>0) {
    qq <- ctxt$qmd_file |>
      purrr::map(~c(ctxt$root, .x))
    qq <- qq |>
      fs::path_join() |>
      file.exists()
    ctxt$qmd_file <- ctxt$qmd_file[qq] |> as.list()
  }

  return(ctxt)
}

get_all_metadata <- function(ctxt) {

  if(length(ctxt$metas) == 0 || length(ctxt$metas$json_file)==0) {
    return(tibble::tibble()) }

  fast_read_mdata(ctxt$metas)
}

# ctxt_json_history <- function(ctxt) {
#   ctxt <- get_all_metadata(ctxt)
#
#   ctxt$qmds <- purrr::list_transpose(ctxt$full_meta_datas) |>
#     purrr::pluck("qmd_file") |>
#     c() |>
#     unique()
#   ctxt$new_qmds <- unique(c(ctxt$qmds, ctxt$qmd_file))
#
#   ctxt$track_hash <- 0
#   already_tracked <- purrr::list_transpose(ctxt$meta_datas) |>
#     purrr::pluck("track") |>
#     c() |>
#     unique()
#   tracked <- unique(ctxt$track, already_tracked)
#   if(length(tracked) > 0) {
#     track_files <- purrr::map(track, ~fs::path_join(c(ctxt$root, .x)))
#     ok_files <- purrr::map_lgl(track_files, fs::file_exists)
#     tracked <- track_files[ok_files]
#     if(any(ok_files))
#       ctxt$track_hash <- hash_file(as.character(ctxt$track)) |> digest::digest(algo = "sha1")
#     else {
#       logger::log_info("Tracked files not found ({track_files[[!ok_files]]}), check your paths.")
#     }
#   }
#   ctxt$track <- tracked
# }

# startup_log <- function(log, ctxt) {
#   if(log==FALSE)
#     log <- "OFF"
#   log_dir <- fs::path_join(c(ctxt$root,".sourcoise_logs"))
#   logger::log_threshold(log)
#
#   if(log == "OFF") {
#     return(ctxt)
#   }
#
#   if(!fs::dir_exists(log_dir))
#     fs::dir_create(log_dir)
#   log_fn <- fs::path_join(c(log_dir, stringr::str_c(ctxt$uid, "_", lubridate::today() |> as.character()))) |>
#     fs::path_ext_set("log")
#   logger::log_appender(logger::appender_file(log_fn))
#   ctxt$log_file <- fs::path_rel(log_fn, getwd() |> path_abs())
#
#   return(ctxt)
# }

startup_log2 <- function(log, root,
                         uid = digest::digest(as.character(root), algo = "crc32"),
                         inform = FALSE) {
  if(log==FALSE)
    log <- "OFF"
  log_dir <- fs::path_join(c(root,".sourcoise_logs")) |>
    path_abs()
  logger::log_threshold(log)

  if(log == "OFF") {
    return(NULL)
  }

  if(!dir.exists(log_dir))
    fs::dir_create(log_dir)
  log_fn <- fs::path_join(c(log_dir, stringr::str_c(uid, "_", lubridate::today() |> as.character()))) |>
    fs::path_ext_set("log")
  if(!inform)
    logger::log_appender(logger::appender_file(log_fn))
  if(inform)
    logger::log_appender(logger::appender_tee(log_fn))
  log_file <- fs::path_rel(log_fn, getwd() |> path_abs())
  return(log_file)
}
