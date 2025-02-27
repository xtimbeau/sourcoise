# calcule les différents chemins et trouve les fichiers/répertoire dont on a besoin

setup_context <- function(path, root, src_in, cache_rep, exec_wd, wd, track, args,
                          lapse, nocache, limit_mb, grow_cache, quiet=TRUE) {

  ctxt <- list()

  if(is.null(track))
    ctxt$track <- list()
  else
    ctxt$track <- track

  if(is.null(args))
    ctxt$args <- list()
  else
    ctxt$args <- args

  ctxt$lapse <- lapse
  ctxt$quiet <- quiet
  ctxt$wd <- wd
  ctxt$src_in <- src_in
  ctxt$nocache <- nocache
  ctxt$grow_cache <- grow_cache
  ctxt$limit_mb <- limit_mb

  # on trouve le fichier
  ctxt$name <- remove_ext(path)
  ctxt$paths <- find_project_root()
  ctxt$root <- try_find_root(root, src_in)
  ctxt$uid <- digest::digest(ctxt$root, algo = "crc32")

  if(!ctxt$quiet)
    cli::cli_alert_info("root: {ctxt$root}")
  if(!ctxt$quiet)
    cli::cli_alert_info("uid: {ctxt$uid}")
  if(is.null(cache_rep))
    ctxt$root_cache_rep <- fs::path_join(c(ctxt$root, ".data")) |>
    fs::path_norm()
  else
    ctxt$root_cache_rep <- fs::path_abs(cache_rep)
  if(!quiet)
    cli::cli_alert_info("cache: {ctxt$root_cache_rep}")

  ctxt[["src"]] <- find_src(ctxt$root, ctxt$name)
  if(is.null(ctxt[["src"]])) {
    ctxt[["src"]] <- try_find_src(ctxt$root, ctxt$name)
    if(length(ctxt[["src"]])==0) {
      if(!ctxt$quiet)
        cli::cli_alert_warning("Le fichier n'existe pas en .r ou .R, vérifier le chemin")
      return(NULL)
    }
    if(length(ctxt[["src"]])>1) {
      if(!ctxt$quiet)
        cli::cli_alert_warning("Plusieurs fichiers src sont possibles")
      l_src <- purrr::map(ctxt[["src"]], stringr::str_length) # ce critère est curieux
      ctxt[["src"]] <- ctxt[["src"]][[which.min(l_src)]]
    }
  }

  if(!ctxt$quiet)
    cli::cli_alert_info("{.file {ctxt[['src']]}} comme source")

  if(length(check_return(ctxt[['src']]))==0) {
    cli::cli_alert_danger("Pas de return() d\u00e9t\u00e9ct\u00e9 dans le fichier {.file {ctxt[['src']]}}")
  }

  if(length(check_return(ctxt[['src']]))>1) {
    if(!ctxt$quiet)
      cli::cli_alert_info("Plusieurs return() dans le fichier {ctxt[['src']]}, attention !")
  }

  ctxt$basename <- fs::path_file(ctxt$name)
  ctxt$relname <- fs::path_rel(ctxt$src, ctxt$root)
  ctxt$reldirname <- fs::path_dir(ctxt$relname)
  ctxt$full_cache_rep <- fs::path_join(c(ctxt$root_cache_rep, ctxt$reldirname)) |>
    fs::path_norm()
  ctxt$qmd_path <- ctxt$paths$doc_path
  if(Sys.getenv("QUARTO_DOCUMENT_PATH") != "") {
    ctxt$qmd_file <- fs::path_join(c(ctxt$qmd_path, knitr::current_input())) |>
      fs::path_ext_set("qmd") |>
      fs::path_norm()
  } else {
    ctxt$qmd_file <- NULL
  }

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
        cli::cli_alert_warning("Pas de document identifié, probablement, non exectu\u00e9 de quarto")
        ctxt$exec_wd <- fs::path_dir(ctxt$src)
      }
    }
  }

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
      cli::cli_alert_warning("Les fichiers de track sont introuvables, v\u00e9rifiez les chemins")
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

# pick les meilleures données en cache

pick_gooddata <- function(good_datas, ctxt) {
  dates <- purrr::map(good_datas, "date") |>
    unlist() |>
    lubridate::as_datetime()
  mdd <- which.max(dates)
  good_good_data <- good_datas[[mdd]]
  fnm <- names(good_datas)[[mdd]]
  fnd <- fs::path_join(c(ctxt$root, good_good_data$data_file))

  if(!ctxt$quiet)
    cli::cli_alert_warning("M\u00e9tadonn\u00e9es lues dans {.file {fnd}}")

  ggd_lapse <- good_good_data$lapse %||% "never"
  ggd_wd <- good_good_data$wd %||% "file"
  ggd_qmds <- setequal(good_good_data$qmd_file, ctxt$new_qmds)
  ggd_track <- setequal(good_good_data$track, ctxt$track)
  ggd_src_in <- ctxt$src_in == good_good_data$src_in %||% "project"

  if(ggd_lapse != ctxt$lapse | ggd_wd != ctxt$wd | !ggd_qmds | !ggd_track | !ggd_src_in) {
    newmdata <- good_good_data
    newmdata$file <- NULL
    newmdata$lapse <- ctxt$lapse
    newmdata$wd <- ctxt$wd
    newmdata$qmd_file <- ctxt$new_qmds
    newmdata$track <- ctxt$track
    newmdata$src_in <- ctxt$src_in
    jsonlite::write_json(newmdata, path = fnm)
  }
  if(!ctxt$quiet)
    cli::cli_alert_warning("Donn\u00e9es en cache")

  good_good_data$ok <- "cache"
  good_good_data$data <- qs::qread(fnd)

  return(good_good_data)
}

startup_log <- function(log, ctxt) {
  if(log==FALSE)
    log <- "OFF"

  logger::log_threshold(log)

  if(log != "OFF") {
    log_dir <- fs::path_join(c(ctxt$root,".logs"))
    if(!fs::dir_exists(log_dir))
      fs::dir_create(log_dir)
    log_fn <- fs::path_join(c(log_dir, str_c("sourcoise_", lubridate::today() |> as.character()))) |>
      fs::path_ext_set("log")
    logger::log_appender(logger::appender_file(log_fn))
    if(!is.null(ctxt$qmd_file))
      logger::log_info("qmd file : {ctxt[['qmd_file']]}")
    logger::log_info("source file : {ctxt[['src']]}")
    logger::log_debug("root : {ctxt[['root']]}")
    logger::log_debug("cache : {ctxt[['full_cache_rep']]}")
  }
}

prune_cache <- function(ctxt) {
  if(is.infinite(ctxt$grow_cache))
    return(NULL)
  md <- get_mdatas(ctxt$basename, ctxt$full_cache_rep)
  if(length(md)<=grow_cache)
    return(NULL)
  date <- purrr::map_chr(md, "date")
  rdate <- rank(date) > length(md) - ctxt$grow_cache

  sure_delete <- function(fn) {
    if(fs::file_exists(fn))
      fs::file_delete(fn)
  }
  good_datas <- map_chr(md[rdate], "data_file") |> unique()
  purrr::iwalk(md[!rdate], ~{
    sure_delete(.y)
    if(!.x$data_file %in% good_datas)
      sure_delete(.x$data_file)
  })
}
