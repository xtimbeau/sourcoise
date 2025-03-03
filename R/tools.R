# fonctions --------------------

check_return <- function(src) {
  src.txt <- readLines(src, warn=FALSE)
  ret <- stringr::str_extract(src.txt, "^return\\((.*)", group=1)
  purrr::keep(ret, ~!is.na(.x))
}

valid_meta4meta <- function(meta, root) {
  src_hash <- hash_file(fs::path_join(c(root, meta$src)))
  track_hash <- 0

  if(length(meta$track) >0) {
    track_files <- purrr::map(meta$track, ~fs::path_join(c(root, .x)))
    ok_files <- purrr::map_lgl(track_files, fs::file_exists)
    if(any(ok_files))
      track_hash <- hash_file(as.character(track_files[ok_files]))
    else {
      cli::cli_alert_warning("Les fichiers de track sont invalides, vérifiez les chemins")
    }
  }

  meme_null <- function(x, n, def = 0) ifelse(is.null(x[[n]]), def, x[[n]])
  meta$valid_src <- meme_null(meta,"src_hash")==src_hash
  meta$valid_track <- setequal(meta$track_hash, track_hash)
  meta$data_exists <- fs::file_exists(fs::path_join(c(root, meta$data_file)))
  if(meta$lapse != "never") {
    alapse <- what_lapse(meta$lapse)
    meta$valid_lapse <- lubridate::now() - lubridate::as_datetime(meta[["date"]]) <= alapse
  } else
    meta$valid_lapse <- TRUE
  meta$valid <- meta$valid_src & meta$valid_track & meta$valid_lapse & meta$data_exists
  return(meta)
}

valid_metas <- function(ctxt) {

  meme_null <- function(x, n, def = 0) ifelse(is.null(x[[n]]), def, x[[n]])

  ctxt$meta_datas <- purrr::map(ctxt$meta_datas, ~{
    .x$valid_src <- meme_null(.x,"src_hash")==ctxt$src_hash
    .x$valid_arg <- meme_null(.x,"arg_hash", digest::digest(list()))==ctxt$arg_hash
    .x$valid_track <- setequal(.x$track_hash, ctxt$track_hash)
    .x$data_exists <- fs::file_exists(fs::path_join(c(ctxt$root, .x$data_file)))
    if(ctxt$lapse != "never") {
      alapse <- what_lapse(ctxt$lapse)
      .x$valid_lapse <- lubridate::now() - lubridate::as_datetime(.x[["date"]]) <= alapse
    } else
      .x$valid_lapse <- TRUE
    .x$valid <- .x$valid_src & .x$valid_arg & .x$valid_track & .x$valid_lapse & .x$data_exists
    .x
  })

  return(ctxt)
}

hash_file <- function(path) {
  purrr::map_chr(path, ~ {
    if(fs::file_exists(.x)) {
      if(fs::path_ext(.x) %in% c("R", "r", "txt", "csv"))
        digest::digest(readLines(.x, warn = FALSE), algo = "sha1")
      else
        digest::digest(.x, algo = "sha1", file = TRUE)
    }
    else
      glue::glue("no_{.x}_{round(100000000*runif(1))}")
  })
}

get_datas <- function(name, data_rep) {
  m <- get_mdatas(name, data_rep)
  dn <- names(m) |>
    stringr::str_replace(glue::glue(".json"), glue::glue(".qs2")) |>
    rlang::set_names(names(m))
  d <- purrr::map(dn, ~qs2::qs_read(.x, nthreads = getOption("sourcoise.nthreads") ))
  purrr::map(rlang::set_names(names(m)), ~{
    l <- m[[.x]]
    l$data <- d[[.x]]
    l})
}

get_mdatas <- function(name, data_rep) {
  pat <- stringr::str_c(name, "_([a-f0-9]){8}-([0-9]+).json")
  files <- list()
  if(fs::dir_exists(data_rep))
    files <- fs::dir_ls(path = data_rep, regexp = pat, fail=FALSE)
  purrr::map(files, read_mdata)
}

read_mdata <- function(path) {
  l <- jsonlite::read_json(path) |>
    purrr::map( ~if(length(.x)>1) purrr::list_flatten(.x) else unlist(.x) )
  l$file <- path
  l
}

get_ddatas <- function(name, data_rep) {
  pat <- stringr::str_c(name, "_([a-f0-9]){8}-([0-9]+).qs2")
  files <- list()
  if(fs::dir_exists(data_rep))
    files <- fs::dir_ls(path = data_rep, regexp = pat, fail=FALSE)
  res <- purrr::map(files, ~ qs2::qs_read(.x, nthreads = getOption("sourcoise.nthreads")))
  names(res) <- files
  res
}

exec_source <- function(ctxt) {
  safe_source <- purrr::safely(\(src, args) {
    args <- args
    res <- suppressMessages(
      suppressWarnings( base::source(src, local=TRUE) ) )
    res
  })

  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(ctxt$exec_wd)
  start <- Sys.time()
  res <- safe_source(ctxt$src, args = ctxt$args)
  timing <- difftime(Sys.time() , start, unit = "secs") |> as.numeric()
  setwd(current_wd)
  if(!is.null(res$error)) {
    return(
      list(
        ok=FALSE,
        error = res$error,
        args =ctxt$args,
        lapse = ctxt$lapse,
        src = ctxt$relname,
        src_hash = ctxt$src_hash,
        arg_hash = ctxt$arg_hash,
        track_hash = ctxt$track_hash,
        track = ctxt$track,
        wd = ctxt$wd,
        qmd_file = ctxt$new_qmds,
        src_in = ctxt$src_in,
        ok = "exec",
        log_file = ctxt$log_file))
  }

  list(
    data = res$result$value,
    timing = timing,
    date = lubridate::now(),
    size = lobstr::obj_size(res$result$value) |> as.numeric(),
    args =ctxt$args,
    lapse = ctxt$lapse,
    src = ctxt$relname,
    src_hash = ctxt$src_hash,
    arg_hash = ctxt$arg_hash,
    track_hash = ctxt$track_hash,
    track = ctxt$track,
    wd = ctxt$wd,
    qmd_file = ctxt$new_qmds,
    src_in = ctxt$src_in,
    ok = "exec",
    log_file = ctxt$log_file)
}

cache_data <- function(data, ctxt) {
  pat <- stringr::str_c(ctxt$name, "_([a-f0-9]{8})-([0-9]+)\\.json")
  files <- tibble::tibble()

  if(fs::dir_exists(ctxt$full_cache_rep)) {
    files <- fs::dir_info(path = ctxt$full_cache_rep, regexp = pat) |>
      dplyr::mutate(uid = stringr::str_extract(path, pat, group=1),
                    cc = stringr::str_extract(path, pat, group=2) |> as.numeric())
  }
  cc <- 1
  exists <- FALSE
  new_data_hash <- digest::digest(data$data)

  if(nrow(files)>0) {
    uids <- files$uid
    ccs <- files$cc
    hashes <- purrr::map_dfr(
      files$path, ~{
        mdata <- read_mdata(.x)
        tibble::tibble(path = .x, data_hash = mdata$data_hash, data_file = mdata$data_file)
      }) |>
      dplyr::filter(data_hash == new_data_hash)
    if(nrow(hashes)>0) {
      exists_data_file <- hashes |>
        dplyr::slice(1) |>
        dplyr::pull(data_file) |>
        fs::path_file()

      exists_data_file <- fs::path_join(c(ctxt$full_cache_rep, exists_data_file))
      exists <- fs::file_exists(exists_data_file)
      file_size <- fs::file_info(exists_data_file)$size
    }
    cc <- max(files$cc, na.rm = TRUE) + 1
  }
  if(!fs::dir_exists(ctxt$full_cache_rep))
    fs::dir_create(ctxt$full_cache_rep, recurse=TRUE)
  data$data_hash <- new_data_hash
  data$id <- stringr::str_c(ctxt$uid, "-", cc)
  data$uid <- ctxt$uid
  data$cc <- cc
  fnm <- fs::path_join(
    c(ctxt$full_cache_rep,
      stringr::str_c(ctxt$basename, "_", data$id))) |> fs::path_ext_set("json")
  if(!ctxt$nocache) {
    if(!exists) {
      fnd <- fs::path_join(
        c(ctxt$full_cache_rep,
          stringr::str_c(ctxt$basename, "_", data$data_hash))) |> fs::path_ext_set("qs2")
      qs2::qs_save( data$data, file = fnd, nthreads = getOption("sourcoise.nthreads") )
      file_size <- fs::file_info(fnd)$size
      if(fs::file_info(fnd)$size > ctxt$limit_mb*1024*1024) {
        fs::file_delete(fnd)
        logger::log_warn("Les données ne sont pas mis en cache parce que trop volumineuses ({scales::label_bytes()(file_size)}")
      }
    } else
      fnd <- exists_data_file
    les_metas <- data
    les_metas$data <- NULL
    les_metas$data_file <- fs::path_file(fnd)
    les_metas$file_size <- file_size
    les_metas$file <- NULL
    les_metas$ok <- NULL
    les_metas$id <- NULL
    jsonlite::write_json(les_metas, path = fnm)
  }
  return(data)
}

what_lapse <- function(check) {

  ext <- function(e) {
    num <- stringr::str_extract(e, "^([0-9]+)")
    if(is.na(num))
      num <- 1
    num <- as.numeric(num)
  }

  if(stringr::str_detect(check, "month"))
    return(lubridate::period(ext(check), unit = "months"))
  if(stringr::str_detect(check, "week"))
    return(lubridate::weeks(ext(check)))
  if(stringr::str_detect(check, "quarter"))
    return(lubridate::period(3*ext(check), unit = "months"))
  if(stringr::str_detect(check, "day"))
    return(lubridate::days(ext(check)))
  if(stringr::str_detect(check, "hour"))
    return(lubridate::hours(ext(check)))
  if(stringr::str_detect(check, "year"))
    return(lubridate::years(ext(check)))
}

remove_ext <- function(name) {
  stringr::str_remove(name, "\\.[r|R]$")
}

find_src <- function(root, name) {
  path <- fs::path_join(c(root, name)) |> fs::path_norm()
  fn <- stringr::str_c(path, ".r")
  if(fs::file_exists(fn)) return(fn)
  fn <- stringr::str_c(path, ".R")
  if(fs::file_exists(fn)) return(fn)
  return(NULL)
}

try_find_src <- function(root, name) {
  pat <- glue::glue("{name}\\.[Rr]$")
  ff <- fs::dir_ls(path = root, regexp=pat, recurse=TRUE)
  ff |> purrr::discard(~ stringr::str_detect(.x, "/_"))
}

unfreeze <- function(qmd_file, root, quiet=TRUE) {
  if(is.null(qmd_file))
    return(NULL)
  qmd_folder <- qmd_file |> fs::path_ext_remove()
  freeze_path <- fs::path_join(c(root, "_freeze", qmd_folder))
  if(fs::dir_exists(freeze_path)) {
    if(!quiet)
      cli::cli_alert_info("Unfreezing {.file {freeze_path}}")
    unlink(freeze_path, recursive=TRUE, force=TRUE)
  }
  return(NULL)
}

uncache <- function(qmd_file, root, quiet=TRUE) {
  if(is.null(qmd_file))
    return(NULL)
  qmd_bn <- qmd_file |> fs::path_file() |> fs::path_ext_remove()
  rel_path <- fs::path_dir(qmd_file)
  cache_path <- fs::path_join(c(root, rel_path, stringr::str_c(qmd_bn, "_cache")))
  files_path <- fs::path_join(c(root, rel_path, stringr::str_c(qmd_bn, "_files")))
  if(fs::dir_exists(cache_path)) {
    if(!quiet)
      cli::cli_alert_info("Uncaching {.file {cache_path}}")
    unlink(cache_path, recursive=TRUE, force=TRUE)
  }
  if(fs::dir_exists(files_path)) {
    if(!quiet)
      cli::cli_alert_info("Unfiles {.file {files_path}}")
    unlink(files_path, recursive=TRUE, force=TRUE)
  }
  return(NULL)
}

try_find_root <- function(root=NULL, src_in = getOption("sourcoise.src_in"), quiet = TRUE) {
  if(!is.null(root))
    return(root)
  if(src_in == "project") {
    if(Sys.getenv("QUARTO_PROJECT_DIR") == "") {
      safe_find_root <- purrr::safely(rprojroot::find_root)
      root <- safe_find_root(
        rprojroot::is_quarto_project | rprojroot::is_r_package | rprojroot::is_rstudio_project)
      if(is.null(root$error))
        return(root$result |> fs::path_abs() |> fs::path_norm())
      else {
        if(!quiet)
          cli::cli_alert_warning("{root$error}")
        return(NULL)
      }
    }
    return(Sys.getenv("QUARTO_PROJECT_DIR") |> fs::path_abs() |> fs::path_norm())
  }

  if(src_in == "file") {
    paths <- find_project_root(NULL, NULL)
    return( fs::path_join(c(paths$project_path, paths$doc_path)) |> fs::path_norm() )
  }
  root <- NULL
}


find_project_root <- function(project_path = NULL, doc_path = NULL) {
  if(is.null(doc_path)) {
    if(Sys.getenv("QUARTO_DOCUMENT_PATH") != "")
      doc_path <- Sys.getenv("QUARTO_DOCUMENT_PATH") |> fs::path_abs() |> fs::path_norm()
    else
      doc_path <- getwd() |> fs::path_abs() |> fs::path_norm()
  }
  if(is.null(project_path)) {
    project_path <- Sys.getenv("QUARTO_PROJECT_DIR")
    if(project_path == "") {
      safe_find_root <- purrr::safely(rprojroot::find_root)
      project_path <- safe_find_root(rprojroot::is_quarto_project)
      if(!is.null(project_path$error))
        project_path <- safe_find_root(rprojroot::is_rstudio_project)
      if(!is.null(project_path$error))
        project_path$result <- getwd()
      project_path <- project_path$result
    }
  }
  project_path <- project_path |> fs::path_norm() |> fs::path_abs()
  doc_path <- doc_path |> fs::path_norm() |> fs::path_abs() |> fs::path_rel(project_path)
  return(list(project_path = project_path, doc_path = doc_path))
}
