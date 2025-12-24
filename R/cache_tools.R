#' @importFrom rlang .data
#' @noRd
cache_data <- function(data, ctxt) {

  all_metas <- get_all_metadata(ctxt)
  new_data_hash <- digest::digest(data$data)

  exist <- FALSE
  if(nrow(all_metas)>0) {
    meta <- all_metas |>
      dplyr::filter(.data$data_hash == new_data_hash) |>
      dplyr::slice(1)
    if(nrow(meta)==1) {
      exist <- TRUE
      exists_data_file <- meta |>
        dplyr::pull(.data$data_file) |>
        fs::path_file()

      exists_data_file <- fs::path_join(c(ctxt$full_cache_rep, exists_data_file))
      exists <- file.exists(exists_data_file)
      finfo <- file.info(exists_data_file)
      exists_file_size <- finfo$size
      exists_data_date <- meta |> dplyr::pull(.data$data_date) |> as.character()
    }
  }
  if(!dir.exists(ctxt$full_cache_rep))
    dir.create(ctxt$full_cache_rep, recursive=TRUE)
  data$data_hash <- new_data_hash
  if(!ctxt$nocache) {
    les_metas <- data
    les_metas$priority <- ctxt$priority
    if(!exist) {
      fnd <- fs::path_join(
        c(ctxt$root_cache_rep,
          stringr::str_c(ctxt$cachebasename, "_", stringr::str_c(data$data_hash, ".qs2"))))
      qs2::qs_save( data$data, file = fnd, nthreads = getOption("sourcoise.nthreads") )
      f_i <- file.info(fnd)
      les_metas$file_size <- f_i$size
      les_metas$data_date <- f_i$mtime |> as.character()
      if(f_i$size > ctxt$limit_mb*1024*1024) {
        fs::file_delete(fnd)
        logger::log_warn(
          "cached data not saved ({fs::as_fs_bytes(file_size)} -- over the {ctxt$limit_md} Mb limit.")
      }
    } else {
      fnd <- exists_data_file
      les_metas$file_size <- exists_file_size
      les_metas$data_date <- exists_data_date
    }
    les_metas$data_file <- data$data_file <- fs::path_file(fnd)
    if(!is.null(ctxt$log_file))
      les_metas$log_file <- ctxt$log_file |> fs::path_rel(ctxt$root)
    data$data_date <- les_metas$data_date
    data$json_file <- write_meta(les_metas, ctxt)
    prune_cache(ctxt)
  }
  return(data)
}

#' @importFrom rlang .data
#' @noRd
prune_cache <- function(ctxt) {
  if(is.infinite(ctxt$grow_cache))
    return(NULL)

  md <- get_mdatas(ctxt$cachebasename, ctxt$full_cache_rep, ctxt$root)$meta1

  pairs <- purrr::imap_dfr(
    md,
    ~tibble::tibble(data_file = .x$data_file, json_file = .y, date = .x$date) )

  datas <- unique(pairs$data_file)
  jsons <- unique(pairs$json_file)
  datapairs <- pairs |>
    dplyr::group_by(.data$data_file) |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::summarize(
      date = dplyr::first(.data$date),
      json_file = dplyr::first(.data$json_file)) |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::slice_head(n=ctxt$grow_cache)
  datas_out <- setdiff(datas, datapairs$data_file)
  json_in <- pairs |>
    dplyr::semi_join(datapairs, dplyr::join_by(data_file)) |>
    dplyr::pull(json_file)
  jsons_out <- setdiff(jsons, json_in)

  purrr::walk(jsons_out, ~ sure_delete(.x))
  purrr::walk(datas_out, ~ sure_delete(fs::path_join(c(ctxt$full_cache_rep, .x))))
}

read_meta1 <- function(ctxt) {
  metas <- ctxt$meta1
  new_meta <- purrr::map_lgl(
    rlang::set_names(c("track", "track_hash", "qmd_file", "wd", "src_in")),
    ~{
      chged <- !setequal(metas[[.x]], ctxt[[.x]])
      metas[[.x]] <<- metas[[.x]]
      chged})
  fnd <- fs::path_join(c(ctxt$full_cache_rep, ctxt$meta1$data_file))
  if(!data_ok(fnd, ctxt$cachebasename))
    return(NULL)
  data <- metas
  data$ok <- "cache"
  data$error <- NULL
  if(file.exists(fnd)) {
    data$data_date <- lubridate::as_datetime(data$data_date)
    data$data <- qs2::qs_read(fnd, nthreads = getOption("sourcoise.nthreads"))
  }
  if(any(new_meta)) {
    data$json_file <- write_meta(metas, ctxt)
  }
  return(data)
}

read_meta1_valid <- function(ctxt) {
  metas <- ctxt$meta1
  new_meta <- purrr::map_lgl(
    rlang::set_names(c("track", "track_hash", "qmd_file", "wd", "src_in")),
    ~{
      chged <- !setequal(metas[[.x]], ctxt[[.x]])
      metas[[.x]] <<- metas[[.x]]
      chged})
  fnd <- fs::path_join(c(ctxt$full_cache_rep, ctxt$meta1$data_file))
  if(!data_ok(fnd, ctxt$cachebasename))
    return(NULL)
  data <- metas
  data$ok <- "cache"
  data$error <- NULL
  if(file.exists(fnd)) {
    data$data_date <- lubridate::as_datetime(data$data_date)
    data$data <- qs2::qs_read(fnd, nthreads = getOption("sourcoise.nthreads"))
  }
  if(any(new_meta)) {
    data$json_file <- write_meta(metas, ctxt)
  }
  return(data)
}

write_meta <- function(metas, ctxt) {

  towrite <- list(
    timing = metas$timing |> as.numeric(),
    date = metas$date |> as.character(),
    size = metas$size |> as.numeric(),
    args = metas[["args"]] %||% list(),
    lapse = metas$lapse,
    src = metas$src,
    src_hash = metas$src_hash,
    arg_hash = metas$arg_hash,
    track_hash = metas$track_hash,
    track = metas[["track"]] %||% list(),
    wd = metas$wd,
    qmd_file = metas[["qmd_file"]] %||% list(),
    src_in = metas$src_in,
    data_hash = metas$data_hash,
    priority = metas$priority,
    file_size = metas$file_size,
    data_date = metas$data_date,
    data_file = metas$data_file)

  new_cc <- 1
  existing <- fast_metadata(root=ctxt$root,
                            uid = ctxt$uid,
                            bn = ctxt$cachename,
                            argid = ctxt$argid,
                            cache_reps = ctxt$root_cache_rep)
  if(length(existing$index)>0) {
    new_cc <- existing$index
    if(length(new_cc) > 0)
      new_cc <- max(new_cc, na.rm=TRUE)+1
  }
  new_json_fn <- fs::path_join(
    c(ctxt$root_cache_rep,
      glue::glue("{ctxt$cachename}-{ctxt$argid}_{ctxt$uid}-{new_cc}"))) |>
    fs::path_ext_set("json")
  jsonlite::write_json(towrite, new_json_fn, pretty = TRUE)
  return(new_json_fn)
}

read_metas <- function(ctxt) {
  all_metas <- get_all_metadata(ctxt)
  if(nrow(all_metas)==0)
    return(NULL)
  all_metas <- all_metas |>
    dplyr::filter(data_date == max(data_date)) |>
    dplyr::slice(1) |>
    as.list()

  data <- all_metas
  data$data <- NULL
  fnd <- fs::path_join(c(fs::path_dir(all_metas$name), all_metas$data_file))

  if(file.exists(fnd)) {
    data$data <- qs2::qs_read(fnd, nthreads = getOption("sourcoise.nthreads"))
  }

  return(data)
}

data_returned <- function(data, ctxt) {
  if(!ctxt$metadata)
    return(data$data)
  list(
    ok = data$ok,
    error = data$error,
    data = data$data,
    timing = data$timing,
    date = data$date,
    size = data$size,
    args = ctxt$args,
    lapse = ctxt$lapse,
    track = ctxt$track,
    qmd_file = ctxt$qmd_file,
    log_file = ctxt$log_file,
    data_file = data$data_file,
    file_size = data$file_size,
    data_date = data$data_date,
    json_file = data$json_file
  )
}
