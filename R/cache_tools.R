#' @importFrom rlang .data
cache_data <- function(data, ctxt) {
  pat <- stringr::str_c(ctxt$basename, "_([a-f0-9]{8})-([0-9]+)\\.json")
  files <- tibble::tibble()

  if(fs::dir_exists(ctxt$full_cache_rep)) {
    files <- fs::dir_info(path = ctxt$full_cache_rep, regexp = pat) |>
      dplyr::mutate(uid = stringr::str_extract(.data$path, pat, group=1),
                    cc = stringr::str_extract(.data$path, pat, group=2) |> as.numeric())
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
        tibble::tibble(
          path = .x,
          data_hash = mdata$data_hash,
          data_file = mdata$data_file,
          data_date = mdata$data_date,
          file_size = mdata$file_size)
      }) |>
      dplyr::filter(.data$data_hash == new_data_hash)
    if(nrow(hashes)>0) {
      exists_data_file <- hashes |>
        dplyr::slice(1) |>
        dplyr::pull(.data$data_file) |>
        fs::path_file()

      exists_data_file <- fs::path_join(c(ctxt$full_cache_rep, exists_data_file))
      exists <- fs::file_exists(exists_data_file)
      finfo <- fs::file_info(exists_data_file)
      exists_file_size <- finfo$size
      exists_data_date <- finfo$modification_time |> as.character()
    }
    cc <- max(files$cc, na.rm = TRUE) + 1
  }
  if(!fs::dir_exists(ctxt$full_cache_rep))
    fs::dir_create(ctxt$full_cache_rep, recurse=TRUE)
  data$data_hash <- new_data_hash
  data$id <- stringr::str_c(ctxt$uid, "-", cc)
  data$uid <- ctxt$uid
  data$priority <- ctxt$priority
  data$cc <- cc
  data$json_file <- fs::path_join(
    c(ctxt$full_cache_rep,
      stringr::str_c(ctxt$basename, "_", stringr::str_c(data$id, ".json"))))
  if(!ctxt$nocache) {

    les_metas <- data
    les_metas$data <- NULL
    les_metas$file <- NULL
    les_metas$ok <- NULL
    les_metas$id <- NULL

    if(!exists) {
      fnd <- fs::path_join(
        c(ctxt$full_cache_rep,
          stringr::str_c(ctxt$basename, "_", stringr::str_c(data$data_hash, ".qs2"))))
      qs2::qs_save( data$data, file = fnd, nthreads = getOption("sourcoise.nthreads") )
      f_i <- fs::file_info(fnd)
      les_metas$file_size <- f_i$size
      les_metas$data_date <- f_i$modification_time |> as.character()
      if(f_i$size > ctxt$limit_mb*1024*1024) {
        fs::file_delete(fnd)
        logger::log_warn("cached data not saved because ({scales::label_bytes()(file_size)} is over the {ctxt$limit_md} Mb limit.")
      }
    } else {
      fnd <- exists_data_file
      les_metas$file_size <- exists_file_size
      les_metas$data_date <- exists_data_date
    }
    les_metas$data_file <- data$data_file <- fs::path_file(fnd)
    data$data_date <- les_metas$data_date
    jsonlite::write_json(les_metas, path = data$json_file)
    prune_cache(ctxt)
  }
  return(data)
}

#' @importFrom rlang .data
prune_cache <- function(ctxt) {
  if(is.infinite(ctxt$grow_cache))
    return(NULL)
  md <- get_mdatas(ctxt$basename, ctxt$full_cache_rep)

  pairs <- purrr::imap_dfr(
    md,
    ~tibble::tibble(data_file = .x$data_file, json_file = .y, date = .x$date) )

  datas <- unique(pairs$data_file)
  jsons <- unique(pairs$json_file)
  pairs <- pairs |>
    dplyr::group_by(.data$data_file) |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::summarize(
      date = dplyr::first(.data$date),
      json_file = dplyr::first(.data$json_file)) |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::slice_head(n=ctxt$grow_cache)
  jsons_out <- setdiff(jsons, pairs$json_file)
  datas_out <- setdiff(datas, pairs$data_file)

  sure_delete <- function(fn) {
    if(fs::file_exists(fn))
      fs::file_delete(fn)
  }
  purrr::walk(jsons_out, ~ sure_delete(.x))
  purrr::walk(datas_out, ~ sure_delete(fs::path_join(c(ctxt$full_cache_rep, .x))))
}

# pick les meilleures données en cache
#' @importFrom rlang %||%
pick_gooddata <- function(good_datas, ctxt) {
  dates <- purrr::map(good_datas, "date") |>
    unlist() |>
    lubridate::as_datetime()
  mdd <- which.max(dates)
  good_good_data <- good_datas[[mdd]]
  fnm <- names(good_datas)[[mdd]]
  fnd <- fs::path_join(c(ctxt$full_cache_rep, good_good_data$data_file))

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
    newmdata$log_file <- ctxt$log_file
    newmdata$json_file <- fnm
    jsonlite::write_json(newmdata, path = fnm)
  }

  good_good_data$ok <- "cache"
  good_good_data$json_file <- fnm
  if(getOption("sourcoise.memoize"))
    good_good_data$data <- read_data_from_cache(fnd)
  else
    good_good_data$data <-   qs2::qs_read(fnd, nthreads = getOption("sourcoise.nthreads"))

  return(good_good_data)
}

read_data_from_cache <- function(fnd) {
  qs2::qs_read(fnd, nthreads = getOption("sourcoise.nthreads"))
}

data_returned <- function(data, ctxt) {
  if(!ctxt$metadata)
    return(data$data)
  list(
    ok = data$ok,
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
