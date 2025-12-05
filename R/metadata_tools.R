valid_meta4meta <- function(meta, root) {
  cache_dir <- fs::path_dir(meta$json_file)
  src_hash <- hash_file(fs::path_join(c(root, meta$src)))
  track_hash <- 0

  if(length(meta$track) >0) {
    track_files <- purrr::map(meta$track, ~fs::path_join(c(root, .x)))
    ok_files <- purrr::map_lgl(track_files, fs::file_exists)
    if(any(ok_files))
      track_hash <- hash_file(as.character(track_files[ok_files]))
    else {
      cli::cli_alert_warning("invalid track ({track_files[!ok_files]}), please check paths.")
    }
  }

  meme_null <- function(x, n, def = 0) ifelse(is.null(x[[n]]), def, x[[n]])
  meta$valid_src <- meme_null(meta,"src_hash")==src_hash
  meta$valid_track <- setequal(meta$track_hash, track_hash)
  meta$data_exists <- fs::file_exists(fs::path_join(c(cache_dir, meta$data_file)))
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
    .x$data_exists <- fs::file_exists(fs::path_join(c(ctxt$full_cache_rep, .x$data_file)))
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
  name_allid <- stringr::str_remove(name, "-([a-f0-9]){8}")
  pat <- stringr::str_c(name_allid, "-([a-f0-9]{8})_([a-f0-9]{8})-([0-9]+).json")
  files <- list()
  if(fs::dir_exists(data_rep))
    files <- fs::dir_ls(path = data_rep, regexp = pat, fail=FALSE, ignore.case = TRUE)
  purrr::map(files, read_mdata)

  # mdatas <- RcppSimdJson::fload(files)
  # if(length(files)==1)
  #   mdatas <- list(mdatas)
  # names(mdatas) <- files
  # purrr::imap(mdatas, ~{
  #   r <- .x
  #   r$file <- .y
  #   r})
}

read_json_safe <- purrr::safely(jsonlite::read_json)

read_mdata <- function(path) {
  l <- read_json_safe(path, simplifyVector = TRUE)
  if(is.null(l$error)) {
    l <- l$result
    l$json_file <- path
    return(l)
  }
  logger::log_error("{path} is not a json file, {l$error$message} [deleting json]")
  fs::file_delete(path)
  l <- list()
}

get_ddatas <- function(name, data_rep) {
  pat <- stringr::str_c(name, "_([a-f0-9]){8}-([0-9]+).qs2")
  files <- list()
  if(fs::dir_exists(data_rep))
    files <- fs::dir_ls(path = data_rep, regexp = pat, fail=FALSE, ignore.case=TRUE)
  res <- purrr::map(files, ~ qs2::qs_read(.x, nthreads = getOption("sourcoise.nthreads")))
  names(res) <- files
  res
}

#' Set priority for sourcoise
#'
#' The desingated file is set to the priority. Low priority means that when refreshing those files are groing
#' to be executed first; the reason being that this file is used by other and that you prefer it is refreshed before refreshing other.
#' However, when refreshing, subcall to sourcoise are executed, unless priority is set to 0
#'
#' So, if you want to refresh it first and then not (because it is long to refresh), then, ste it to 0
#'
#' Default priority is 10
#'
#' @param path the file for which priority is set
#' @param priority the level of priority (10 is the default)
#' @param root the root -- use only if you know what it does
#'
#' @returns list of json created
#' @export
#'

sourcoise_priority <- function(path, priority = 10, root = getOption("sourcoise.root")) {
  ctxt <- setup_context(
    path = path,
    root = root,
    src_in = "project",
    exec_wd = NULL,
    wd = "file",
    track = list(),
    args = list(),
    lapse = "never",
    nocache = FALSE,
    grow_cache = Inf,
    limit_mb = 0,
    log = FALSE,
    inform = FALSE,
    quiet = TRUE,
    metadata = FALSE)
  if(!fs::dir_exists(ctxt$full_cache_rep))
    return(NULL)
  bbnme <- ctxt$basename |> stringr::str_remove("-[a-f0-9]{8}")
  pat <- stringr::str_c(bbnme, "-([a-f0-9]{8})_([a-f0-9]{8})-([0-9]+)\\.json")
  json_files <- fs::dir_info(path = ctxt$full_cache_rep, regexp = pat) |>
    select(path) |>
    dplyr::mutate(
      argsid = stringr::str_extract(.data$path, pat, group=1),
      uid = stringr::str_extract(.data$path, pat, group=2),
      cc = stringr::str_extract(.data$path, pat, group=3) |> as.numeric(),
      date = purrr::map_chr(path, ~jsonlite::read_json(.x) |>
                              pluck("date") |>
                              unlist()) |>
        lubridate::as_datetime())
  most_recent <- json_files |>
    group_by(argsid) |>
    arrange(desc(date)) |>
    slice(1) |>
    ungroup()
  purrr::map(most_recent$argsid, ~{
    les_metas <- most_recent |>
      filter(argsid==.x) |>
      pull(path) |>
      read_mdata()
    if(les_metas$priority == priority)
      return(NULL)
    les_metas$priority <- priority
    les_metas$date <- lubridate::now()
    # on fait un nouveau nom
    cc <- json_files |>
      filter(argsid==.x, uid == ctxt$uid) |>
      pull(cc)
    if(length(cc)==0)
      cc <- 0
    cc <- max(cc) + 1
    json_fn <- fs::path_join(c(ctxt$full_cache_rep,
                               str_c(bbnme, "-", .x, "_", ctxt$uid, "-", cc ))) |>
      fs::path_ext_set("json")
    jsonlite::write_json(les_metas, path = json_fn)
    json_fn
  }) |> unlist()
}

extract_priority <- function(ctxt) {
  dates <- purrr::map(ctxt$meta_datas, "date") |>
    unlist() |>
    lubridate::as_datetime()
  mdd <- which.max(dates)
  return(ctxt$meta_datas[[mdd]][["priority"]])
}

extract_data_date <- function(ctxt) {
  dates <- purrr::map(ctxt$meta_datas, "data_date") |>
    unlist() |>
    lubridate::as_datetime()
  return(max(dates))
}
