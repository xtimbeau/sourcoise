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

hash_tracks <- function(tracks, root) {
  purrr::map2(tracks, root, \(.t, .r) {
    if(length(.t)==0|!any(is.na(.t)))
      return(0)
    track_files <- purrr::map(.t, ~fs::path_join(c(.r, .x)))
    ok_files <- purrr::map_lgl(track_files, fs::file_exists)
    if(any(ok_files))
      return(hash_file(as.character(track_files[ok_files])))
    return(0)
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
  sn <- stringr::str_split(name, "-")[[1]]
  qm <- fast_metadata(cache_reps = data_rep, bn = sn[[1]], argsid = sn[[2]])
  if(nrow(qm)==0)
    return(list())
  qm <- qm |>
    dplyr::group_by(uid) |>
    dplyr::filter(index == max(index)) |>
    dplyr::ungroup()
  purrr::map(qm$json_file, read_mdata)

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
  sure_delete(path)
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

sourcoise_priority <- function(path, priority = 10, root = getOption("sourcoise.root")) {
  name <- remove_ext(path)
  paths <- find_project_root()
  root <- try_find_root(root, "project") |> as.character()

  uid <- digest::digest(as.character(root), algo = "crc32")
  src <- fs::path_rel(path, root)
  bbnme <- fs::path_file(src)
  reldirname <- fs::path_dir(src)
  full_cache_rep <- NULL
  sourcoise_rep <- fs::path_join(c(root, reldirname, ".sourcoise"))
  pat <- stringr::str_c(bbnme, "-([a-f0-9]{8})_([a-f0-9]{8})-([0-9]+)\\.json")
  if(fs::dir_exists(sourcoise_rep)) {
    sourcoises <- fs::dir_ls(sourcoise_rep, regexp = pat)
    istheresourcoise <- length(sourcoises)>0
    if(istheresourcoise) {
      file_path <- fs::path_dir(src)
      full_cache_rep <- sourcoise_rep
    }
  }
  if(is.null(full_cache_rep)) {
    sourcoise_rep <- fs::path_join(c(root, ".sourcoise", reldirname))
    if(fs::dir_exists(sourcoise_rep)) {
      sourcoises <- fs::dir_ls(sourcoise_rep, regexp = pat)
      istheresourcoise <- length(sourcoises)>0
      if(istheresourcoise) {
        file_path <- fs::path_dir(src)
        full_cache_rep <- sourcoise_rep
      }
    }
  }

  if(is.null(full_cache_rep))
    return(NULL)

  json_files <- sourcoises |>
    tibble::tibble(path = _) |>
    dplyr::mutate(
      argsid = stringr::str_extract(path, pat, group=1),
      uid = stringr::str_extract(path, pat, group=2),
      cc = stringr::str_extract(path, pat, group=3) |> as.numeric(),
      date = purrr::map_chr(path, ~read_mdata(.x) |> purrr::pluck("date")) |>
        lubridate::as_datetime())
  most_recent <- json_files |>
    dplyr::group_by(argsid) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  purrr::map(most_recent$argsid, ~{
    les_metas <- most_recent |>
      dplyr::filter(argsid==.x) |>
      dplyr::pull(path) |>
      read_mdata()
    if(les_metas$priority == priority)
      return(NULL)
    les_metas$priority <- priority
    les_metas$date <- lubridate::now()
    # on fait un nouveau nom
    cc <- json_files |>
      dplyr::filter(argsid==.x, uid == !!uid) |>
      dplyr::pull(cc)
    if(length(cc)==0)
      cc <- 0
    cc <- max(cc) + 1
    json_fn <- fs::path_join(c(full_cache_rep,
                               stringr::str_c(bbnme, "-", .x, "_", uid, "-", cc ))) |>
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

sure_delete <- function(path) {
  if(fs::file_exists(path))
    fs::file_delete(path)
}

exists_file <- function(file, root) {
  purrr::map2_lgl(file, root, \(.f, .r) {
    fs::file_exists(fs::path_join(c(.r, .f)))
  })
}

valid_lapse <- function(lapse, date) {
  maintenant <- lubridate::now()
  purrr::map2_lgl(lapse, date, \(.l, .d) {
    if(.l == "never")
      return(TRUE)
    lapse <- what_lapse(.l)
    maintenant - lubridate::as_datetime(.d) <= lapse
  })
}

ls_cache_files <- function(root=NULL, uid = NULL, bn = NULL, argsid = NULL, cache_reps = NULL) {
  if(is.null(cache_reps)) {
    root <- try_find_root(root)
    cache_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$", all = TRUE, recurse = TRUE)
  }
  roots <- fs::path_dir(cache_reps) |> as.character()
  cache_reps <- cache_reps |> purrr::keep(~fs::dir_exists(.x))
  if(length(cache_reps)==0)
    return(list())
  if(length(roots)==0)
    return(NULL)
  if(is.null(uid))
    uid <- "[a-f0-9]{8}"
  if(is.null(bn))
    bn <- ".+"
  if(is.null(argsid))
    argsid <- "[a-f0-9]{8}"
  cache_reps <- rlang::set_names(cache_reps, roots)
  jpat <- "({bn})-({argsid})_({uid})-([0-9]+)\\.json" |> glue::glue()
  dpat <- "({bn})-({argsid})_([a-f0-9]{{32}})\\.qs2" |> glue::glue()
  jsons <- purrr::map(cache_reps,
                      ~fs::dir_ls(.x,  regexp = jpat, recurse = TRUE))
  qs2 <- purrr::map(cache_reps,
                    ~fs::dir_ls(.x, regexp = dpat, recurse = TRUE))
  return(list(qs2=qs2,
              jsons=jsons,
              cache_reps = cache_reps,
              roots = roots|> as.character()))
}

clean_caches <- function(root=NULL, cache_reps = NULL) {
  root <- try_find_root(root)
  if(is.null(cache_reps))
    cache_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$",
                             type = "directory", all = TRUE, recurse = TRUE)
  caches <- ls_cache_files(root, cache_reps = cache_reps)
  allfiles <- purrr::map(cache_reps,
                         ~fs::dir_ls(.x, type = "file", recurse = TRUE))
  if(length(allfiles)==0)
    return(NULL)
  outfiles <- purrr::list_c(allfiles) |>
    setdiff(purrr::list_c(caches$jsons)) |>
    setdiff(purrr::list_c(caches$qs2))
  if(length(outfiles)>0)
    fs::file_delete(outfiles)
  return(outfiles)
}

fast_metadata <- function(root=NULL, uid = NULL, bn = NULL,
                          argsid = NULL, cache_reps = NULL, quiet = FALSE) {
  files <- ls_cache_files(root=root, uid = uid, bn = bn,
                          argsid = argsid, cache_reps = cache_reps)
  if(length(files$json)==0)
    return(tibble::tibble())
  pat <- "(.+)/\\.sourcoise/(.+)-([a-f0-9]{8})_([a-f0-9]{8})-([0-9]+)\\.json"

  tibble::tibble(json_file = files$jsons |> purrr::flatten_chr() |> unname()) |>
    dplyr::mutate(
      root = stringr::str_extract(json_file, pat, group=1),
      basename = stringr::str_extract(json_file, pat, group=2),
      argsid = stringr::str_extract(json_file, pat, group=3),
      uid = stringr::str_extract(json_file, pat, group=4),
      index = stringr::str_extract(json_file, pat, group=5) |> as.numeric(),
      short_json_file = stringr::str_remove(json_file, stringr::str_c("^", root, "/")) ) |>
    dplyr::relocate(basename, argsid, uid, index,
                    root, short_json_file, json_file) |>
    dplyr::arrange(basename, argsid, uid, dplyr::desc(index))
}

get_metadata <- function(root=NULL, uid = NULL,
                         bn = NULL, argsid = NULL, cache_reps = NULL,
                         filter = "recent", quiet = FALSE) {
  qm <- fast_metadata(root=root, uid = uid, bn = bn,
                      argsid = argsid, cache_reps = cache_reps)

  if(nrow(qm)==0) {
    if(!quiet)
      cli::cli_alert_info("No cache data")
    return(tibble::tibble())
  }

  if(filter == "recent")
    qm <- qm |>
      dplyr::group_by(root, basename, argsid, uid) |>
      dplyr::filter(index==max(index)) |>
      dplyr::ungroup()

  if(nrow(qm)==0) {
    if(!quiet)
      cli::cli_alert_info("No cache data")
    return(tibble::tibble())
  }

  # metas <- purrr::map2_dfr(qm$root, qm$json_file, \(a_root, a_json) {
  #   jsonlite::read_json(a_json, simplifyVector = TRUE) |> purrr::flatten() }) |>
  #   dplyr::mutate(root = qm$root, cache_rep = stringr::str_c(root, "/.sourcoise"))
  jsons_raw <- RcppSimdJson::fload(qm$json_file, always_list = TRUE)
  n1 <- names(jsons_raw[[1]])
  # cette méthode permet de lire les listes
  simplifies <- rlang::set_names(rep(NA, length(n1)), n1)
  simplifies[c("args", "track", "qmd_file")] <- FALSE
  metas <- jsons_raw |>
    purrr::list_transpose(simplify = simplifies |> as.list(), default = NULL) |>
    purrr::imap(~ tibble::enframe(.x) |>
                  dplyr::rename({{.y}}:= value) |>
                  dplyr::select(-name) ) |>
    dplyr::bind_cols() |>
    dplyr::mutate(
      name = qm$json_file,
      root = qm$root,
      cache_rep = stringr::str_c(root, "/.sourcoise")) |>
    dplyr::relocate(name)

  if(nrow(metas) == 0) {
    if(!quiet)
      cli::cli_alert_info("No cache data")
    return(tibble::tibble())
  }

  if(! "args" %in% names(metas))
    metas <- metas |> dplyr::mutate(args = NA_character_)
  if(! "log_file" %in% names(metas))
    metas <- metas |> dplyr::mutate(log_file = NA_character_)
  if(! "args" %in% names(metas))
    metas <- metas |> dplyr::mutate(args = NA_character_)
  if(! "track" %in% names(metas))
    metas <- metas |> dplyr::mutate(track = NA_character_)
  if(! "qmd_file" %in% names(metas))
    metas <- metas |> dplyr::mutate(qmd_file = NA_character_)

  metas <- metas |>
    dplyr::mutate(
      src_exist = exists_file(src, root),
      full_cache_rep = purrr::map2_chr(cache_rep, fs::path_dir(src),
                                       ~fs::path_join(c(.x, .y))),
      data_exist = exists_file(data_file,
                               full_cache_rep))
  sources <- metas |>
    dplyr::distinct(src, root) |>
    dplyr::mutate(
      ffn = stringr::str_c(root,"/", src),
      cur_src_hash = hash_file(ffn) ) |>
    dplyr::select(root, src, cur_src_hash)
  metas <- metas |>
    dplyr::left_join(sources, dplyr::join_by(src, root))
  args <- metas |>
    dplyr::distinct(args) |>
    tidyr::drop_na() |>
    dplyr::mutate(cur_arg_hash =  purrr::map_chr(args, ~digest::digest(.x, "crc32")))
  if(nrow(args)>0)
    metas <- metas |>
    dplyr::left_join(args, dplyr::join_by(args)) else
      metas <- metas |> dplyr::mutate(cur_arg_hash =  NA_character_)
  tracks <- metas |>
    tidyr::drop_na(track) |>
    dplyr::distinct(root, track) |>
    dplyr::mutate(
      cur_track_hash = hash_tracks(track, root) )
  if(nrow(tracks)>0)
    metas <- metas |>
    dplyr::left_join(tracks, dplyr::join_by(root, track)) else
      metas <- metas |> dplyr::mutate(cur_track_hash = NA_character_)
  metas <- metas |>
    dplyr::mutate(valid_lapse = valid_lapse(lapse, date))
  metas <- metas |>
    dplyr::mutate(
      cur_arg_hash = ifelse(is.na(cur_arg_hash),
                            digest::digest(list(), algo = "crc32"),
                            cur_arg_hash),
      cur_track_hash = ifelse(is.na(cur_track_hash),
                              0,
                              cur_track_hash),
      log_file = ifelse(purrr::map_lgl(log_file, length), "", log_file))
  metas <- metas |>
    dplyr::mutate(
      valid_src = cur_src_hash == src_hash,
      valid_args = cur_arg_hash == arg_hash,
      valid_track = track_hash == cur_track_hash,
      valid = valid_src &
        valid_args &
        valid_lapse &
        valid_track &
        src_exist &
        data_exist)
  metas |>
    dplyr::transmute(
      src,
      exists = src_exist,
      date = lubridate::as_datetime(date),
      valid, priority, uid, index = cc,
      timing, size, lapse, wd, args,
      json_file, qmd_file, src_in, data_file,
      data_date = lubridate::as_datetime(data_date),
      file_size = scales::label_bytes()(file_size),
      log_file, root, src_hash, track_hash,
      track, arg_hash, data_hash)
}

