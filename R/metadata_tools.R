# valid_meta4meta <- function(meta, root) {
#   cache_dir <- fs::path_dir(meta$json_file)
#   src_hash <- hash_file(fs::path_join(c(root, meta$src)))
#   track_hash <- 0
#
#   if(length(meta$track) >0) {
#     track_files <- purrr::map(meta$track, ~fs::path_join(c(root, .x)))
#     ok_files <- purrr::map_lgl(track_files, fs::file_exists)
#     if(any(ok_files))
#       track_hash <- hash_file(as.character(track_files[ok_files])) |>
#       digest::digest(algo = "sha1")
#     else {
#       cli::cli_alert_warning("invalid track ({track_files[!ok_files]}), please check paths.")
#     }
#   }
#
#   meme_null <- function(x, n, def = 0) ifelse(is.null(x[[n]]), def, x[[n]])
#   meta$valid_src <- meme_null(meta,"src_hash")==src_hash
#   meta$valid_track <- setequal(meta$track_hash, track_hash)
#   meta$data_exists <- fs::file_exists(fs::path_join(c(cache_dir, meta$data_file)))
#   if(meta$lapse != "never") {
#     alapse <- what_lapse(meta$lapse)
#     meta$valid_lapse <- lubridate::now() - lubridate::as_datetime(meta[["date"]]) <= alapse
#   } else
#     meta$valid_lapse <- TRUE
#   meta$valid <- meta$valid_src & meta$valid_track & meta$valid_lapse & meta$data_exists
#   return(meta)
# }
#
# valid_metas <- function(ctxt) {
#
#   meme_null <- function(x, n, def = 0) ifelse(is.null(x[[n]]), def, x[[n]])
#
#   ctxt$meta_datas <- purrr::map(ctxt$meta_datas, ~{
#     .x$valid_src <- meme_null(.x,"src_hash")==ctxt$src_hash
#     .x$valid_arg <- meme_null(.x,"arg_hash", digest::digest(list()))==ctxt$arg_hash
#     .x$valid_track <- setequal(.x$track_hash, ctxt$track_hash)
#     .x$data_exists <- fs::file_exists(fs::path_join(c(ctxt$full_cache_rep, .x$data_file)))
#     if(ctxt$lapse != "never") {
#       alapse <- what_lapse(ctxt$lapse)
#       .x$valid_lapse <- lubridate::now() - lubridate::as_datetime(.x[["date"]]) <= alapse
#     } else
#       .x$valid_lapse <- TRUE
#     .x$valid <- .x$valid_src & .x$valid_arg & .x$valid_track & .x$valid_lapse & .x$data_exists
#     .x
#   })
#
#   return(ctxt)
# }

data_ok <- function(path, basename) {
  pat <- glue::glue(".*{basename}_[0-9a-f]{{32}}\\.qs2")
  if(!stringr::str_detect(path, pat))
    return(FALSE)
  if(!file.exists(path))
    return(FALSE)
  return(TRUE)
}

valid_meta1 <- function(ctxt) {
  if(length(ctxt$meta1) == 0) {
    ctxt$meta_valid <- list(valid = FALSE)
    return(ctxt)
  }

  ctxt$meta_valid <- list(
    valid_src = ctxt$meta1$src_hash == ctxt$src_hash,
    valid_arg = ctxt$meta1$arg_hash == ctxt$arg_hash,
    valid_track = ctxt$meta1$track_hash == (ctxt$track_hash %||% 0),
    data_exists = data_ok(fs::path_join(c(ctxt$full_cache_rep, ctxt$meta1$data_file)), ctxt$cachebasename),
    valid_lapse = ifelse(
      ctxt$lapse != "never",
      lubridate::now() - lubridate::as_datetime(ctxt$meta1[["date"]], tz=Sys.timezone()) <= what_lapse(ctxt$lapse),
      TRUE))

  ctxt$meta_valid$valid <- ctxt$meta_valid$valid_src &
    ctxt$meta_valid$valid_arg &
    ctxt$meta_valid$valid_track &
    ctxt$meta_valid$valid_lapse &
    ctxt$meta_valid$data_exists

  return(ctxt)
}


hash_file <- function(path) {
  purrr::map_chr(path, ~ {
    if(file.exists(.x)) {
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
    if(length(.t)==0|any(is.na(.t)))
      return(0)
    track_files <- purrr::map(.t, ~fs::path_join(c(.r, .x)))
    ok_files <- purrr::map_lgl(track_files, file.exists)
    if(any(ok_files))
      return(hash_file(as.character(track_files[ok_files])) |>
               digest::digest(algo = "sha1"))
    return(0)
  })
}

# get_datas <- function(name, data_rep) {
#   m <- get_mdatas(name, data_rep)
#   dn <- names(m) |>
#     stringr::str_replace(glue::glue(".json"), glue::glue(".qs2")) |>
#     rlang::set_names(names(m))
#   d <- purrr::map(dn, ~qs2::qs_read(.x, nthreads = getOption("sourcoise.nthreads") ))
#   purrr::map(rlang::set_names(names(m)), ~{
#     l <- m[[.x]]
#     l$data <- d[[.x]]
#     l})
# }


get_mdatas <- function(name, data_rep, root=NULL) {
  pat <- "(.+)-([0-9a-f]{8})"
  s1 <- stringr::str_extract(name, pat, group=1)
  s2 <- stringr::str_extract(name, pat, group=2)
  qm <- fast_metadata(root = root, cache_reps = data_rep, bn = s1, argid = s2)
  root <- unique(qm$root)
  if(length(qm$index)==0)
    return(list(meta1 = list(),
                metas = tibble::tibble()))

  qm$root <- NULL
  grps <- qm$uid |>
    as.factor()
  lg <- levels(grps)
  imax <- lapply(split(qm$index, grps), which.max)
  qms <- lapply(qm, \(l) split(l, grps))
  qm <- lapply(qms, \(item) lapply(lg, \(g) item[[g]][[imax[[g]]]]) |> unlist())

  # fast_read_mdata(qm$json_file)
  meta1 <- RcppSimdJson::fload(qm$json_file)
  if(length(qm$json_file)==1) {
    meta1$json_file <- qm$json_file
    return(
      list(meta1 = meta1,
           metas = qm) )}

  # meta1 <- purrr::map(qm$json_file, read_mdata)
  idate <- meta1 |>
    purrr::map("date") |>
    unlist() |>
    lubridate::as_datetime(tz=Sys.timezone()) |>
    which.max()
  meta1 <- meta1[[idate]]
  meta1$json_file <- qm$json_file[[idate]] |>
    fs::path_rel(root)

  return(
    list(meta1 =meta1,
         metas = qm) )
}

fast_read_mdata <- function(paths) {
  if(length(paths$json_file)==0)
    return(tibble::tibble())
  jsons_raw <- RcppSimdJson::fload(paths$json_file, always_list = TRUE)
  n1 <- names(jsons_raw[[1]])
  # cette méthode permet de lire les listes
  simplifies <- rlang::set_names(rep(NA, length(n1)), n1)
  simplifies[c("args", "track", "qmd_file")] <- FALSE

  pat <- "(.+)/\\.sourcoise/(.+)-([a-f0-9]{8})_([a-f0-9]{8})-([0-9]+)\\.json"
  root <- paths$root |> unique()

  jsons_raw |>
    purrr::list_transpose(simplify = simplifies |> as.list(), default = NULL) |>
    purrr::imap(~ tibble::enframe(.x) |>
                  dplyr::rename({{.y}}:= value) |>
                  dplyr::select(-name) ) |>
    dplyr::bind_cols() |>
    dplyr::mutate(
      json_file = paths$json_file,
      basename = paths$basename,
      argid = paths$argid,
      uid = paths$uid,
      index = paths$index,
      upcache_rep = stringr::str_extract(json_file, pat, group=1),
      cache_rep = stringr::str_c(upcache_rep, "/.sourcoise"),
      downcache_rep = purrr::map2_chr(
        cache_rep, basename,
        ~fs::path_join(c(.x, fs::path_dir(.y))) |> fs::path_norm()),
      date = lubridate::as_datetime(date, tz=Sys.timezone()),
      data_date = lubridate::as_datetime(data_date, tz=Sys.timezone()),
      track_hash = as.character(track_hash)) |>
    dplyr::relocate(json_file)
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

# get_ddatas <- function(name, data_rep) {
#   pat <- stringr::str_c(name, "_([a-f0-9]){8}-([0-9]+).qs2")
#   files <- list()
#   if(fs::dir_exists(data_rep))
#     files <- fs::dir_ls(path = data_rep, regexp = pat, fail=FALSE, ignore.case=TRUE)
#   res <- purrr::map(files, ~ qs2::qs_read(.x, nthreads = getOption("sourcoise.nthreads")))
#   names(res) <- files
#   res
# }



# extract_priority <- function(ctxt) {
#   dates <- purrr::list_transpose(ctxt$meta_datas) |>
#     purrr::pluck("date") |>
#     lubridate::as_datetime()
#   mdd <- which.max(dates)
#   return(ctxt$meta_datas[[mdd]][["priority"]])
# }

# extract_data_date <- function(ctxt) {
#   dates <- purrr::list_transpose(ctxt$meta_datas) |>
#     purrr::pluck("data_date") |>
#     lubridate::as_datetime()
#   return(max(dates))
# }

sure_delete <- function(path) {
  if(file.exists(path))
    fs::file_delete(path)
}

exists_file <- function(file, root) {
  purrr::map2(file, root, ~list(c(.y,.x))) |>
    fs::path_join() |>
    file.exists()
}

valid_lapse <- function(lapse, date) {
  purrr::map2_lgl(lapse, date, \(.l, .d) {
    if(.l == "never")
      return(TRUE)
    lapse <- what_lapse(.l)
    lubridate::now() - lubridate::as_datetime(.d, tz=Sys.timezone()) <= lapse
  })
}

ls_cache_files <- function(root=NULL, uid = NULL, bn = NULL, argid = NULL, cache_reps = NULL,
                           qs2 = FALSE) {
  root <- try_find_root(root)
  if(is.null(cache_reps)) {
    cache_reps <- fs::dir_ls(path = root,
                             regexp = "\\.sourcoise$",
                             all = TRUE,
                             type = "directory",
                             recurse = TRUE)
  }

  if(length(cache_reps)==0)
    return(list())
  if(is.null(uid))
    uid <- "[a-f0-9]{8}"

  if(is.null(bn)) {
    recurse <- TRUE
    bn <- ".+"
    subdir <- "." }
  else {
    recurse <- FALSE
    subdir <- fs::path_dir(bn)
    bn <- fs::path_file(bn)
  }

  if(is.null(argid))
    argid <- "[a-f0-9]{8}"
  cache_reps <- rlang::set_names(cache_reps, cache_reps)
  jpat <- "({bn})-({argid})_({uid})-([0-9]+)\\.json" |> glue::glue()
  jsons <- purrr::map(cache_reps, ~{
    rep <- fs::path_join(c(.x, subdir)) |> fs::path_norm()
      if(dir.exists(rep))
        return(fs::dir_ls(rep,  regexp = jpat, recurse = recurse, type = "file")  )
    return(list())
  })
  qs2s <- list()
  if(qs2) {
    dpat <- "({bn})-({argid})_([a-f0-9]{{32}})\\.qs2" |> glue::glue()
    qs2 <- purrr::map(cache_reps,
                      ~fs::dir_ls(.x, regexp = dpat, recurse = TRUE, type = "file")   )
  }
  return(list(
    jsons=jsons,
    qs2=qs2s,
    cache_reps = cache_reps,
    root = root))
}

clean_caches <- function(root=NULL, cache_reps = NULL) {
  root <- try_find_root(root)
  if(is.null(cache_reps))
    cache_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$",
                             type = "directory", all = TRUE, recurse = TRUE)
  caches <- ls_cache_files(root, cache_reps = cache_reps, qs2=TRUE)
  allfiles <- purrr::map(cache_reps,
                         ~fs::dir_ls(.x, type = "file", recurse = TRUE))
  if(length(allfiles)==0)
    return(NULL)

  outfiles <- purrr::list_c(allfiles) |>
    setdiff(caches$jsons |> purrr::list_c()) |>
    setdiff(caches$qs2 |> purrr::list_c())
  if(length(outfiles)>0)
    fs::file_delete(outfiles)
  return(outfiles)
}

fast_metadata <- function(root=NULL, uid = NULL, bn = NULL,
                          argid = NULL, cache_reps = NULL, quiet = FALSE) {
  files <- ls_cache_files(root=root, uid = uid, bn = bn,
                          argid = argid, cache_reps = cache_reps)
  if(length(files$json)==0)
    return(list())
  pat <- ".+/\\.sourcoise/(.+)-([a-f0-9]{8})_([a-f0-9]{8})-([0-9]+)\\.json"
  json_file <- files$jsons |> purrr::list_c() |> unname()
  list(
    json_file = json_file,
    root = files$root,
    basename = stringr::str_extract(json_file, pat, group=1),
    argid = stringr::str_extract(json_file, pat, group=2),
    uid = stringr::str_extract(json_file, pat, group=3),
    index = stringr::str_extract(json_file, pat, group=4) |> as.numeric(),
    short_json_file = stringr::str_remove(json_file, stringr::str_c("^", root, "/")) )

}

get_metadata <- function(root=NULL, uid = NULL,
                         bn = NULL, argid = NULL, cache_reps = NULL,
                         filter = "recent", quiet = FALSE) {

  qm <- fast_metadata(root=root, uid = uid, bn = bn,
                      argid = argid, cache_reps = cache_reps)

  root <- qm$root |> unique()
  qm$root <- NULL
  if(length(qm$index)==0) {
    if(!quiet)
      cli::cli_alert_info("No cache data")
    return(tibble::tibble())
  }

  if(filter == "recent") {
    grps <- stringr::str_c(qm$uid, qm$basename, qm$argid) |>
      as.factor()
    lg <- levels(grps)
    imax <- lapply(split(qm$index, grps), which.max)
    qms <- lapply(qm, \(l) split(l, grps))
    qm <- lapply(qms, \(item) lapply(lg, \(g) item[[g]][[imax[[g]]]]) |> unlist())
  }

  if(length(qm$index)==0) {
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
  pat <- "(.+)/.sourcoise/(.+)-[a-f0-9]{8}_[a-f0-9]{8}-[0-9]+\\.json"

  metas <- jsons_raw |>
    purrr::list_transpose(simplify = simplifies |> as.list(), default = NULL) |>
    purrr::imap(~ tibble::enframe(.x) |>
                  dplyr::rename({{.y}}:= value) |>
                  dplyr::select(-name) ) |>
    dplyr::bind_cols() |>
    dplyr::mutate(
      track_hash = as.character(track_hash),
      name = qm$json_file,
      json_file = qm$short_json_file,
      index = qm$index,
      root = !!root,
      uid = qm$uid,
      upcache_rep = stringr::str_extract(qm$json_file, pattern = pat, group=1),
      cache_rep = stringr::str_c(upcache_rep, "/.sourcoise"),
      src = stringr::str_c(
        upcache_rep,
        "/",
        stringr::str_extract(qm$json_file, pattern = pat, group=2)) |>
        fs::path_rel(!!root) |>
        fs::path_ext_set(".r")) |>
    dplyr::mutate(
      src = ifelse(exists_file(src, root),
                   src,
                   src |> fs::path_ext_set(".R")) ) |>
    dplyr::relocate(name)

  if(nrow(metas) == 0) {
    if(!quiet)
      cli::cli_alert_info("No cache data")
    return(tibble::tibble())
  }

  if(filter=="recent")
    metas <- metas |>
    dplyr::group_by(root, tolower(src), arg_hash) |>
    # dplyr::filter(data_date==max(data_date)) |>
    dplyr::filter(date==max(date)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

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
      metas <- metas |> dplyr::mutate(cur_arg_hash = NA_character_)
  tracks <- metas |>
    tidyr::drop_na(track) |>
    dplyr::distinct(root, track) |>
    dplyr::mutate(
      cur_track_hash = hash_tracks(track, root) )
  if(nrow(tracks)>0)
    metas <- metas |>
    dplyr::left_join(tracks, dplyr::join_by(root, track)) else
      metas <- metas |> dplyr::mutate(cur_track_hash = 0)
  metas <- metas |>
    dplyr::mutate(valid_lapse = valid_lapse(lapse, date))
  metas <- metas |>
    dplyr::mutate(
      cur_arg_hash = ifelse(is.na(cur_arg_hash),
                            digest::digest(list(), algo = "crc32"),
                            cur_arg_hash),
      cur_track_hash = ifelse(purrr::map_lgl(cur_track_hash, ~length(.x)==0),
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
      date = lubridate::as_datetime(date, tz=Sys.timezone()),
      valid, priority, uid, index,
      timing, size, lapse, wd, args,
      json_file, qmd_file, src_in, data_file,
      data_date = lubridate::as_datetime(data_date, tz=Sys.timezone()),
      file_size = fs::as_fs_bytes(file_size),
      log_file, root, src_hash, track_hash,
      track, arg_hash, data_hash)
}
