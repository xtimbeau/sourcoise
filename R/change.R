sourcoise_priority <- function(path, priority = 10, root = getOption("sourcoise.root")) {

  ctxt <- setup_context(path = path, root = root, hash = FALSE)
  short_metas <- fast_metadata(
    root=ctxt$root,
    bn = ctxt$name |> fs::path_file(),
    cache_reps = ctxt$full_cache_rep) |>
    dplyr::group_by(argid, uid) |>
    dplyr::filter(index == max(index)) |>
    dplyr::ungroup()
  if(nrow(short_metas)==0)
    return("no file to change found")

  long_metas <- fast_read_mdata(short_metas) |>
    dplyr::group_by(arg_hash) |>
    dplyr::filter(date == max(date)) |>
    dplyr::filter(priority != !!priority) |>
    dplyr::ungroup()
  if(nrow(long_metas)==0)
    return("no change in priority found")
  long_metas |>
    dplyr::mutate(priority = !!priority) |>
    purrr::pmap(\(...) {
      metas <- list(...)
      local_ctxt <- list(
        root = metas$root,
        uid = metas$uid,
        cachename = ctxt$cachename,
        argid = metas$arg_hash,
        full_cache_rep = ctxt$full_cache_rep)
      write_meta(metas = list(...), local_ctxt) |>
        fs::path_rel(getwd())
    })
}

sourcoise_untrack <- function(path, root = getOption("sourcoise.root")) {

  ctxt <- setup_context(path = path, root = root, hash = FALSE)
  short_metas <- fast_metadata(
    root=ctxt$root,
    bn = ctxt$name |> fs::path_file(),
    cache_reps = ctxt$full_cache_rep) |>
    dplyr::group_by(argid, uid) |>
    dplyr::filter(index == max(index))
  if(nrow(short_metas)==0)
    return("no file to change found")
  long_metas <- fast_read_mdata(short_metas) |>
    dplyr::group_by(arg_hash) |>
    dplyr::filter(date == max(date)) |>
    dplyr::filter(purrr::map_dbl(priority, length)>0)
  if(nrow(long_metas)==0)
    return("no file with track found")
  long_metas |>
    dplyr::mutate(track = list(list())) |>
    purrr::pmap(\(...) {
      metas <- list(...)
      local_ctxt <- list(
        root = metas$root,
        uid = metas$uid,
        cachename = ctxt$cachename,
        argid = metas$arg_hash,
        full_cache_rep = ctxt$full_cache_rep)
      write_meta(metas = list(...), local_ctxt) |>
        fs::path_rel(getwd())
    })
}

sourcoise_lapse <- function(path, lapse = "never", root = getOption("sourcoise.root")) {

  ctxt <- setup_context(path = path, root = root, hash = FALSE)
  short_metas <- fast_metadata(
    root=ctxt$root,
    bn = ctxt$name |> fs::path_file(),
    cache_reps = ctxt$full_cache_rep) |>
    dplyr::group_by(argid, uid) |>
    dplyr::filter(index == max(index))
  if(nrow(short_metas)==0)
    return("no file to change found")
  long_metas <- fast_read_mdata(short_metas) |>
    dplyr::group_by(arg_hash) |>
    dplyr::filter(date == max(date)) |>
    dplyr::filter(lapse != !!lapse)
  if(nrow(long_metas)==0)
    return("no change in lapse found")
  long_metas |>
    dplyr::mutate(lapse = !!lapse) |>
    purrr::pmap(\(...) {
      metas <- list(...)
      local_ctxt <- list(
        root = metas$root,
        uid = metas$uid,
        cachename = ctxt$cachename,
        argid = metas$arg_hash,
        full_cache_rep = ctxt$full_cache_rep)
      write_meta(metas = list(...), local_ctxt) |>
        fs::path_rel(getwd())
    })
}
