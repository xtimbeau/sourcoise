#' Change Priority of Cached Files
#'
#' Updates the priority metadata for cached files associated with a given path.
#' Only affects cache entries where the priority differs from the specified value.
#'
#' @param path Character string specifying the file path whose cache metadata
#'   should be updated.
#' @param priority Numeric priority value to set. Default is 10. Lower values
#'   indicate higher priority.
#' @param root Character string specifying the root directory for the cache.
#'   Defaults to `getOption("sourcoise.root")`.
#'
#' @return Invisibly returns the results of writing metadata for each updated
#'   cache entry. Returns a message string if no files are found or no changes
#'   are needed.
#'
#' @details
#' The function locates all cache entries for the specified path, filters to
#' the most recent entry for each argument hash, and updates the priority
#' metadata only for entries where the current priority differs from the
#' specified value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Set priority to 5 for cached results of a script
#' sourcoise_priority("scripts/analysis.R", priority = 5)
#'
#' # Use default priority of 10
#' sourcoise_priority("scripts/model.R")
#' }
sourcoise_priority <- function(path, priority = 10, root = getOption("sourcoise.root")) {

  ctxt <- setup_context(path = path, root = root, hash = FALSE)
  short_metas <- fast_metadata(
    root=ctxt$root,
    bn = ctxt$name |> fs::path_file(),
    cache_reps = ctxt$full_cache_rep) |>
    tibble::as_tibble() |>
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
        root = ctxt$root,
        uid = metas$uid,
        cachename = ctxt$cachename,
        argid = metas$arg_hash,
        root_cache_rep = ctxt$root_cache_rep)
      write_meta(metas = list(...), local_ctxt)
    })
}

#' Remove Tracking from Cached Files
#'
#' Removes tracking metadata from cached files associated with a given path by
#' setting the track field to an empty list.
#'
#' NOte that tracked fies are accumulated when specified in `track` argument of `soucoise()`.
#' This function allows to reset the list.
#'
#' @param path Character string specifying the file path whose cache metadata
#'   should be updated.
#' @param root Character string specifying the root directory for the cache.
#'   Defaults to `getOption("sourcoise.root")`.
#'
#' @return Invisibly returns the results of writing metadata for each updated
#'   cache entry. Returns a message string if no files are found or no tracked
#'   files exist.
#'
#' @details
#' The function locates all cache entries for the specified path, filters to
#' entries that currently have tracking enabled (non-empty priority field), and
#' removes tracking by setting the track field to an empty list. Only affects
#' the most recent cache entry for each argument hash.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove tracking from cached results
#' sourcoise_untrack("scripts/analysis.R")
#' }
sourcoise_untrack <- function(path, root = getOption("sourcoise.root")) {

  ctxt <- setup_context(path = path, root = root, hash = FALSE)
  short_metas <- fast_metadata(
    root=ctxt$root,
    bn = ctxt$name |> fs::path_file(),
    cache_reps = ctxt$full_cache_rep) |>
    tibble::as_tibble() |>
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
        root = ctxt$root,
        uid = metas$uid,
        cachename = ctxt$cachename,
        argid = metas$arg_hash,
        root_cache_rep = ctxt$root_cache_rep)
      write_meta(metas = list(...), local_ctxt)
    })
}

#' Change Cache Lapse Policy
#'
#' Updates the lapse policy metadata for cached files associated with a given
#' path. The lapse policy determines when cached results should expire.
#'
#' @param path Character string specifying the file path whose cache metadata
#'   should be updated.
#' @param lapse Character string specifying the lapse policy. Default is "never".
#'   Common values include "never", "daily", "weekly", or custom time periods.
#' @param root Character string specifying the root directory for the cache.
#'   Defaults to `getOption("sourcoise.root")`.
#'
#' @return Invisibly returns the results of writing metadata for each updated
#'   cache entry. Returns a message string if no files are found or no changes
#'   are needed.
#'
#' @details
#' The function locates all cache entries for the specified path, filters to
#' the most recent entry for each argument hash, and updates the lapse policy
#' metadata only for entries where the current lapse value differs from the
#' specified value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Set cache to expire daily
#' sourcoise_lapse("scripts/analysis.R", lapse = "daily")
#'
#' # Set cache to never expire (default)
#' sourcoise_lapse("scripts/model.R", lapse = "never")
#' }
sourcoise_lapse <- function(path, lapse = "never", root = getOption("sourcoise.root")) {

  ctxt <- setup_context(path = path, root = root, hash = FALSE)
  short_metas <- fast_metadata(
    root=ctxt$root,
    bn = ctxt$name |> fs::path_file(),
    cache_reps = ctxt$full_cache_rep) |>
    tibble::as_tibble() |>
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
        root = ctxt$root,
        uid = metas$uid,
        cachename = ctxt$cachename,
        argid = metas$arg_hash,
        root_cache_rep = ctxt$root_cache_rep)
      write_meta(metas = list(...), local_ctxt)
    })
}
