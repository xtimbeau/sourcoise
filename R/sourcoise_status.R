#' Cache status of sourcoise
#'
#' Given the current project, `soucoise_status()` collects all information about cache (could be project level, file level)
#' and return a tibble with this data.
#'
#' `sourcoise_status()` reflects what is on the disk (and results indeed from a scan of all cached files and their metadatas).
#' So modifying the result of `sourcoise_status()` can produce complex bugs when it is passed to `sourcoise_refresh()` or `sourcoise_clean()`.
#'
#' Data returned is:
#' -  `src`: path to the source file (r script)
#' -  `date`: last execution date
#' -  `valid`: is cache valid ?
#' -  `uid`: id of user
#' -  `index`: index of cache
#' -  `timing`: last execution timing
#' -  `size`: size of the R object(s) returned
#' -  `lapse`: periodic refresh trigger
#' -  `wd`: wd setting for execution of r script
#' -  `args`: arguments passed to R script
#' -  `json_file`: path to the file keeping cache information
#' -  `qmd_file`: list of path to qmd files calling this script (relevant only for quarto projects)
#' -  `src_in`: localisation of cache option
#' -  `data_file`: path to data cached
#' -  `data_date`: date and time of last save of data
#' -  `log_file`: path to log file, if logging activated
#' -  `root`: path to the project root, used as reference for all paths
#' -  `scr_hash`: hash of the source file
#' -  `track_hash`: hash of the tracked files, if any
#' -  `track`: list of files tracked
#' -  `args_hash`: hash of arguments
#' -  `data_hash`: hash of data cached
#'
#' @param short (boolean) (deafault `TRUE`) return a simplified tibble
#' @param quiet (boolean) (default `TRUE`) no messages during execution
#' @param root (string) (default `NULL`) force root to a defined path, advanced and not recommanded use
#' @param prune (boolean) (default `TRUE`) clean up status to display only on relevant cache. However, does not clean other cache files.
#' @family sourcoise
#'
#' @importFrom rlang .data %||%
#' @return tibble of cached files (see details for structure)
#' @export
#' @examplesIf rlang::is_installed("insee")
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#' fs::file_copy(
#'     fs::path_package("sourcoise", "some_data.R"),
#'     dir,
#'     overwrite = TRUE)
#' # Force execution
#' data <- sourcoise("some_data.R", force_exec = TRUE)
#' # status returns the cache status
#' sourcoise_status()

sourcoise_status <- function(
    short = TRUE,
    quiet = TRUE,
    root = NULL,
    prune = TRUE) {

  root <- try_find_root(root, src_in = "project")
  if(prune)
    cached <- get_metadata(root = root, filter = "recent")
  if(!prune)
    cached <- get_metadata(root = root, filter = "all")

  if(nrow(cached)>0) {
    cached <- cached |>
      dplyr::arrange(dplyr::desc(.data$priority), .data$src, dplyr::desc(.data$date)) |>
      dplyr::mutate(
        last_exec = format_timespan(date),
        last_update = format_timespan(data_date) )

    if(short)
      cached <- cached |> dplyr::select(valid, priority, src, last_exec, last_update, file_size, json_file)

    return(cached)
  }

  if(!quiet)
    cli::cli_alert_info("No cache data")

  return(tibble::tibble())
}
