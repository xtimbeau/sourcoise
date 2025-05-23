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
#' -  `src_in`: localisaiton of cache option
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
#' @param quiet (boolean) (default `TRUE`) no messages during execution
#' @param root (string) (default `NULL`) force root to a defined path, advanced and not recommanded use
#' @param prune (boolean) (default `TRUE`) clean up status to display only on relevant cache. However, does not clean other cache files.
#' @param clean (boolean) (default `FALSE`) check if some data files have not json referring to them and cleans if any.
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
    quiet = TRUE,
    root = NULL,
    prune = TRUE,
    clean = FALSE) {

  root <- try_find_root(root, src_in = "project")
  caches_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$", all = TRUE, recurse = TRUE)
  roots <- fs::path_dir(caches_reps)
  caches_reps <- rlang::set_names(caches_reps, roots)

  jsons <- purrr::map(caches_reps,
                      ~fs::dir_ls(.x, glob = "*.json", recurse = TRUE))
  qs2 <- purrr::map(caches_reps,
                    ~fs::dir_ls(.x, glob = "*.qs2", recurse = TRUE))

  if(length(roots)>0) {
    cached <- purrr::map_dfr(roots, \(a_root) {
      purrr::map_dfr(jsons[[a_root]], ~{
        dd <- read_mdata(.x)
        valid <- valid_meta4meta(dd, root = a_root)
        if(is.null(dd$log_file)||length(dd$log_file)==0)
          log_file <- ""
        else
          log_file <- dd$log_file

        tibble::tibble(
          src = tolower(dd$src),
          date = lubridate::as_datetime(dd$date),
          valid = valid$valid,
          priority = dd$priority %||% 10,
          uid = dd$uid,
          index = dd$cc |> as.numeric(),
          timing = dd$timing,
          size = scales::label_bytes()(dd$size),
          lapse = dd$lapse |> as.character(),
          wd = dd$wd,
          args = list(dd$args),
          json_file = fs::path_rel(.x, a_root),
          qmd_file = list(dd$qmd_files),
          src_in = dd$src_in,
          data_file = dd$data_file,
          data_date = dd$data_date,
          file_size = scales::label_bytes()(dd$file_size),
          log_file = log_file,
          root =  a_root,
          src_hash = dd$src_hash,
          track_hash = list(dd$track_hash),
          track = list(dd$track),
          args_hash = dd$args_hash,
          data_hash = dd$data_hash)
      })
    })

    if(clean) {
      qs2_jsoned <- purrr::pmap_chr(cached, \(root, json_file, data_file, ...) {
        dir <- fs::path_join(c(root, json_file)) |>
          fs::path_dir()
        fs::path_join(c(dir, data_file))
      })
      qs2_orphed <- setdiff(qs2 |> purrr::list_c(), qs2_jsoned)
      purrr::walk(qs2_orphed, fs::file_delete)
    }
    if(nrow(cached)>0) {
      cached <- cached |>
        dplyr::arrange(.data$src, dplyr::desc(.data$date))

      if(prune)
        cached <- cached |>
          dplyr::group_by(.data$src, .data$args) |>
          dplyr::filter(.data$date == max(.data$date)) |>
          dplyr::ungroup()

      return(cached)
    }
  }
  if(!quiet)
    cli::cli_alert_info("No cache data")
  return(tibble::tibble())
}
