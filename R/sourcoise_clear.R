#' Cleans sourcoise cache
#'
#' removes every json and qs2 files found by `sourcoise_status()` unless a specific tibble (filtered from `sourcoise_status()`) is passed as an argument.
#'
#' @param what (--) a tibble such as the one obtained by `sourcoise_status()`, possibly filtered
#' @param root to force root, not recommended (expert use only)
#'
#' @family sourcoise
#'
#' @return list of cleared files, plus a side-effect as specified cache files are deleted (no undo possible)
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
#' # we then clear all caches
#' sourcoise_clear()
#' sourcoise_status()

sourcoise_clear <- function(
    what2keep = "last",
    root = NULL) {

  root <- try_find_root(root)

  sure_delete <- function(fn) {
    if(fs::file_exists(fn))
      fs::file_delete(fn)
  }
  ww <- sourcoise_status(short = FALSE, prune = FALSE)

  if(what2keep=="last") {
    pat <- "(.+)-([a-f0-9]{8})_([a-f0-9]{8})-([0-9]+)\\.json"
    what2keep <- ww |>
      dplyr::mutate(
        bn = stringr::str_extract(.data$json_file, pattern = pat, group = 1),
        argsid = stringr::str_extract(.data$json_file, pat, group=2),
        uid = stringr::str_extract(.data$json_file, pat, group=3),
        cc = stringr::str_extract(.data$json_file, pat, group=4) |> as.numeric(),
        date = purrr::map_chr(.data$json_file, ~read_mdata(.x) |> purrr::pluck("date")) |>
          lubridate::as_datetime() ) |>
      dplyr::group_by(src, argsid) |>
      dplyr::arrange(.data$date, .data$cc) |>
      dplyr::slice_tail(n=1) |>
      dplyr::ungroup()
  }

  json2del <- setdiff(ww[["json_file"]], what2keep[["json_file"]])
  qs22del <- setdiff(ww[["data_file"]], what2keep[["data_file"]])
  purrr::walk(json2del, sure_delete)
  purrr::walk(qs22del, sure_delete)
  c(json2del, qs22del)
}

#' Resets sourcoise
#'
#' Removes all `.sourcoise` folders found under the project root.
#'
#' @param root to force root (expert use)
#'
#' @family sourcoise
#'
#' @return No return, effect is through removal of .sourcoise folders (this is a side effect, no undo possible)
#' @export
#' @examplesIf rlang::is_installed("insee")
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#' fs::file_copy(
#'    fs::path_package("sourcoise", "some_data.R"),
#'    dir,
#'    overwrite = TRUE)
#' data <- sourcoise("some_data.R", force_exec = TRUE)
#' sourcoise_reset()

sourcoise_reset <- function(
    root = NULL) {

  root <- try_find_root(root)

  caches_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise$", all = TRUE, recurse = TRUE)

  purrr::walk(caches_reps, ~fs::dir_delete(.x))

}
