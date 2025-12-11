#' Cleans sourcoise cache
#'
#' removes every json and qs2 files found by `sourcoise_status()` unless a specific tibble (filtered from `sourcoise_status()`) is passed as an argument.
#'
#' @param what2keep (--) a string (such as "last", the default or "nothing" clears all or "all" removes only non sourcoise files) or a tibble such as the one obtained by `sourcoise_status()`, possibly filtered for the files you whish to keep
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
#' @importFrom rlang .data

sourcoise_clear <- function(
    what2keep = "all",
    root = NULL) {
  clean_caches(root=root)
  if(what2keep=="all") {
    return(character(0))
  }
  fc <- ls_cache_files(root, qs2=TRUE)
  if(length(fc$json)==0)
    return(character(0))
  if(what2keep=="nothing") {
    ww <- list(json_file = list(), data_file = list())
  }
  if(what2keep=="recent") {
    ww <- get_metadata(root, filter="recent") |>
      dplyr::group_by(root, basename, argsid, uid) |>
      dplyr::filter(index==max(index)) |>
      dplyr::ungroup()
  }
  json2del <- setdiff(fc[["jsons"]] |> unlist()  |> unname(), ww[["json_file"]])
  qs22del <- setdiff(fc[["qs2"]] |> unlist() |> unname(), ww[["data_file"]])
  purrr::walk(json2del, sure_delete)
  purrr::walk(qs22del, sure_delete)
  c(json2del, qs22del)
}


#' Cleans sourcoise cache
#'
#' removes every json and qs2 files found by `sourcoise_status()`.
#'
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
#' sourcoise_clear_all()
#' sourcoise_status()
#' @importFrom rlang .data

sourcoise_clear_all <- function(root = NULL) {
  sourcoise_clear(what2keep = "nothing", root=root)
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

  caches_reps <- fs::dir_ls(path = root, regex = "\\.sourcoise", type = "directory", all = TRUE, recurse = TRUE)

  purrr::walk(caches_reps, ~fs::dir_delete(.x))

}
