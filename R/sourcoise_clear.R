#' Cleans sourcoise cache
#'
#' Always removes files in `.sourcoise` folders that do not belong to the cache (orphans), then removes cache files according to `what2keep`.
#'
#' @param what2keep (character) one of `"all"` (the default) keeps every cache file, so only orphan files are removed, `"recent"` keeps, for each script and set of arguments, only the latest cached data and its metadata, and `"nothing"` removes the whole cache.
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
  what2keep <- rlang::arg_match0(what2keep, c("all", "recent", "nothing"))
  clean_caches(root=root)
  if(what2keep=="all") {
    return(character(0))
  }
  fc <- ls_cache_files(root, qs2=TRUE)
  jsons <- fc[["jsons"]] |> unlist() |> unname() |> as.character()
  qs2s <- fc[["qs2"]] |> unlist() |> unname() |> as.character()
  if(length(jsons)==0)
    return(character(0))

  keep <- list(json = character(0), data = character(0))
  if(what2keep=="recent")
    keep <- most_recent_cache_files(root, fc$cache_reps)

  json2del <- setdiff(jsons, keep$json)
  qs22del <- setdiff(qs2s, keep$data)
  purrr::walk(json2del, sure_delete)
  purrr::walk(qs22del, sure_delete)
  c(json2del, qs22del)
}

# json files holding the latest metadata of each script/args combination,
# and the data files they point to (data lives next to its json)
most_recent_cache_files <- function(root, cache_reps) {
  fm <- fast_metadata(root = root, cache_reps = cache_reps)
  if(length(fm$json_file)==0)
    return(list(json = character(0), data = character(0)))
  json_file <- as.character(fm$json_file)
  key <- stringr::str_c(
    fs::path_dir(json_file), fs::path_file(fm$basename), fm$argid, fm$uid,
    sep = "|")
  json_keep <- json_file[tapply(seq_along(key), key, \(i) i[which.max(fm$index[i])])]
  data_keep <- purrr::map_chr(json_keep, \(j) {
    # scalars are stored as length-1 json arrays
    data_file <- jsonlite::read_json(j)$data_file |> unlist()
    if(length(data_file) != 1)
      return(NA_character_)
    as.character(fs::path_join(c(fs::path_dir(j), data_file)))
  })
  data_keep <- data_keep[!is.na(data_keep)]
  list(json = json_keep, data = data_keep)
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

  caches_reps <- fs::dir_ls(path = root,
                            regex = "\\.sourcoise",
                            type = "directory",
                            all = TRUE, recurse = TRUE)

  purrr::walk(caches_reps, ~if(fs::dir_exists(.x)) fs::dir_delete(.x))

}
