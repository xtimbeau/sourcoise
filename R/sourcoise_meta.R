#' Get Sourcoise Metadata for a Script
#'
#' Retrieves metadata about a cached script without fetching the actual data. This function
#' provides quick access to information about script execution, cache status, and related files.
#'
#' @param path Path to the script file (character). Can be an absolute or relative path.
#' @param args Named list of arguments that were passed to the script, if any. Default is `NULL`.
#'   This is used to identify the specific cached version when the script was executed with
#'   different argument sets.
#'
#' @returns A named list containing cache metadata with the following elements:
#' \describe{
#'   \item{ok}{Cache status indicator: "cache ok&valid", "invalid cache", or "no cache data"}
#'   \item{timing}{Execution time of the full script (duration)}
#'   \item{date}{Date and time of the last full execution}
#'   \item{size}{Size of objects returned, measured in R memory}
#'   \item{args}{Arguments given to sourcoise for the script}
#'   \item{lapse}{Delay interval before reexecution is triggered}
#'   \item{track}{List of files being tracked for changes}
#'   \item{qmd_file}{List of Quarto (.qmd) files calling this script}
#'   \item{log_file}{Path to the last log file}
#'   \item{file_size}{Size of cached data on disk}
#'   \item{data_date}{Date of last data save (note: if no new data is generated during
#'     execution, no new data file is saved)}
#'   \item{data_file}{Path to the cached data file (stored as .qs2 format)}
#'   \item{json_file}{Path to the JSON file storing metadata (located in .sourcoise directory)}
#' }
#'
#' @export
#'
#' @examples
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#' fs::file_copy(
#'    fs::path_package("sourcoise", "some_data.R"),
#'   dir,
#'   overwrite = TRUE)
#' # Force execution (root is set explicitly here, it is normally deduced from project)
#' data <- sourcoise("some_data.R", force_exec = TRUE)
#'
#' # Access metadata without loading the cached data
#' meta <- sourcoise_meta("some_data.R")
#' print(meta$timing)  # View execution time
#' print(meta$ok)      # Check cache status
#'
sourcoise_meta <- function(path, args=NULL) {

  if(is.null(args))
    args <- list()
  argid <- digest::digest(args, algo = "crc32")
  metas <- fast_metadata(bn = path |>
                           fs::path_file() |>
                           fs::path_ext_remove(),
                         argid = argid) |>
    tibble::as_tibble()
  if(nrow(metas)==0)
    return(list(ok = "file not found"))

  metas <- metas |>
    dplyr::group_by(uid, argid, basename) |>
    dplyr::filter(index == max(index)) |>
    dplyr::ungroup()

  mm <- fast_read_mdata(metas)
  if(nrow(mm)==0)
    return(list(ok = "metadata not found"))

  mm <- mm |>
    dplyr::filter(date == max(date)) |>
    dplyr::slice(1) |>
    as.list()
  if(is.null(mm$track))
    mm$track <- list()
  if(is.null(mm$args))
    mm$args <- list()
  mm$qmd_file <- mm$qmd_file |> purrr::list_c()
  root <- unique(metas$root)
  src <- fs::path_join(c(root, mm$src))
  src_hash <- hash_file(src)
  if(length(unlist(mm$track))>0)
    track_hash <- hash_tracks(mm$track, root) else
      track_hash <- 0
  data_exists <- file.exists(fs::path_join(c(mm$downcache_rep, mm$data_file)))
  valid <- src_hash == mm$src_hash &
    track_hash == mm$track_hash &
    data_exists
  if(valid)
    mm$ok <- "cache ok&valid" else
      mm$ok <- "invalid cache"
  return(mm)
}
