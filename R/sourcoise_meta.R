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
  ctxt <- setup_context(
    path = path,
    root = getOption("sourcoise.root"),
    src_in = getOption("sourcoise.src_in"),
    exec_wd = NULL,
    wd = getOption("sourcoise.wd"),
    track = NULL,
    args = args,
    lapse = "never",
    nocache = FALSE,
    grow_cache = getOption("sourcoise.grow_cache"),
    limit_mb = getOption("sourcoise.limit_mb"),
    log = "OFF",
    inform = FALSE,
    quiet = TRUE,
    metadata = TRUE)

  ctxt <- valid_metas(ctxt)

  good_datas <- ctxt$meta_datas |> purrr::keep(~.x$valid)
  if(length(good_datas)==0)
    if(length(ctxt$meta_datas)>=1) {
      nogood_data <- ctxt$meta_datas[which.max(purrr::map_chr(ctxt$meta_datas, "data_date") |> as.Date())]
      nogood_data$ok <- "invalid cache"
      return(nogood_data[c("ok", "timing", "date", "size", "args",
                               "lapse", "track", "qmd_file", "log_file", "file_size",
                               "data_date", "data_file", "json_file")])
    } else
    return(list(ok="no cache data"))

  good_datas[[1]]$ok <- "cache ok&valid"

  return(good_datas[[1]][c("ok", "timing", "date", "size", "args",
                           "lapse", "track", "qmd_file", "log_file", "file_size",
                           "data_date", "data_file", "json_file")])
}
