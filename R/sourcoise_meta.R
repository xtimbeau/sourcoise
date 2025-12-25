#' Get Sourcoise Metadata for a Script
#'
#' Retrieves metadata about a cached script without fetching the actual data. This function
#' provides quick access to information about script execution, cache status, and related files.
#'
#' @param path Path to the script file (character). Can be an absolute or relative path.
#' @param args Named list of arguments that were passed to the script, if any. Default is `NULL`.
#'   This is used to identify the specific cached version when the script was executed with
#'   different argument sets.
#' @param root (defaut `NULL`) the root of the project (you'd better rely on sourcoise for that one)
#' @param quiet (defaut `FALSE`) should we say something ?
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
sourcoise_meta <- function(path, args = NULL, root = NULL, quiet = FALSE) {

  ctxt <- setup_context(
    path = path,
    root = root,
    src_in = getOption("sourcoise.src_in"),
    exec_wd = NULL,
    wd = getOption("sourcoise.wd"),
    track = list(),
    args = args,
    lapse = NULL,
    nocache = FALSE,
    grow_cache = getOption("sourcoise.grow_cache"),
    limit_mb = getOption("sourcoise.limit_mb"),
    log = "OFF",
    inform = FALSE,
    quiet = TRUE,
    metadata = TRUE)

  meta <- list()

  if(is.null(ctxt)) {
    meta$ok <- "file {path} not found" |> glue::glue() }

  if(!is.null(ctxt)) {
    ctxt <- valid_meta1(ctxt)
    meta <- ctxt$meta1
    meta <- purrr::list_modify(meta, !!!ctxt$meta_valid)

    if(meta$valid)
      meta$ok <- "cache ok&valid"
    if(!meta$data_exists)
      meta$ok <- "no cache"
    if(!meta$valid_src&meta$data_exists)
      meta$ok <- "cache invalid -- source file changed"
    if(!meta$valid_lapse&meta$data_exists)
      meta$ok <- "cache expired"
    if(!meta$valid_track&meta$data_exists)
      meta$ok <- "cache invalid -- tracked filed changed"

    meta$date <- lubridate::as_datetime(meta$date, tz=Sys.timezone())
    meta$data_date <- lubridate::as_datetime(meta$data_date, tz=Sys.timezone())
  }
  if(!quiet)
    cli::cli_alert_info(meta$ok)

  return(meta)
}
