#' Returns sourcoise metadata on a script
#'
#' quick acces to metadata of the script, data is not fecthed.
#'
#' -    `timing`: time of full script execution
#' -    `date`: date of last full execution
#' -    `size`: size of objects returned (in R memory)
#' -    `args`: args given to sourcoise for the script
#' -    `lapse`: dely before reexecution
#' -    `track`: list of files tracked
#' -    `qmd_file`: list of qmd calling this script
#' -    `log_file`: last log file
#' -    `file_size`: size of data cached on disk
#' -    `data_date`: date of last data save (if no new data when executed, no data is saved)
#' -    `data_file`: path to data cached (as a qs2 data file)
#' -    `file`: path to the json file storing metadata (and .sourcoise dir)
#' @param path (character) path of the script
#' @param args (named list) arguments of the script if any
#'
#' @returns a named list with cache information
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
#' # Then we access metadata
#' sourcoise_meta("some_data.R")
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
    priority = 10,
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
