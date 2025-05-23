#' sources R script and caches results on disk
#'
#' `sourcoise()` is used as a drop in replacement for `base::source()` but caches results on disk. Cache is persistant over sessions.
#'
#'  `sourcoise()` looks like `base::source()`. However, there are some minor differences.
#'
#'  First, the script called in `sourcoise()` must end by a `return()` or by an object returned. Assignment made in the script won't be kept as `sourcoise()` is executed locally. Only explicitly reruned object will be returned. So `soucoise()` is used by assigning its result to something (`aa <- sourcoise("mon_script.r)` or `sourcoise() |> ggplot() ...`). Unless specified otherwise with `wd` parameter, the working directory for the script execution is (temporarly) set to the dir in which is the script. That allows for simple access to companion files and permit to move the script and companion files to another dir or project.
#'
#'  Second, an heuristic is applied to find the script, in the event the path given is incomplete. Whereas it is not advised and comes with a performance cost, this can be useful when there is a change in the structure of the project. The heuristic is simple, the script is searched inside the porject dir and among all hits the closest to the caller is returned.
#'
#'  Third, if an error is triggered by the script, `sourcoise()` does not fail and return the error and a NULL return. However, if there is a (invalid or valid) cache, the cached data is returned allowing for the script to continue. In that case the error is logged.
#'
#'  Cache is invalidated when :
#'  1 -   a cache is not found
#'  2 -   the script has been modified
#'  3 -   tracked files have been modified
#'  4 -   last execution occurred a certain time ago and is considered as expired
#'  5 -   execution is forced
#'
#' Whatever values takes `src_in`, if the file path starts with a `/`, then the source file will be interpreted from project root (if any). This is coherent whith naming convention in `quarto`. Otherwise, the document path wil be used firstly (if any, that is to say executed from quarto, rendering). Finally, working directory will be used. If everything fails, it will try to search in the project directory a corresponding file and will keep the closest from the calling point.
#'
#' Usually the fisrt call return and cache the results. Results can be aby R object and are serialized and saved using `qs2`. Subsequent calls, supposing none of cache invalidation are true, are then very quick. No logging is used, data is fecteched from the cache and that's it. For standard size data, used in a table or a graph (< 1Mb roughly), return timing is under 5ms.
#'
#' `lapse` parameter is used for invalidation trigger 4. `lapse = "1 day"` ou `lapse="day"` for instance will trigger once a day the execution. `lapse = "3 days"` will do it every 72h. `hours`, `weeks`, `months`, `quarters` or `years` are understood time units. MOre complex calendar instructions could be added, but `sourcoise_refesh()` provides a solution more general and easy to adapt to any use case, as to my knowledge, there is no general mechanism to be warned of data updates.
#'
#' `track` is the trigger #3. It is simply a list of files (following path convention defined by `scr_in`, so either script dir of project dir as reference). If the files in the list are changed then the execution is triggered. It is done with a hash and it is difficult to have a croo plateform hash for excel files. Nevertheless, hash is done on text files with same results of different platforms.
#'
#' @section Global options:
#'
#' In order to simplify usage and to avoid complex bugs, some parameters can be set only globally, through options().
#' - `sourcoise.root` (character) force root, and bypass soucroise mechanism to find root. Useful when you want to execute sourcoise in a non-project context (see examples).
#' `sourcoise.src_in` (character) if `project` stores the cache folder (`.sourcoise`) at the project root, if `file`, `.sourcoise` is stored at the calling point.
#' - `sourcoise.nocache` (boolean) no caching, so makes sourcoise less useful, can be used for testing purpose
#' - `sourcoise.log` (default "OFF") log threshold (see `logger::log_treshold()`).
#' - `sourcoise.grow_cache` (integer) (default 5 par défaut) cache limit in number of data file kept.
#' - `sourcoise.limit_mb` (integer) (default 50) individual cache data files size on disk limit. If above **no caching** occurs.
#'
#' @section Metadata:
#'
#' If `metadata=TRUE`, a list is returned, with some metadatas. Main ones are `$data`, the data returned, `$date`, execution date, `$timing` execution timing, `$size` of the R object in memory, `$data_file`, `$data_date` and  `$file_size` documenting data file path, date size on disk and last modification date, parameters of the call (`$track`, `$wd`, `$src_in`, `$args` and so on).
#'
#' `force_exec` and `prevent_exec` are parameters that force the script execution (trigger #5) of prevent it (so cache is returned or NULL if no cache). Those 2 parameters can be set for one specific execution, but they are intendend to a global setting through the option `sourcoise.force_exec` or `sourcoise.prevent_exec`.
#'
#' If returned data after execution is not different than previously cached data, then no caching occurs in order to limit the disk use and to avoid keeping an history of the same data files. This implies the possibility of a difference between last execution date and last data modification date. If you are insterested in the moment data was changed, then `$data_date` is to be preferred.
#'
#' @section Working with github:
#'
#' `sourcoise()` is designed to function with *github*. Cache information is specific to each user (avoiding conflicts) and cached data is named with the hash. Conflicts could occur in the rare case the same script is executed on different machines and that this script return each time a different result (such as a random generator).
#'
#' @param path (character) path of the script to execute (see details).
#' @param args (list) list of args that can be used in the script (in the form `args$xxx`).
#' @param track (list) list of files which modification triggers cache invalidation and script execution .
#' @param lapse (character) duration over which cache is invalidated. Could be `never` (default) `x hours`, `x days`, `x week`, `x months`, `x quarters`, `x years`.
#' @param force_exec (boolean) execute code, disregarding cache valid or invalid.
#' @param prevent_exec (boolean) prevent execution, cache valid or not, returned previous cached data, possibly invalid.
#' @param metadata (boolean) if TRUE `sourcoise()` returns a list with data is the `$data`  and various meta data (see details).
#' @param wd (character) if `project` working directory for the execution of script will be the root of the project. If `file` then it will be the dir of the script (default) If `qmd`, then working dir will be the dir in which the calling `qmd` is. Current directory is restored after execution (successful or failed).
#' @param quiet (boolean) mute messages and warnings from script execution.
#' @param inform (boolean) Display logs on console, even if logging is disabled with threshold level "INFO".
#' @param priority (integer) (default 10) can be used as a way to control the order of execution when refreshing data (see `sourcoise_refresh()`)

#'
#' @family sourcoise
#' @return data (list ou ce que le code retourne)
#' @export
#' @examplesIf rlang::is_installed("insee")
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#' fs::file_copy(
#'    fs::path_package("sourcoise", "some_data.R"),
#'   dir,
#'   overwrite = TRUE)
#' # Force execution (root is set explicitly here, it is normally deduced from project)
#' data <- sourcoise("some_data.R", force_exec = TRUE)
#' # The second time cache is used
#' data <- sourcoise("some_data.R")
#' @examplesIf rlang::is_installed(c("bench"))
#' # Performance and mem test
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#' fs::file_copy(
#'    fs::path_package("sourcoise", "some_data.R"),
#'    dir,
#'    overwrite = TRUE)
#' bench::mark(
#'  forced = data <- sourcoise("some_data.R", force_exec = TRUE),
#'  cached = data <- sourcoise("some_data.R"),
#'  max_iterations = 1)
#'
## wrapper (limit the parameters)
sourcoise <- function(
    path,
    args = list(),
    track = list(),
    lapse = getOption("sourcoise.lapse"),
    force_exec = getOption("sourcoise.force_exec"),
    prevent_exec = getOption("sourcoise.prevent_exec"),
    metadata = getOption("sourcoise.metadata"),
    wd = getOption("sourcoise.wd"),
    quiet = TRUE,
    inform = FALSE,
    priority = 10) {

  sourcoise_(path = path,
             args = args,
             track = track,
             lapse = lapse,
             force_exec = force_exec,
             prevent_exec = prevent_exec,
             metadata = metadata,
             wd = wd,
             src_in = getOption("sourcoise.src_in"),
             exec_wd = NULL,
             root = getOption("sourcoise.root"),
             quiet = quiet,
             nocache = getOption("sourcoise.nocache"),
             inform = inform,
             log = getOption("sourcoise.log"),
             grow_cache = getOption("sourcoise.grow_cache"),
             limit_mb = getOption("sourcoise.limit_mb"),
             priority = priority)
}

## real function

sourcoise_ <- function(
    path,
    args = list(),
    track = list(),
    lapse = getOption("sourcoise.lapse"),
    force_exec = getOption("sourcoise.force_exec"),
    prevent_exec = getOption("sourcoise.prevent_exec"),
    metadata = getOption("sourcoise.metadata"),
    wd = getOption("sourcoise.wd"),
    src_in = getOption("sourcoise.src_in"),
    exec_wd = NULL,
    root = getOption("sourcoise.root"),
    quiet = TRUE,
    nocache = getOption("sourcoise.nocache"),
    inform = FALSE,
    log = getOption("sourcoise.log"),
    grow_cache = getOption("sourcoise.grow_cache"),
    limit_mb = getOption("sourcoise.limit_mb"),
    priority = 10) {

  ctxt <- setup_context(
    path = path,
    root = root,
    src_in = src_in,
    exec_wd = exec_wd,
    wd = wd,
    track = track,
    args = args,
    lapse = lapse,
    nocache = nocache,
    grow_cache = grow_cache,
    limit_mb = limit_mb,
    log = log,
    inform = inform,
    priority = priority,
    quiet = quiet,
    metadata = metadata)

  if(is.null(ctxt)) {
    logger::log_error("file {path} not found")
    return(list(error = glue::glue("file {path} not found"), ok = FALSE, log_file = ctxt$log_file))
  }

  if(is.null(force_exec)) force <- FALSE else if(force_exec=="TRUE") force <- TRUE else force <- FALSE
  if(is.null(prevent_exec))
    prevent <- FALSE
  else if(prevent_exec=="TRUE")
    prevent <- TRUE
  else
    prevent <- FALSE
  our_data <- list()
  if(force&!prevent) {
    our_data <- exec_source(ctxt)
    if(our_data$ok=="exec") {
      our_data <- cache_data(our_data, ctxt)
      logger::log_success(
        "{ctxt$relname} (forced) in {round(our_data$timing, 2)} sec. ({scales::label_bytes()(our_data$size)})")
      return(data_returned(our_data, ctxt))
    }
  }

  ctxt <- valid_metas(ctxt)

  good_datas <- ctxt$meta_datas |> purrr::keep(~.x$valid)

  if(length(good_datas)==0) {
    if(prevent) {
      logger::log_warn("No cached data, execution prevented")
      return(list(error = "No cache&prevent", ok = FALSE, log_file = ctxt$log_file))
    }
    if(!is.null(our_data))
      our_data <- exec_source(ctxt)
    if(our_data$ok=="exec") {
      our_data <- cache_data(our_data, ctxt)
      logger::log_success(
        "{ctxt$relname} (exec. no cache found) in {round(our_data$timing, 2)} sec. ({scales::label_bytes()(our_data$size)})")

      return(data_returned(our_data, ctxt))

    } else {
      if(length(ctxt$meta_datas)==0) {
        msg <- "{ctxt$relname} failed, no cache: \n{our_data$error$message}"
        if(ctxt$quiet)
          cli::cli_alert_danger(msg)
        logger::log_error(msg)
        return(our_data)
      }
      return_data <- pick_gooddata(ctxt$meta_datas, ctxt)
      msg <-
        "{ctxt$relname} failed, returning invalid cache ({scales::label_bytes()(return_data$size)})"
      if(ctxt$quiet)
        cli::cli_alert_danger(msg)
      logger::log_warn(msg)
    }
  } else {
    return_data <- pick_gooddata(good_datas, ctxt)
    logger::log_success("{ctxt$relname} valid cache ({scales::label_bytes()(return_data$size)})")
  }

  return(data_returned(return_data, ctxt))
}

