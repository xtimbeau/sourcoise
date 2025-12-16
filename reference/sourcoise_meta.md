# Get Sourcoise Metadata for a Script

Retrieves metadata about a cached script without fetching the actual
data. This function provides quick access to information about script
execution, cache status, and related files.

## Usage

``` r
sourcoise_meta(path, args = NULL)
```

## Arguments

- path:

  Path to the script file (character). Can be an absolute or relative
  path.

- args:

  Named list of arguments that were passed to the script, if any.
  Default is `NULL`. This is used to identify the specific cached
  version when the script was executed with different argument sets.

## Value

A named list containing cache metadata with the following elements:

- ok:

  Cache status indicator: "cache ok&valid", "invalid cache", or "no
  cache data"

- timing:

  Execution time of the full script (duration)

- date:

  Date and time of the last full execution

- size:

  Size of objects returned, measured in R memory

- args:

  Arguments given to sourcoise for the script

- lapse:

  Delay interval before reexecution is triggered

- track:

  List of files being tracked for changes

- qmd_file:

  List of Quarto (.qmd) files calling this script

- log_file:

  Path to the last log file

- file_size:

  Size of cached data on disk

- data_date:

  Date of last data save (note: if no new data is generated during
  execution, no new data file is saved)

- data_file:

  Path to the cached data file (stored as .qs2 format)

- json_file:

  Path to the JSON file storing metadata (located in .sourcoise
  directory)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
#> /tmp/RtmpOyU1Hu
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
  dir,
  overwrite = TRUE)
# Force execution (root is set explicitly here, it is normally deduced from project)
data <- sourcoise("some_data.R", force_exec = TRUE)
#> Called from: cache_data(our_data, ctxt)
#> debug: exist <- FALSE
#> debug: if (nrow(all_metas) > 0) {
#>     meta <- slice(dplyr::filter(all_metas, .data$data_hash == 
#>         new_data_hash), 1)
#>     if (nrow(meta) == 1) {
#>         exist <- TRUE
#>         exists_data_file <- fs::path_file(dplyr::pull(meta, .data$data_file))
#>         exists_data_file <- fs::path_join(c(ctxt$full_cache_rep, 
#>             exists_data_file))
#>         exists <- fs::file_exists(exists_data_file)
#>         finfo <- fs::file_info(exists_data_file)
#>         exists_file_size <- finfo$size
#>         exists_data_date <- as.character(dplyr::pull(meta, .data$data_date))
#>     }
#> }
#> debug: if (!fs::dir_exists(ctxt$full_cache_rep)) fs::dir_create(ctxt$full_cache_rep, 
#>     recurse = TRUE)
#> debug: data$data_hash <- new_data_hash
#> debug: if (!ctxt$nocache) {
#>     les_metas <- data
#>     les_metas$data <- NULL
#>     les_metas$file <- NULL
#>     les_metas$ok <- NULL
#>     les_metas$priority <- ctxt$priority
#>     if (!exist) {
#>         fnd <- fs::path_join(c(ctxt$full_cache_rep, stringr::str_c(ctxt$basename, 
#>             "_", stringr::str_c(data$data_hash, ".qs2"))))
#>         qs2::qs_save(data$data, file = fnd, nthreads = getOption("sourcoise.nthreads"))
#>         f_i <- fs::file_info(fnd)
#>         les_metas$file_size <- f_i$size
#>         les_metas$data_date <- as.character(f_i$modification_time)
#>         if (f_i$size > ctxt$limit_mb * 1024 * 1024) {
#>             fs::file_delete(fnd)
#>             logger::log_warn("cached data not saved because ({scales::label_bytes()(file_size)} is over the {ctxt$limit_md} Mb limit.")
#>         }
#>     }
#>     else {
#>         fnd <- exists_data_file
#>         les_metas$file_size <- exists_file_size
#>         les_metas$data_date <- exists_data_date
#>     }
#>     les_metas$data_file <- data$data_file <- fs::path_file(fnd)
#>     if (!is.null(ctxt$log_file)) 
#>         les_metas$log_file <- fs::path_rel(ctxt$log_file, ctxt$root)
#>     data$data_date <- les_metas$data_date
#>     write_meta(les_metas, ctxt)
#>     prune_cache(ctxt)
#> }
#> debug: les_metas <- data
#> debug: les_metas$data <- NULL
#> debug: les_metas$file <- NULL
#> debug: les_metas$ok <- NULL
#> debug: les_metas$priority <- ctxt$priority
#> debug: if (!exist) {
#>     fnd <- fs::path_join(c(ctxt$full_cache_rep, stringr::str_c(ctxt$basename, 
#>         "_", stringr::str_c(data$data_hash, ".qs2"))))
#>     qs2::qs_save(data$data, file = fnd, nthreads = getOption("sourcoise.nthreads"))
#>     f_i <- fs::file_info(fnd)
#>     les_metas$file_size <- f_i$size
#>     les_metas$data_date <- as.character(f_i$modification_time)
#>     if (f_i$size > ctxt$limit_mb * 1024 * 1024) {
#>         fs::file_delete(fnd)
#>         logger::log_warn("cached data not saved because ({scales::label_bytes()(file_size)} is over the {ctxt$limit_md} Mb limit.")
#>     }
#> } else {
#>     fnd <- exists_data_file
#>     les_metas$file_size <- exists_file_size
#>     les_metas$data_date <- exists_data_date
#> }
#> debug: fnd <- fs::path_join(c(ctxt$full_cache_rep, stringr::str_c(ctxt$basename, 
#>     "_", stringr::str_c(data$data_hash, ".qs2"))))
#> debug: qs2::qs_save(data$data, file = fnd, nthreads = getOption("sourcoise.nthreads"))
#> debug: f_i <- fs::file_info(fnd)
#> debug: les_metas$file_size <- f_i$size
#> debug: les_metas$data_date <- as.character(f_i$modification_time)
#> debug: if (f_i$size > ctxt$limit_mb * 1024 * 1024) {
#>     fs::file_delete(fnd)
#>     logger::log_warn("cached data not saved because ({scales::label_bytes()(file_size)} is over the {ctxt$limit_md} Mb limit.")
#> }
#> debug: les_metas$data_file <- data$data_file <- fs::path_file(fnd)
#> debug: if (!is.null(ctxt$log_file)) les_metas$log_file <- fs::path_rel(ctxt$log_file, 
#>     ctxt$root)
#> debug: data$data_date <- les_metas$data_date
#> debug: write_meta(les_metas, ctxt)
#> Error in UseMethod("filter"): no applicable method for 'filter' applied to an object of class "list"

# Access metadata without loading the cached data
meta <- sourcoise_meta("some_data.R")
print(meta$timing)  # View execution time
#> NULL
print(meta$ok)      # Check cache status
#> [1] "file not found"
```
