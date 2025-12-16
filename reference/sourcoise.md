# sources R script and caches results on disk

`sourcoise()` is used as a drop in replacement for
[`base::source()`](https://rdrr.io/r/base/source.html) but caches
results on disk. Cache is persistant over sessions.

## Usage

``` r
sourcoise(
  path,
  args = list(),
  track = list(),
  lapse = getOption("sourcoise.lapse"),
  force_exec = getOption("sourcoise.force_exec"),
  prevent_exec = getOption("sourcoise.prevent_exec"),
  metadata = getOption("sourcoise.metadata"),
  wd = getOption("sourcoise.wd"),
  quiet = getOption("sourcoise.quiet"),
  inform = FALSE
)
```

## Arguments

- path:

  (character) path of the script to execute (see details).

- args:

  (list) list of args that can be used in the script (in the form
  `args$xxx`).

- track:

  (list) list of files which modification triggers cache invalidation
  and script execution .

- lapse:

  (character) duration over which cache is invalidated. Could be `never`
  (default) `x hours`, `x days`, `x week`, `x months`, `x quarters`,
  `x years`.

- force_exec:

  (boolean) execute code, disregarding cache valid or invalid.

- prevent_exec:

  (boolean) prevent execution, cache valid or not, returned previous
  cached data, possibly invalid.

- metadata:

  (boolean) if TRUE `sourcoise()` returns a list with data is the
  `$data` and various meta data (see details).

- wd:

  (character) if `project` working directory for the execution of script
  will be the root of the project. If `file` then it will be the dir of
  the script (default) If `qmd`, then working dir will be the dir in
  which the calling `qmd` is. Current directory is restored after
  execution (successful or failed).

- quiet:

  (boolean) mute messages and warnings from script execution.

- inform:

  (boolean) Display logs on console, even if logging is disabled with
  threshold level "INFO".

## Value

data (list ou ce que le code retourne)

## Details

`sourcoise()` looks like
[`base::source()`](https://rdrr.io/r/base/source.html). However, there
are some minor differences.

First, the script called in `sourcoise()` must end by a
[`return()`](https://rdrr.io/r/base/function.html) or by an object
returned. Assignment made in the script won't be kept as `sourcoise()`
is executed locally. Only explicitly reruned object will be returned. So
`soucoise()` is used by assigning its result to something
(`aa <- sourcoise("mon_script.r)` or `sourcoise() |> ggplot() ...`).
Unless specified otherwise with `wd` parameter, the working directory
for the script execution is (temporarly) set to the dir in which is the
script. That allows for simple access to companion files and permit to
move the script and companion files to another dir or project.

Second, an heuristic is applied to find the script, in the event the
path given is incomplete. Whereas it is not advised and comes with a
performance cost, this can be useful when there is a change in the
structure of the project. The heuristic is simple, the script is
searched inside the porject dir and among all hits the closest to the
caller is returned.

Third, if an error is triggered by the script, `sourcoise()` does not
fail and return the error and a NULL return. However, if there is a
(invalid or valid) cache, the cached data is returned allowing for the
script to continue. In that case the error is logged.

Cache is invalidated when : 1 - a cache is not found 2 - the script has
been modified 3 - tracked files have been modified 4 - last execution
occurred a certain time ago and is considered as expired 5 - execution
is forced

Whatever values takes `src_in`, if the file path starts with a `/`, then
the source file will be interpreted from project root (if any). This is
coherent whith naming convention in `quarto`. Otherwise, the document
path wil be used firstly (if any, that is to say executed from quarto,
rendering). Finally, working directory will be used. If everything
fails, it will try to search in the project directory a corresponding
file and will keep the closest from the calling point.

Usually the fisrt call return and cache the results. Results can be aby
R object and are serialized and saved using `qs2`. Subsequent calls,
supposing none of cache invalidation are true, are then very quick. No
logging is used, data is fecteched from the cache and that's it. For
standard size data, used in a table or a graph (\< 1Mb roughly), return
timing is under 5ms.

`lapse` parameter is used for invalidation trigger 4. `lapse = "1 day"`
ou `lapse="day"` for instance will trigger once a day the execution.
`lapse = "3 days"` will do it every 72h. `hours`, `weeks`, `months`,
`quarters` or `years` are understood time units. MOre complex calendar
instructions could be added, but `sourcoise_refesh()` provides a
solution more general and easy to adapt to any use case, as to my
knowledge, there is no general mechanism to be warned of data updates.

`track` is the trigger \#3. It is simply a list of files (following path
convention defined by `scr_in`, so either script dir of project dir as
reference). If the files in the list are changed then the execution is
triggered. It is done with a hash and it is difficult to have a croo
plateform hash for excel files. Nevertheless, hash is done on text files
with same results of different platforms.

## Global options

In order to simplify usage and to avoid complex bugs, some parameters
can be set only globally, through options().

- `sourcoise.root` (character) force root, and bypass soucroise
  mechanism to find root. Useful when you want to execute sourcoise in a
  non-project context (see examples). `sourcoise.src_in` (character) if
  `project` stores the cache folder (`.sourcoise`) at the project root,
  if `file`, `.sourcoise` is stored at the calling point.

- `sourcoise.nocache` (boolean) no caching, so makes sourcoise less
  useful, can be used for testing purpose

- `sourcoise.log` (default "OFF") log threshold (see
  `logger::log_treshold()`).

- `sourcoise.grow_cache` (integer) (default 5 par défaut) cache limit in
  number of data file kept.

- `sourcoise.limit_mb` (integer) (default 50) individual cache data
  files size on disk limit. If above **no caching** occurs.

## Metadata

If `metadata=TRUE`, a list is returned, with some metadatas. Main ones
are `$data`, the data returned, `$date`, execution date, `$timing`
execution timing, `$size` of the R object in memory, `$data_file`,
`$data_date` and `$file_size` documenting data file path, date size on
disk and last modification date, parameters of the call (`$track`,
`$wd`, `$src_in`, `$args` and so on).

`force_exec` and `prevent_exec` are parameters that force the script
execution (trigger \#5) of prevent it (so cache is returned or NULL if
no cache). Those 2 parameters can be set for one specific execution, but
they are intendend to a global setting through the option
`sourcoise.force_exec` or `sourcoise.prevent_exec`.

If returned data after execution is not different than previously cached
data, then no caching occurs in order to limit the disk use and to avoid
keeping an history of the same data files. This implies the possibility
of a difference between last execution date and last data modification
date. If you are insterested in the moment data was changed, then
`$data_date` is to be preferred.

## Working with github

`sourcoise()` is designed to function with *github*. Cache information
is specific to each user (avoiding conflicts) and cached data is named
with the hash. Conflicts could occur in the rare case the same script is
executed on different machines and that this script return each time a
different result (such as a random generator).

## See also

Other sourcoise:
[`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md),
[`sourcoise_clear_all()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear_all.md),
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md),
[`sourcoise_reset()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_reset.md),
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)

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
#> debug: fs::dir_create(ctxt$full_cache_rep, recurse = TRUE)
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
# The second time cache is used
data <- sourcoise("some_data.R")
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
# Performance and mem test
dir <- tempdir()
set_sourcoise_root(dir)
#> /tmp/RtmpOyU1Hu
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
   dir,
   overwrite = TRUE)
bench::mark(
 forced = data <- sourcoise("some_data.R", force_exec = TRUE),
 cached = data <- sourcoise("some_data.R"),
 max_iterations = 1)
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
```
