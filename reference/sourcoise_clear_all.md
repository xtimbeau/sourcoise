# Cleans sourcoise cache

removes every json and qs2 files found by
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md).

## Usage

``` r
sourcoise_clear_all(root = NULL)
```

## Arguments

- root:

  to force root, not recommended (expert use only)

## Value

list of cleared files, plus a side-effect as specified cache files are
deleted (no undo possible)

## See also

Other sourcoise:
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md),
[`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md),
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
# Force execution
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
# we then clear all caches
sourcoise_clear_all()
#> character(0)
sourcoise_status()
#> ℹ No cache data
#> # A tibble: 0 × 0
```
