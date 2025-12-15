# Cache status of sourcoise

Given the current project, `soucoise_status()` collects all information
about cache (could be project level, file level) and return a tibble
with this data.

## Usage

``` r
sourcoise_status(short = TRUE, quiet = TRUE, root = NULL, prune = TRUE)
```

## Arguments

- short:

  (boolean) (deafault `TRUE`) return a simplified tibble

- quiet:

  (boolean) (default `TRUE`) no messages during execution

- root:

  (string) (default `NULL`) force root to a defined path, advanced and
  not recommanded use

- prune:

  (boolean) (default `TRUE`) clean up status to display only on relevant
  cache. However, does not clean other cache files.

## Value

tibble of cached files (see details for structure)

## Details

`sourcoise_status()` reflects what is on the disk (and results indeed
from a scan of all cached files and their metadatas). So modifying the
result of `sourcoise_status()` can produce complex bugs when it is
passed to
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md)
or `sourcoise_clean()`.

Data returned is:

- `src`: path to the source file (r script)

- `date`: last execution date

- `valid`: is cache valid ?

- `uid`: id of user

- `index`: index of cache

- `timing`: last execution timing

- `size`: size of the R object(s) returned

- `lapse`: periodic refresh trigger

- `wd`: wd setting for execution of r script

- `args`: arguments passed to R script

- `json_file`: path to the file keeping cache information

- `qmd_file`: list of path to qmd files calling this script (relevant
  only for quarto projects)

- `src_in`: localisation of cache option

- `data_file`: path to data cached

- `data_date`: date and time of last save of data

- `log_file`: path to log file, if logging activated

- `root`: path to the project root, used as reference for all paths

- `scr_hash`: hash of the source file

- `track_hash`: hash of the tracked files, if any

- `track`: list of files tracked

- `args_hash`: hash of arguments

- `data_hash`: hash of data cached

## See also

Other sourcoise:
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md),
[`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md),
[`sourcoise_clear_all()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear_all.md),
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md),
[`sourcoise_reset()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_reset.md)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
#> /tmp/RtmprFG60F
fs::file_copy(
    fs::path_package("sourcoise", "some_data.R"),
    dir,
    overwrite = TRUE)
# Force execution
data <- sourcoise("some_data.R", force_exec = TRUE)
#> Called from: hash_context(ctxt)
#> debug: ctxt$src_hash <- hash_file(ctxt$src)
#> debug: ctxt$arg_hash <- ctxt$argid
#> debug: mm <- get_mdatas(ctxt$basename, ctxt$full_cache_rep)
#> debug: ctxt$meta1 <- mm$meta1
#> debug: ctxt$metas <- mm$metas
#> debug: ctxt$track <- unique(unlist(c(ctxt$track, ctxt$meta1$track)))
#> debug: ctxt$track_hash <- 0
#> debug: if (length(ctxt$track) > 0) {
#>     track_files <- purrr::map(ctxt$track, ~fs::path_join(c(ctxt$root, 
#>         .x)))
#>     ok_files <- purrr::map_lgl(track_files, fs::file_exists)
#>     tracked <- track_files[ok_files]
#>     if (any(ok_files)) 
#>         ctxt$track_hash <- digest::digest(hash_file(as.character(ctxt$track)), 
#>             algo = "sha1")
#>     else {
#>         logger::log_info("Tracked files not found ({track_files[[!ok_files]]}), check your paths.")
#>     }
#> }
#> debug: track_files <- purrr::map(ctxt$track, ~fs::path_join(c(ctxt$root, 
#>     .x)))
#> debug: ok_files <- purrr::map_lgl(track_files, fs::file_exists)
#> debug: tracked <- track_files[ok_files]
#> debug: if (any(ok_files)) ctxt$track_hash <- digest::digest(hash_file(as.character(ctxt$track)), 
#>     algo = "sha1") else {
#>     logger::log_info("Tracked files not found ({track_files[[!ok_files]]}), check your paths.")
#> }
#> debug: logger::log_info("Tracked files not found ({track_files[[!ok_files]]}), check your paths.")
#> debug: ctxt$qmd_file <- c(ctxt$meta1$qmd_file, ctxt$qmd_file)
#> debug: return(ctxt)
# status returns the cache status
sourcoise_status()
#> # A tibble: 1 × 6
#>   src       priority date                data_date           file_size json_file
#>   <chr>        <int> <dttm>              <dttm>              <chr>     <chr>    
#> 1 some_dat…       10 2025-12-15 16:52:13 2025-12-15 16:52:13 242 B     .sourcoi…
```
