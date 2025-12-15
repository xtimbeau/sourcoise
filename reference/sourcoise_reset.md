# Resets sourcoise

Removes all `.sourcoise` folders found under the project root.

## Usage

``` r
sourcoise_reset(root = NULL)
```

## Arguments

- root:

  to force root (expert use)

## Value

No return, effect is through removal of .sourcoise folders (this is a
side effect, no undo possible)

## See also

Other sourcoise:
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md),
[`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md),
[`sourcoise_clear_all()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear_all.md),
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md),
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
#> /tmp/RtmprFG60F
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
   dir,
   overwrite = TRUE)
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
sourcoise_reset()
```
