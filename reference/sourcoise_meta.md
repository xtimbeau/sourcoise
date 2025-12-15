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
#> /tmp/RtmprFG60F
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
  dir,
  overwrite = TRUE)
# Force execution (root is set explicitly here, it is normally deduced from project)
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

# Access metadata without loading the cached data
meta <- sourcoise_meta("some_data.R")
print(meta$timing)  # View execution time
#> [1] 7e-04
print(meta$ok)      # Check cache status
#> [1] "cache ok&valid"
```
