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
#> [1] "/tmp/Rtmp8laUpV"
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
  dir,
  overwrite = TRUE)
# Force execution (root is set explicitly here, it is normally deduced from project)
data <- sourcoise("some_data.R", force_exec = TRUE)

# Access metadata without loading the cached data
meta <- sourcoise_meta("some_data.R")
print(meta$timing)  # View execution time
#> [1] 7e-04
print(meta$ok)      # Check cache status
#> [1] "cache ok&valid"
```
