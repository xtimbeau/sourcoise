# Returns sourcoise metadata on a script

quick acces to metadata of the script, data is not fecthed.

## Usage

``` r
sourcoise_meta(path, args = NULL)
```

## Arguments

- path:

  (character) path of the script

- args:

  (named list) arguments of the script if any

## Value

a named list with cache information

## Details

- `timing`: time of full script execution

- `date`: date of last full execution

- `size`: size of objects returned (in R memory)

- `args`: args given to sourcoise for the script

- `lapse`: dely before reexecution

- `track`: list of files tracked

- `qmd_file`: list of qmd calling this script

- `log_file`: last log file

- `file_size`: size of data cached on disk

- `data_date`: date of last data save (if no new data when executed, no
  data is saved)

- `data_file`: path to data cached (as a qs2 data file)

- `file`: path to the json file storing metadata (and .sourcoise dir)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
#> [1] "/tmp/Rtmp6WFJAw"
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
  dir,
  overwrite = TRUE)
# Force execution (root is set explicitly here, it is normally deduced from project)
data <- sourcoise("some_data.R", force_exec = TRUE)
# Then we access metadata
sourcoise_meta("some_data.R")
#> Error in setup_context(path = path, root = getOption("sourcoise.root"),     src_in = getOption("sourcoise.src_in"), exec_wd = NULL, wd = getOption("sourcoise.wd"),     track = NULL, args = args, lapse = "never", nocache = FALSE,     grow_cache = getOption("sourcoise.grow_cache"), limit_mb = getOption("sourcoise.limit_mb"),     log = "OFF", inform = FALSE, priority = 10, quiet = TRUE,     metadata = TRUE): unused argument (priority = 10)
```
