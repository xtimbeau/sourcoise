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
#> /tmp/Rtmp4vaRSI
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
  dir,
  overwrite = TRUE)
# Force execution (root is set explicitly here, it is normally deduced from project)
data <- sourcoise("some_data.R", force_exec = TRUE)
# Then we access metadata
sourcoise_meta("some_data.R")
#> $ok
#> [1] "cache ok&valid"
#> 
#> $timing
#> [1] 7e-04
#> 
#> $date
#> [1] "2025-12-09 11:06:43"
#> 
#> $size
#> [1] 1720
#> 
#> $args
#> list()
#> 
#> $lapse
#> [1] "never"
#> 
#> $track
#> list()
#> 
#> $qmd_file
#> named list()
#> 
#> $log_file
#> named list()
#> 
#> $file_size
#> [1] 242
#> 
#> $data_date
#> [1] "2025-12-09 11:06:43.872034"
#> 
#> $data_file
#> [1] "some_data-4262323b_f92a79811b1d8866b336be3b35cd7f50.qs2"
#> 
#> $json_file
#> [1] "/tmp/Rtmp4vaRSI/.sourcoise/some_data-4262323b_a29eec33-1.json"
#> 
```
