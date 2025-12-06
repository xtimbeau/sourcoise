# Cleans sourcoise cache

removes every json and qs2 files found by
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)
unless a specific tibble (filtered from
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md))
is passed as an argument.

## Usage

``` r
sourcoise_clear(what2keep = "last", root = NULL)
```

## Arguments

- what2keep:

  (–) a string (such as "last", the default) or a tibble such as the one
  obtained by
  [`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md),
  possibly filtered for the files you whish to keep

- root:

  to force root, not recommended (expert use only)

## Value

list of cleared files, plus a side-effect as specified cache files are
deleted (no undo possible)

## See also

Other sourcoise:
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md),
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md),
[`sourcoise_reset()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_reset.md),
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
#> [1] "/tmp/Rtmprb6yE0"
fs::file_copy(
    fs::path_package("sourcoise", "some_data.R"),
    dir,
    overwrite = TRUE)
# Force execution
data <- sourcoise("some_data.R", force_exec = TRUE)
# we then clear all caches
sourcoise_clear()
#> Error in dplyr::mutate(ww, bn = stringr::str_extract(.data$json_file,     pattern = pat, group = 1), argsid = stringr::str_extract(.data$json_file,     pat, group = 2), uid = stringr::str_extract(.data$json_file,     pat, group = 3), cc = as.numeric(stringr::str_extract(.data$json_file,     pat, group = 4)), date = lubridate::as_datetime(purrr::map_chr(.data$json_file,     ~purrr::pluck(read_mdata(.x), "date")))): ℹ In argument: `date = lubridate::as_datetime(...)`.
#> Caused by error:
#> ! error in evaluating the argument 'x' in selecting a method for function 'as_datetime': ℹ In index: 1.
#> Caused by error:
#> ! [ENOENT] Failed to remove '.sourcoise/some_data-4262323b_b97cd655-4.json': no such file or directory
sourcoise_status()
#> # A tibble: 1 × 6
#>   src      priority date                data_date           file_size json_file 
#>   <chr>       <int> <dttm>              <dttm>              <chr>     <fs::path>
#> 1 some_da…       10 2025-12-06 17:59:56 2025-12-06 17:59:54 242 B     …55-4.json
```
