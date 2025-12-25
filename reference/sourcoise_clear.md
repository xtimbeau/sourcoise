# Cleans sourcoise cache

removes every json and qs2 files found by
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)
unless a specific tibble (filtered from
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md))
is passed as an argument.

## Usage

``` r
sourcoise_clear(what2keep = "all", root = NULL)
```

## Arguments

- what2keep:

  (–) a string (such as "last", the default or "nothing" clears all or
  "all" removes only non sourcoise files) or a tibble such as the one
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
[`sourcoise_clear_all()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear_all.md),
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md),
[`sourcoise_reset()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_reset.md),
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
fs::file_copy(
    fs::path_package("sourcoise", "some_data.R"),
    dir,
    overwrite = TRUE)
# Force execution
data <- sourcoise("some_data.R", force_exec = TRUE)
# we then clear all caches
sourcoise_clear()
#> character(0)
sourcoise_status()
#> # A tibble: 1 × 7
#>   valid priority src         last_call last_update   file_size json_file        
#>   <lgl>    <int> <chr>       <chr>     <chr>       <fs::bytes> <chr>            
#> 1 FALSE       10 some_data.R 0s        2s                  242 .sourcoise/some_…
```
