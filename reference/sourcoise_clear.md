# Cleans sourcoise cache

removes every json and qs2 files found by
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)
unless a specific tibble (filtered from
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md))
is passed as an argument.

## Usage

``` r
sourcoise_clear(
  what = sourcoise_status(short = FALSE, root = root, prune = FALSE),
  root = NULL
)
```

## Arguments

- what:

  (–) a tibble such as the one obtained by
  [`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md),
  possibly filtered

- root:

  to force root, not recommended (expert use)

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
#> [1] "/tmp/RtmpN3PvGM"
fs::file_copy(
    fs::path_package("sourcoise", "some_data.R"),
    dir,
    overwrite = TRUE)
# Force execution
data <- sourcoise("some_data.R", force_exec = TRUE)
# we then clear all caches
sourcoise_clear()
#> [1] "some_data.r" "some_data.r" "some_data.r" "some_data.r"
sourcoise_status()
#> # A tibble: 0 × 0
```
