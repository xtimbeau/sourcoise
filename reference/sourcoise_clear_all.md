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
fs::file_copy(
    fs::path_package("sourcoise", "some_data.R"),
    dir,
    overwrite = TRUE)
# Force execution
data <- sourcoise("some_data.R", force_exec = TRUE)
# we then clear all caches
sourcoise_clear_all()
#> [1] "/tmp/RtmpSRPYXf/.sourcoise/some_data-4262323b_8fc9cea2-1.json"
#> [2] "/tmp/RtmpSRPYXf/.sourcoise/some_data-4262323b_8fc9cea2-2.json"
#> [3] "/tmp/RtmpSRPYXf/.sourcoise/some_data-4262323b_8fc9cea2-3.json"
#> [4] "/tmp/RtmpSRPYXf/.sourcoise/some_data-4262323b_8fc9cea2-4.json"
#> [5] "/tmp/RtmpSRPYXf/.sourcoise/some_data-4262323b_8fc9cea2-5.json"
sourcoise_status()
#> # A tibble: 0 × 0
```
