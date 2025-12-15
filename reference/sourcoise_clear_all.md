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
#> /tmp/RtmpLY1YKh
fs::file_copy(
    fs::path_package("sourcoise", "some_data.R"),
    dir,
    overwrite = TRUE)
# Force execution
data <- sourcoise("some_data.R", force_exec = TRUE)
#> Error in purrr::map(cache_reps, ~fs::dir_ls(.x, regexp = jpat, recurse = TRUE)): ℹ In index: 1.
#> ℹ With name: /tmp/RtmpLY1YKh.
#> Caused by error:
#> ! [ENOENT] Failed to search directory '/tmp/RtmpLY1YKh/.sourcoise': no such file or directory
# we then clear all caches
sourcoise_clear_all()
#> character(0)
sourcoise_status()
#> ℹ No cache data
#> # A tibble: 0 × 0
```
