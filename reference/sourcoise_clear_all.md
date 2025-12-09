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
#> /tmp/RtmpHGm86s
fs::file_copy(
    fs::path_package("sourcoise", "some_data.R"),
    dir,
    overwrite = TRUE)
# Force execution
data <- sourcoise("some_data.R", force_exec = TRUE)
# we then clear all caches
sourcoise_clear_all()
#> [1] "/tmp/RtmpHGm86s/.sourcoise/some_data-4262323b_932f07a1-1.json"                     
#> [2] "/tmp/RtmpHGm86s/.sourcoise/some_data-4262323b_932f07a1-2.json"                     
#> [3] "/tmp/RtmpHGm86s/.sourcoise/some_data-4262323b_932f07a1-3.json"                     
#> [4] "/tmp/RtmpHGm86s/.sourcoise/some_data-4262323b_932f07a1-4.json"                     
#> [5] "/tmp/RtmpHGm86s/.sourcoise/some_data-4262323b_932f07a1-5.json"                     
#> [6] "/tmp/RtmpHGm86s/.sourcoise/some_data-4262323b_f92a79811b1d8866b336be3b35cd7f50.qs2"
sourcoise_status()
#> ℹ No cache data
#> # A tibble: 0 × 0
```
