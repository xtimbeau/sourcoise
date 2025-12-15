# Resets sourcoise

Removes all `.sourcoise` folders found under the project root.

## Usage

``` r
sourcoise_reset(root = NULL)
```

## Arguments

- root:

  to force root (expert use)

## Value

No return, effect is through removal of .sourcoise folders (this is a
side effect, no undo possible)

## See also

Other sourcoise:
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md),
[`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md),
[`sourcoise_clear_all()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear_all.md),
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md),
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
data <- sourcoise("some_data.R", force_exec = TRUE)
#> Error in purrr::map(cache_reps, ~fs::dir_ls(.x, regexp = jpat, recurse = TRUE)): ℹ In index: 1.
#> ℹ With name: /tmp/RtmpLY1YKh.
#> Caused by error:
#> ! [ENOENT] Failed to search directory '/tmp/RtmpLY1YKh/.sourcoise': no such file or directory
sourcoise_reset()
```
