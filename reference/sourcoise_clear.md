# Cleans sourcoise cache

Always removes files in `.sourcoise` folders that do not belong to the
cache (orphans), then removes cache files according to `what2keep`.

## Usage

``` r
sourcoise_clear(what2keep = "all", root = NULL)
```

## Arguments

- what2keep:

  (character) one of `"all"` (the default) keeps every cache file, so
  only orphan files are removed, `"recent"` keeps, for each script and
  set of arguments, only the latest cached data and its metadata, and
  `"nothing"` removes the whole cache.

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
if (FALSE) { # rlang::is_installed("insee")
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
sourcoise_status()
}
```
