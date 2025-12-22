# Set the Root Directory for Sourcoise

This function allows you to manually set the root directory for the
sourcoise package, bypassing the automatic root detection mechanism used
by
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md).
Setting the root directory affects where sourcoise looks for files and
stores cache data.

## Usage

``` r
set_sourcoise_root(root = NULL, quiet = TRUE)
```

## Arguments

- root:

  Path to the desired root directory. If `NULL` (default), sourcoise
  will attempt to automatically detect the project root. Can be an
  absolute or relative path.

- quiet:

  Logical value indicating whether to suppress messages during root
  detection. Default is `TRUE` (messages suppressed).

## Value

The root path that was set (character string), invisibly returned by
`try_find_root()`.

## Details

By default, sourcoise automatically detects the project root. This
function is equivalent to setting the `sourcoise.root` option directly,
except when dealing with file-level cache storage. To enable file-level
cache storage behavior, set root to `NULL`.

## Examples

``` r
# Set root to a temporary directory
dir <- tempdir()
set_sourcoise_root(dir)
#> [1] "/tmp/RtmpKQIHh9"

# Reset to automatic detection
set_sourcoise_root(NULL)
#> /tmp/RtmpKQIHh9

# Set root with messages enabled
set_sourcoise_root(dir, quiet = FALSE)
#> [1] "/tmp/RtmpKQIHh9"
```
