# Force root

[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
mechanism to find root of the project automatically can be bypassed.
This function is equivalent to setting the `sourcoise.root` option,
except for storage of cache at the level of the file. To allow thus
behaviour, root should be set to `NULL`.

## Usage

``` r
set_sourcoise_root(root = NULL, quiet = TRUE)
```

## Arguments

- root:

  (default `NULL`, character) path of the root

- quiet:

  (default `TRUE` boolean) displays messages

## Value

root set (character)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
#> [1] "/tmp/RtmpuHG5sJ"
```
