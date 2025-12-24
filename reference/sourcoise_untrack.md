# Remove Tracking from Cached Files

Removes tracking metadata from cached files associated with a given path
by setting the track field to an empty list.

## Usage

``` r
sourcoise_untrack(path, root = getOption("sourcoise.root"))
```

## Arguments

- path:

  Character string specifying the file path whose cache metadata should
  be updated.

- root:

  Character string specifying the root directory for the cache. Defaults
  to `getOption("sourcoise.root")`.

## Value

Invisibly returns the results of writing metadata for each updated cache
entry. Returns a message string if no files are found or no tracked
files exist.

## Details

NOte that tracked fies are accumulated when specified in `track`
argument of `soucoise()`. This function allows to reset the list.

The function locates all cache entries for the specified path, filters
to entries that currently have tracking enabled (non-empty priority
field), and removes tracking by setting the track field to an empty
list. Only affects the most recent cache entry for each argument hash.

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove tracking from cached results
sourcoise_untrack("scripts/analysis.R")
} # }
```
