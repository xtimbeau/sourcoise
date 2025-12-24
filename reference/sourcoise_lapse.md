# Change Cache Lapse Policy

Updates the lapse policy metadata for cached files associated with a
given path. The lapse policy determines when cached results should
expire.

## Usage

``` r
sourcoise_lapse(path, lapse = "never", root = getOption("sourcoise.root"))
```

## Arguments

- path:

  Character string specifying the file path whose cache metadata should
  be updated.

- lapse:

  Character string specifying the lapse policy. Default is "never".
  Common values include "never", "daily", "weekly", or custom time
  periods.

- root:

  Character string specifying the root directory for the cache. Defaults
  to `getOption("sourcoise.root")`.

## Value

Invisibly returns the results of writing metadata for each updated cache
entry. Returns a message string if no files are found or no changes are
needed.

## Details

The function locates all cache entries for the specified path, filters
to the most recent entry for each argument hash, and updates the lapse
policy metadata only for entries where the current lapse value differs
from the specified value.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set cache to expire daily
sourcoise_lapse("scripts/analysis.R", lapse = "daily")

# Set cache to never expire (default)
sourcoise_lapse("scripts/model.R", lapse = "never")
} # }
```
