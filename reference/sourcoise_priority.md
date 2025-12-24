# Change Priority of Cached Files

Updates the priority metadata for cached files associated with a given
path. Only affects cache entries where the priority differs from the
specified value.

## Usage

``` r
sourcoise_priority(path, priority = 10, root = getOption("sourcoise.root"))
```

## Arguments

- path:

  Character string specifying the file path whose cache metadata should
  be updated.

- priority:

  Numeric priority value to set. Default is 10. Lower values indicate
  higher priority.

- root:

  Character string specifying the root directory for the cache. Defaults
  to `getOption("sourcoise.root")`.

## Value

Invisibly returns the results of writing metadata for each updated cache
entry. Returns a message string if no files are found or no changes are
needed.

## Details

The function locates all cache entries for the specified path, filters
to the most recent entry for each argument hash, and updates the
priority metadata only for entries where the current priority differs
from the specified value.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set priority to 5 for cached results of a script
sourcoise_priority("scripts/analysis.R", priority = 5)

# Use default priority of 10
sourcoise_priority("scripts/model.R")
} # }
```
