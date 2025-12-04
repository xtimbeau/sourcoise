# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

[sourcoise](https://xtimbeau.github.io/sourcoise/) is an R package that
provides intelligent caching for R scripts. It acts as a drop-in
replacement for [`base::source()`](https://rdrr.io/r/base/source.html)
but with persistent disk-based caching across sessions. The package is
designed to work seamlessly with Quarto projects and GitHub workflows.

**Core concept**: Execute R scripts that fetch data from files/APIs,
cache the results to disk, and reuse cached data when nothing has
changed. This enables fast rendering of Quarto documents even when APIs
are slow or unavailable.

## Development Commands

### Package Building and Testing

``` r
# Load package for development
devtools::load_all()

# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-sourcoise.R")

# Check package (R CMD check)
devtools::check()

# Build documentation
devtools::document()

# Build README from README.Rmd
rmarkdown::render("README.Rmd")
```

### Code Quality

``` r
# Check code coverage
covr::package_coverage()
```

### Installation for Testing

``` r
# Install from local source
devtools::install()

# Install with dependencies
devtools::install_deps(dependencies = TRUE)
```

## Architecture

### Core Function Flow

The package has a two-layer architecture for the main
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
function:

1.  **[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)**
    (R/sourcoise.R:91-122) - User-facing wrapper that collects options
    from global settings
2.  **`sourcoise_()`** (R/sourcoise.R:126-229) - Internal implementation
    handling cache logic

**Execution flow**:

    sourcoise()
      → setup_context() [finds files, sets up paths, initializes logging]
      → valid_metas() [checks cache validity]
      → exec_source() [executes script if needed]
      → cache_data() [saves results to disk]
      → data_returned() [formats output]

### Cache Invalidation System

Cache is invalidated when any of these conditions are met
(R/sourcoise.R:13-18):

1.  No cache found
2.  Source script modified (detected via hash)
3.  Tracked files modified (via `track` parameter)
4.  Cache expired based on `lapse` parameter
5.  Execution forced via `force_exec=TRUE`

### Module Organization

- **R/sourcoise.R** - Main entry point and cache decision logic
- **R/cache_tools.R** - Cache writing, reading, and hash management
- **R/exec_tools.R** - Script execution in isolated environment
- **R/setup_tools.R** - Context setup: finds project root, resolves
  paths
- **R/path_tools.R** - Path resolution and file finding heuristics
- **R/metadata_tools.R** - JSON metadata reading/writing/validation
- **R/sourcoise_status.R** - Cache inspection across project
- **R/sourcoise_refresh.R** - Batch cache refresh with progress tracking
- **R/sourcoise_clear.R** - Cache cleanup utilities
- **R/other_tools.R** - Quarto-specific helpers (unfreeze, uncache)

### Key Design Patterns

**Project Root Detection** (R/setup_tools.R): - Uses `rprojroot` to find
project root (.Rproj file, git repo, etc.) - Falls back to multiple
heuristics if standard detection fails - Supports manual override via
`options(sourcoise.root = path)`

**File Finding Heuristic** (R/path_tools.R): - When script path is
incomplete, searches entire project - Selects closest match to caller’s
working directory - Warns when multiple matches found

**Working Directory Management** (R/exec_tools.R:15-21): - Script
executed with [`setwd()`](https://rdrr.io/r/base/getwd.html) to script’s
directory (by default) - Original directory restored via
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) - Enables portable
scripts with relative paths to companion files

**Hash-Based Naming** (R/cache_tools.R): - Cache files named:
`{script}_{hash}-{uid}-{counter}.qs2` - Hash based on script content,
args, tracked files - Avoids cache collision across different
machines/users

**Serialization**: Uses `qs2` package for fast, cross-platform binary
serialization

**Error Recovery** (R/sourcoise.R:208-222): - If script fails but cache
exists, returns cached data (possibly invalid) - Logs error but doesn’t
block document rendering - Critical for handling unreliable APIs

## Global Options

Configure package behavior via
[`options()`](https://rdrr.io/r/base/options.html):

``` r
options(
  sourcoise.root = NULL,           # Force project root path
  sourcoise.src_in = "project",    # "project" or "file" for cache location
  sourcoise.nocache = FALSE,       # Disable caching entirely
  sourcoise.log = "OFF",           # Log threshold: "OFF", "INFO", "DEBUG"
  sourcoise.grow_cache = 5,        # Max number of cache versions kept
  sourcoise.limit_mb = 50,         # Max individual cache file size (MB)
  sourcoise.force_exec = FALSE,    # Force execution globally
  sourcoise.prevent_exec = FALSE,  # Prevent execution globally
  sourcoise.metadata = FALSE,      # Return metadata by default
  sourcoise.wd = "file",           # Working dir: "file", "project", "qmd"
  sourcoise.lapse = "never",       # Default expiry: "never", "1 day", etc.
  sourcoise.encoding = "UTF-8",    # Source file encoding
  sourcoise.init_fn = NULL         # Function to run before refresh
)
```

## Testing Notes

- Test suite uses [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  for isolated cache directories
- Key test file: `tests/testthat/test-sourcoise.R`
- Tests copy example scripts from `inst/` directory
- Must set root explicitly in tests: `set_sourcoise_root(dir)`
- Test examples include INSEE API calls (conditional on package
  availability)

## Cache Structure

Cache stored in `.sourcoise/` directory (hidden, but committed to git):

    .sourcoise/
      {script_dir}/
        {script_name}-{args_hash}/
          {script_name}_{src_hash}-{uid}-{counter}.json  # Metadata
          {script_name}-{data_hash}.qs2                  # Cached data

**Metadata JSON contains**: - Execution date, timing, data size - Script
hash, tracked file hashes, args hash, data hash - Cache validity
status - Associated .qmd files (for Quarto projects) - Log file path

## Quarto Integration

When
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
is called from a .qmd file:

1.  Detects calling .qmd file path
2.  Stores association in cache metadata
3.  [`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md)
    can unfreeze/uncache related .qmd files
4.  Enables workflow: refresh data → auto-unfreeze docs → re-render

## Important Implementation Details

**Priority System** (R/sourcoise_refresh.R:23): - Scripts can set
`priority` parameter (default 10) - Higher priority scripts execute
first during refresh - Enables cascading
[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
calls (script A calls script B)

**User ID per Machine** (R/setup_tools.R:36): - UID is CRC32 hash of
project root path - Different machines have different UIDs - Prevents
cache conflicts on shared repos

**Data Deduplication** (R/cache_tools.R:13-40): - Before caching, checks
if data hash matches existing cache - If identical, reuses existing .qs2
file - Saves disk space and reduces writes

**Size Limit Safety** (global option `sourcoise.limit_mb`): - If cached
object exceeds limit, no caching occurs - Prevents bloating repository
with large files

## Common Patterns

**Basic usage in .qmd**:

``` r
library(sourcoise)
data <- sourcoise("scripts/fetch_data.R")
# Use data for plotting/analysis
```

**With tracked files**:

``` r
data <- sourcoise("analysis.R", track = list("raw_data.csv"))
# Re-executes when raw_data.csv changes
```

**With periodic refresh**:

``` r
data <- sourcoise("api_call.R", lapse = "1 day")
# Re-executes once per day
```

**Refresh all invalid caches**:

``` r
sourcoise_refresh(force_exec = FALSE)  # Only invalid
sourcoise_refresh(force_exec = TRUE)   # Force all
```

**Check cache status**:

``` r
status <- sourcoise_status()
# Returns tibble with all cache info
```

## Style Conventions

This codebase follows modern tidyverse patterns per the top-level
CLAUDE.md Modern R Development Guide. When contributing:

- Use native pipe `|>` not `%>%`
- Use `.by` for per-operation grouping
- Use `cli` package for user messages
- Use `logger` for internal logging
- Use modern join syntax with `join_by()`
- Follow tidyverse style guide (snake_case, spacing)

## Dependencies

**Core runtime dependencies** (from DESCRIPTION): - `qs2` - Fast
serialization - `cli` - User messages - `digest` - Hashing - `fs` -
Cross-platform file operations - `purrr`, `dplyr`, `tibble` - Data
manipulation - `lubridate` - Date handling - `stringr`, `glue` - String
operations - `rprojroot` - Project root detection - `rlang` - Tidy
evaluation - `logger` - Logging - `jsonlite` - Metadata storage -
`lobstr` - Object size calculation - `scales` - Size formatting

**Suggested packages**: - `testthat` - Testing - `bench` - Performance
measurement - `memoise` - Additional memoization - `quarto` - Quarto
integration
