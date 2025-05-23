
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sourçoïse : Source and Cache

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sourcoise)](https://CRAN.R-project.org/package=sourcoise)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/xtimbeau/sourcoise/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/xtimbeau/sourcoise/actions/workflows/R-CMD-check.yaml)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/sourcoise)](https://cran.r-project.org/package=sourcoise)
[![CRAN_latest_release_date](https://www.r-pkg.org/badges/last-release/sourcoise)](https://cran.r-project.org/package=sourcoise)
[![Codecov test
coverage](https://codecov.io/gh/xtimbeau/sourcoise/graph/badge.svg)](https://app.codecov.io/gh/xtimbeau/sourcoise)
<!-- badges: end -->

`{sourcoise}` (pronouced \[suʁsɔiz\] as *sourçoïse*) is a package that
provides tools for running an R script and caching the results (saving
them to disk). The aim is to be able to execute (quickly) code that
accesses files or an API and which, in the absence of updates, always
produces the same result. When the API is likely to block (or if you
don’t have an internet connection), this avoids failure of the rendering
of a `qmd` document or a quarto website.

The script code that retrieves the data from a file or an API has to be
isolated, and that is improving reproducibility. `sourcoise()` can be
called in a `sourcoise()` which allows modularity. The cache is
persistent (on disk, possibly synched with github) across sessions of
the same project. The package provides tools, for checking the cache,
`sourcoise_status()`, and refreshing it on demand,
`sourcoise_refresh()`, or at a given frequency through a parameter of
`sourcoise()`.

## Installation

`{sourcoise}` can be installed from CRAN:

``` r
install.packages("sourcoise")
```

The development version can be installed from *github*:

``` r
devtools::install_gitub("xtimbeau/sourcoise")

# or (if pak is installed)
pak::pak("xtimbeau/sourcoise")
```

## Usage

To populate a graph or table with data, put the code in a script `r`
(`"mon_script.r"`), ending the script with a
`return(data_pour_le_graphique)`. In the `.qmd` or `.rmd` (or also an
`R` script) we have the instructions for the graph in a `r` code chunk:

```` qmd
```r
library(tidyverse)
library(sourcoise)

mes_datas <- sourcoise("mon_script.r")
ggplot(mes_datas) + <<graph code>>

```
````

The first time the script is run completely, subsequent calls will use
the cache on disk, persistent across session of the same project (R
project, ), unless the cache is invalidated.

To check the status of cache, just call `sourcoise_status()`. It will
scan your project and collect info about your cached data. To refresh
everything, call `sourcoise_refresh()`, it will execute all scripts and
refresh data in the cache. Options are available to filter what really
need to be refreshed.

## Many benefits

1.  time saving when code execution takes a long time (accessing an API,
    downloading large amounts of data, major processing). Reading an
    excel file can also take a long time. The time taken to access
    cached data depends on its size, but even for large data (and
    there’s no reason why it should be that large), the order of
    magnitude is a few milliseconds (4 to 10ms) for execution when data
    is cached, thanks to optimization and `{memoise}`.

2.  the cache is transferable via *github*. It’s in a (hidden) folder,
    but saved in the project folder and *committed* and *pushed* on
    github. The cache produced on a workstation can therefore be
    accessed via `pull` on other workstations, without the need to
    re-execute the code (and thanks to a smart naming scheme, without
    too much conflicts).

3.  if the source code triggers an error, you can override it: In the
    case of a package that is not installed, missing data (for example,
    an absolute path in the code), or an API that blocks (such as the
    OECD API, particularly unreliable), then `sourcoise()` tries to take
    the result of the last successful execution (if cached). Although
    this can be problematic, i.e. an unreported error and use of an
    ivalid cache, it has the enormous advantage of not blocking the
    process and allows to handle the error in parallel.

4.  `sourcoise()` cleverly searches for the source file in the project
    and executes the code in a local environment, changing the working
    directory to the one where the source code is located. This makes it
    possible to call the source code (the script `r` `mon_script.r`
    passed as a parameter to `sourcoise("mon_script.r")` scripts `r`
    data files `.csv` or `.xlsx` which are saved in the same directory
    as the `mon_script.r`. You can therefore reuse the code without
    having to worry about modifying the paths, which are relative to the
    folder where `mon_script.r` is. Then, the code is resusable anywhere
    in the project or elsewhere. Note that it is done with simple copy
    of source scripts and does not compare to a common code called in
    many situation, which involves another approach (a package and
    functions for instance).

5.  This provides an embryo of reproducibility by designating the script
    that produces the data and thus allowing to complement the code
    chunk with a reference to reproduce it. It is possible to propose
    the code to be downloaded in the qmd.

## Coming soon

- the ability to store hidden data outside of the project folder (and
  therefore outside of *github*) and to use `{pins}` for storage (but
  perhaps at the cost of slower access).

- a schema for declaring dependencies between calls to `sourcoise()`
  calls and trigger cascade executions.

- and possibly a *shiny* update interface (*gui* for
  `sourcoise_refresh()`)
