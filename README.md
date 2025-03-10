# sourcoise <a href="https://xtimbeau.github.io/sourcoise/"><img src="man/figures/logo.png" align="right" height="102" alt="sourcoise website" /></a>

`{sourcoise}` is a package that provides tools for running an R script and caching the results. The aim is to be able to very quickly execute code that accesses files or an API and which, in the absence of updates, always produces the same result. When the API is likely to block (or if you don't have an internet connection), this avoids blocking the rendering of a document or a quarto site.

It also means that the script code that retrieves the data from a file has to be isolated to improve reproducibility. `sourcoise()` can be called in a `sourcoise()` which allows modularity. It provides tools for checking the cache and refreshing it on demand.

## Installation

`{sourcoise}` can be installed from CRAN:

```r
install.packages("sourcoise")
```

The development version can be installed from *github*:

```r
devtools::install_gitub("xtimbeau/sourcoise")

# alternativement
pak::pak("xtimbeau/sourcoise")
```

## Use

To populate a graph or table with data, put the code in a script `r` (`"mon_script.r"`), ending the script with a `return(data_pour_le_graphique)`.
In the `.qmd` or `.rmd` (or also an `R` script) we have the instructions for the graph in a `r` code chunk:

````qmd
```{r}
library(tidyverse)
library(sourcoise)

mes_datas <- sourcoise("mon_script.r")
ggplot(mes_datas) + <<graph code>>

```
````

The first time the script is run, subsequent calls will use the cache, unless the cache is invalidated.

## Benefits

There are many benefits:

1. time savings when code execution takes a long time (accessing an API, downloading large amounts of data, major processing). Reading an excel file can also take a long time. The time taken to access cached data depends on its size, but even for large data (and there's no reason why it should be that large), the order of magnitude is a few milliseconds, thanks to optimization.

2. the cache is transferable via *github*. It's in a (hidden) folder, but saved in the project folder and *committed* by github. The cache produced on a workstation can therefore be accessed via `pull` on other workstations, without the need to reexecute the code.

3. if the source code triggers an error, you can override it: In the case of a package that is not installed, missing data (for example, an absolute path in the code), or an API that blocks (such as that of the OECD), then `sourcoise()` tries to take the result of the last successful execution (if cached). Although this can be problematic, i.e. an unreported error, it has the enormous advantage of not blocking the process and allowing the error to be handled in parallel.

4. `sourcoise()` cleverly searches for the source file in the project and executes the code in a local environment, changing the working directory to the one where the source code is located. This makes it possible to call the source code (the script `r` `mon_script.r` passed as a parameter to `sourcoise("mon_script.r")` scripts `r` data files `.csv` or `.xlsx` which are saved in the same directory as the `mon_script.r`. You can therefore reuse the code without having to worry about modifying the paths, which are relative to the folder where `mon_script.r` is.

5. This provides an embryo of reproducibility by designating the script that produces the data and thus allowing to complement the code chunk with a reference to reproduce it.

## Coming soon

- the ability to store hidden data outside of the project folder (and therefore outside of *github*) and to use `{pins}` for stcokage (but perhaps at the cost of slower access).

- a schema for declaring dependencies between calls to `sourcoise()` calls and trigger cascade executions.

- and possibly a *shiny* update interface (*gui* for `sourcoise_refresh()`)


