# sources R script and caches results on disk

`sourcoise()` is used as a drop in replacement for
[`base::source()`](https://rdrr.io/r/base/source.html) but caches
results on disk. Cache is persistant over sessions.

## Usage

``` r
sourcoise(
  path,
  args = list(),
  track = list(),
  lapse = getOption("sourcoise.lapse"),
  force_exec = getOption("sourcoise.force_exec"),
  prevent_exec = getOption("sourcoise.prevent_exec"),
  metadata = getOption("sourcoise.metadata"),
  wd = getOption("sourcoise.wd"),
  quiet = TRUE,
  inform = FALSE,
  priority = 10
)
```

## Arguments

- path:

  (character) path of the script to execute (see details).

- args:

  (list) list of args that can be used in the script (in the form
  `args$xxx`).

- track:

  (list) list of files which modification triggers cache invalidation
  and script execution .

- lapse:

  (character) duration over which cache is invalidated. Could be `never`
  (default) `x hours`, `x days`, `x week`, `x months`, `x quarters`,
  `x years`.

- force_exec:

  (boolean) execute code, disregarding cache valid or invalid.

- prevent_exec:

  (boolean) prevent execution, cache valid or not, returned previous
  cached data, possibly invalid.

- metadata:

  (boolean) if TRUE `sourcoise()` returns a list with data is the
  `$data` and various meta data (see details).

- wd:

  (character) if `project` working directory for the execution of script
  will be the root of the project. If `file` then it will be the dir of
  the script (default) If `qmd`, then working dir will be the dir in
  which the calling `qmd` is. Current directory is restored after
  execution (successful or failed).

- quiet:

  (boolean) mute messages and warnings from script execution.

- inform:

  (boolean) Display logs on console, even if logging is disabled with
  threshold level "INFO".

- priority:

  (integer) (default 10) can be used as a way to control the order of
  execution when refreshing data (see
  [`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md))

## Value

data (list ou ce que le code retourne)

## Details

`sourcoise()` looks like
[`base::source()`](https://rdrr.io/r/base/source.html). However, there
are some minor differences.

First, the script called in `sourcoise()` must end by a
[`return()`](https://rdrr.io/r/base/function.html) or by an object
returned. Assignment made in the script won't be kept as `sourcoise()`
is executed locally. Only explicitly reruned object will be returned. So
`soucoise()` is used by assigning its result to something
(`aa <- sourcoise("mon_script.r)` or `sourcoise() |> ggplot() ...`).
Unless specified otherwise with `wd` parameter, the working directory
for the script execution is (temporarly) set to the dir in which is the
script. That allows for simple access to companion files and permit to
move the script and companion files to another dir or project.

Second, an heuristic is applied to find the script, in the event the
path given is incomplete. Whereas it is not advised and comes with a
performance cost, this can be useful when there is a change in the
structure of the project. The heuristic is simple, the script is
searched inside the porject dir and among all hits the closest to the
caller is returned.

Third, if an error is triggered by the script, `sourcoise()` does not
fail and return the error and a NULL return. However, if there is a
(invalid or valid) cache, the cached data is returned allowing for the
script to continue. In that case the error is logged.

Cache is invalidated when : 1 - a cache is not found 2 - the script has
been modified 3 - tracked files have been modified 4 - last execution
occurred a certain time ago and is considered as expired 5 - execution
is forced

Whatever values takes `src_in`, if the file path starts with a `/`, then
the source file will be interpreted from project root (if any). This is
coherent whith naming convention in `quarto`. Otherwise, the document
path wil be used firstly (if any, that is to say executed from quarto,
rendering). Finally, working directory will be used. If everything
fails, it will try to search in the project directory a corresponding
file and will keep the closest from the calling point.

Usually the fisrt call return and cache the results. Results can be aby
R object and are serialized and saved using `qs2`. Subsequent calls,
supposing none of cache invalidation are true, are then very quick. No
logging is used, data is fecteched from the cache and that's it. For
standard size data, used in a table or a graph (\< 1Mb roughly), return
timing is under 5ms.

`lapse` parameter is used for invalidation trigger 4. `lapse = "1 day"`
ou `lapse="day"` for instance will trigger once a day the execution.
`lapse = "3 days"` will do it every 72h. `hours`, `weeks`, `months`,
`quarters` or `years` are understood time units. MOre complex calendar
instructions could be added, but `sourcoise_refesh()` provides a
solution more general and easy to adapt to any use case, as to my
knowledge, there is no general mechanism to be warned of data updates.

`track` is the trigger \#3. It is simply a list of files (following path
convention defined by `scr_in`, so either script dir of project dir as
reference). If the files in the list are changed then the execution is
triggered. It is done with a hash and it is difficult to have a croo
plateform hash for excel files. Nevertheless, hash is done on text files
with same results of different platforms.

## Global options

In order to simplify usage and to avoid complex bugs, some parameters
can be set only globally, through options().

- `sourcoise.root` (character) force root, and bypass soucroise
  mechanism to find root. Useful when you want to execute sourcoise in a
  non-project context (see examples). `sourcoise.src_in` (character) if
  `project` stores the cache folder (`.sourcoise`) at the project root,
  if `file`, `.sourcoise` is stored at the calling point.

- `sourcoise.nocache` (boolean) no caching, so makes sourcoise less
  useful, can be used for testing purpose

- `sourcoise.log` (default "OFF") log threshold (see
  `logger::log_treshold()`).

- `sourcoise.grow_cache` (integer) (default 5 par défaut) cache limit in
  number of data file kept.

- `sourcoise.limit_mb` (integer) (default 50) individual cache data
  files size on disk limit. If above **no caching** occurs.

## Metadata

If `metadata=TRUE`, a list is returned, with some metadatas. Main ones
are `$data`, the data returned, `$date`, execution date, `$timing`
execution timing, `$size` of the R object in memory, `$data_file`,
`$data_date` and `$file_size` documenting data file path, date size on
disk and last modification date, parameters of the call (`$track`,
`$wd`, `$src_in`, `$args` and so on).

`force_exec` and `prevent_exec` are parameters that force the script
execution (trigger \#5) of prevent it (so cache is returned or NULL if
no cache). Those 2 parameters can be set for one specific execution, but
they are intendend to a global setting through the option
`sourcoise.force_exec` or `sourcoise.prevent_exec`.

If returned data after execution is not different than previously cached
data, then no caching occurs in order to limit the disk use and to avoid
keeping an history of the same data files. This implies the possibility
of a difference between last execution date and last data modification
date. If you are insterested in the moment data was changed, then
`$data_date` is to be preferred.

## Working with github

`sourcoise()` is designed to function with *github*. Cache information
is specific to each user (avoiding conflicts) and cached data is named
with the hash. Conflicts could occur in the rare case the same script is
executed on different machines and that this script return each time a
different result (such as a random generator).

## See also

Other sourcoise:
[`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md),
[`sourcoise_refresh()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_refresh.md),
[`sourcoise_reset()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_reset.md),
[`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)

## Examples

``` r
dir <- tempdir()
set_sourcoise_root(dir)
#> [1] "/tmp/RtmpIJX9di"
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
  dir,
  overwrite = TRUE)
# Force execution (root is set explicitly here, it is normally deduced from project)
data <- sourcoise("some_data.R", force_exec = TRUE)
# The second time cache is used
data <- sourcoise("some_data.R")
# Performance and mem test
dir <- tempdir()
set_sourcoise_root(dir)
#> [1] "/tmp/RtmpIJX9di"
fs::file_copy(
   fs::path_package("sourcoise", "some_data.R"),
   dir,
   overwrite = TRUE)
bench::mark(
 forced = data <- sourcoise("some_data.R", force_exec = TRUE),
 cached = data <- sourcoise("some_data.R"),
 max_iterations = 1)
#> # A tibble: 2 × 13
#>   expression      min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
#>   <bch:expr> <bch:tm> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
#> 1 forced         35ms   35ms      28.6     920KB        0     1     0       35ms
#> 2 cached       11.5ms 11.5ms      87.1     287KB        0     1     0     11.5ms
#> # ℹ 4 more variables: result <list>, memory <list>, time <list>, gc <list>
```
