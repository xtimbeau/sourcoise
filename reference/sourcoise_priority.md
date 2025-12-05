# Set priority for sourcoise

The desingated file is set to the priority. Low priority means that when
refreshing those files are groing to be executed first; the reason being
that this file is used by other and that you prefer it is refreshed
before refreshing other. However, when refreshing, subcall to sourcoise
are executed, unless priority is set to 0

## Usage

``` r
sourcoise_priority(path, priority = 10, root = getOption("sourcoise.root"))
```

## Arguments

- path:

  the file for which priority is set

- priority:

  the level of priority (10 is the default)

- root:

  the root – use only if you know what it does

## Value

list of json created

## Details

So, if you want to refresh it first and then not (because it is long to
refresh), then, ste it to 0

Default priority is 10
