# Changelog

## sourcoise 1.1.1

- reports error more systematically
- bug fix: error messages for a script that is not found, whose
  execution is prevented or that fails, with no cache to return, are now
  formatted when the error is raised. They were formatted lazily by
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
  and when the error was re-raised in another process (for instance by
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html))
  this hid the real error behind “Could not evaluate cli
  [`{}`](https://rdrr.io/r/base/Paren.html) expression: `ctxt$src`”.
- bug fix: scripts whose names end alike (such as `data.R` and
  `mydata.R` in the same folder) no longer share a cache. Cache files
  were selected with an unanchored pattern matched against the full
  path, so `sourcoise("data.R")` could return the data cached for
  `mydata.R`. Script names are now matched literally (a `.` in a name is
  no longer a wildcard).
- bug fix:
  [`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md)
  (through its internal cleaning step) deleted all cached data files, as
  `.qs2` files were never listed as belonging to the cache. Only orphan
  files are now removed.
- bug fix: `sourcoise_clear(what2keep = "recent")` failed. It now keeps,
  for each script and set of arguments, only the latest cached data and
  metadata. An unknown value of `what2keep` now gives an explicit error
  and the documentation of the argument has been corrected.
- bug fix: scripts with a `.` in their name (such as `my.data.R`) were
  never served from cache, as their metadata file was written under a
  truncated name (`my.json`) and so never found.
- bug fix: the cache of a script that was modified and re-executed was
  dropped from the cache listing used by
  [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
  when looking for a cache after a failed execution. A script failing
  after an edit (for instance when `lapse` has expired and there is no
  network) ended with “failed and no cache to return” although a cache
  existed, instead of returning it as an invalid cache.
  [`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)
  and
  [`sourcoise_meta()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_meta.md)
  were affected as well.
- bug fix: hashing a missing file does not consume random numbers any
  more, so [`set.seed()`](https://rdrr.io/r/base/Random.html) sequences
  of the user are no longer altered by
  [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md).
- bug fix: internal paths of metadata files are now correctly made
  relative to the project root, including when the root contains
  characters special to regular expressions.

## sourcoise 1.1.0

CRAN release: 2026-01-07

- new coverage case and error condition when nothing is returned
- new
  [`sourcoise_meta()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_meta.md)
  producing the same result as
  [`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)
  plus explanation of cache status
- Source not found and execution error with no cache now trigger an
  error
- performance improvement (50%) in cache data cleaning and testing
  existence of files (not anymore done with fs)
- Uppercase is now correctly treated on UNIX systems
- refactor context mechanic, normally safer, plus more test
- tracking is smarter (accumulation)
- errors messages are safer and better and logged systematically when
  refreshing
- circulation in execution cases is safer
- performance improvement when data is cached (30%)
- a bug when source name contains a “-” is corrected (annoying)
- accumulation of tracked files or qmd files, accumulated only when they
  exists (clear track to do, and some cleaning to do in case of track
  changes)

## sourcoise 1.0.0

CRAN release: 2025-12-09

- performance of data gathering has been largely improved, thanks to
  `RcppSimdJson` package
- performance in case of a large number of cached files has been largely
  improved thanks to a early selection of candidates
- helpers have been introduced to select easily in `sourcoise_refresh`
  what will be refreshed
- when refreshing, execution is forced for subcalls to sourcoise, only
  once, in order to allow for consistent refresh
- priority is used to order execution of files when refreshing
- function to set priority (internal use only)
- short output of
  [`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)
- cleaning (with
  [`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md)
  and
  [`sourcoise_clear_all()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear_all.md))
  of cached data when source file does not exist any more
- better handling of error messages, logged and displayed
- critical errors displayed, such as unfound file
- not quiet by default
- `sourcoise_refresh` identify and warn when new data has been generated
- exec engine is based on `parse` and `eval` which solve some bugs (like
  when returning a ggplot)
- `lobstr::object_size` fails on S7 object, this is treated as an
  exception
- `sourcoise_refresh` warns when an error occurs and diplays the error
- multiple minor bugs have been solved

## sourcoise 0.6.1

CRAN release: 2025-05-24

CRAN test failed, unfailed.

## sourcoise 0.6.1

CRAN release: 2025-05-24

CRAN test failed

## sourcoise 0.6.0

CRAN release: 2025-05-23

### Correction de bugs

- enregistre correctement le champ date (quand cache est retourné).
- `ignore.case` dans les recherches de fichiers.
- enregistre correctement le chemin du log si le cache est invalide.
- les arguments sont employés (par un *hash*) pour différencier les
  caches, ce qui permet de les utiliser sans problème.
- le log est maintenant spécifique à chaque utilisateur.
- cas où le nom de fichier comporte un “.” (ne considère pas ça comme
  une extension).
- cas où il n’y a pas de projet (root=wd dans ce cas) et
  [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
  fonctionne (merci François G.).
- nommage correct du fichier si il n’y a pas d’arguments.
- différencie bien les scripts selon les arguments lors de `status` ou
  `refresh`
- vérifie que le json est correctement formé à la lecture (merci Elliot)
- force l’encodage à être selon l’option `sourcoise.encoding` et à être
  `UTF-8` pour fonctionner sur le multiplateforme avec macOS et windows

### ajouts

- le paramètre `priority` permet de controller l’ordre d’exécution dans
  le cas d’un refresh, afin de traiter sommairement les cascades
  d’exécution.
- simplification des paramètres de
  [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md).
  Les paramètres enlevés sont fixés globalement par
  [`options()`](https://rdrr.io/r/base/options.html).
- la fonction
  [`set_sourcoise_root()`](https://xtimbeau.github.io/sourcoise/reference/set_sourcoise_root.md)
  permet de fixer la racine de `sourcoise`.
- la fonction
  [`sourcoise_meta()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_meta.md)
  renvoie les métadonnées directement (sans les datas donc).
- introduit une nouvelle convention d’accès aux fichiers sources (sera
  étendue pour les autres) : “/chemin/src” part de la racine du projet
  systématiquement, “src.r” cherche dans le dossier de l’appelant

### autres

- tests inclus, [covr](https://covr.r-lib.org) implementé.

## sourcoise 0.5.0

CRAN release: 2025-03-15

- CRAN version

## sourcoise 0.4.0

- CRAN version

## sourcoise 0.3.3

### Correction de bugs

- traite correctement les sources multiples et trouve le plus proche
  mieux.

#### Ajouts

- utilise [memoise](https://memoise.r-lib.org) pour cacher en mémoire
  les données sur disque

## sourcoise 0.3.2

### Correction de bugs

- retourne un cache invalide si l’éxécution a échouée.
- n’exécute plus deux fois le source lorsque l’éxécution échoue.
- meilleurs messages d’erreur ou de succès

## sourcoise 0.3.1

### Correction de bugs

- fonctionne correctement avec `src_in="file"` qui enregistre le cache
  au niveau du script R sourcé

## sourcoise 0.3.0

### Ajouts

- [`sourcoise_reset()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_reset.md)
  qui est efface tout (enfin, tout ce que `sourcoise` fabrique).

### Améliorations

- [`sourcoise_clear()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_clear.md)
  est plus prudent et plus efficace.
- [`sourcoise_status()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise_status.md)
  est plus robuste et nettoie les data_file orphelins.
- `{qs}` est remplacé par [qs2](https://github.com/qsbase/qs2), ce qui
  corrige le bug de [gt](https://gt.rstudio.com) (-;)
- le cache est enregistré dans .sourcoise (ainsi que le log)

## sourcoise 0.2.0

### Ajouts

- [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
  a son propre package
  [sourcoise](https://xtimbeau.github.io/sourcoise/), qui est importé
  dans `{ofce}`.
- [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
  a un système de log (par [logger](https://daroczig.github.io/logger/))
- [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
  a un système de prune
- [`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
  a une limite la taille des données sauvegardées

### Correction de bugs

plus quelques bugs réparés.

## sourcoise 0.1.0

[`sourcoise()`](https://xtimbeau.github.io/sourcoise/reference/sourcoise.md)
sort du package `{ofce}` et devient indépendant. Il change de nom
(anciennement `source_data()` qui reste un alias pour la compatibilité
descendante)
