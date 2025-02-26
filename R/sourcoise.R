# sourcoise est un outil qui permet d'exécuter un code, d'en cacher le résultat dans un dossier spécial (_data) en gardant des métadonnées
# sauf modifications ou écart de temps (à configurer), les appels suivant au code ne sont pas exécutés mais le ficheir de data est relu
# quelques fonctions permettent de diagnostiquer le cache et de suivre les mises à jour.


#' sourcoise : exécute le code et cache les données
#'
#' Cette fonction s'utilise presque comme `base::source()` et permet d'en accélérer l'exécution par le cache des données. `source_data()` est un alias de `sourcoise()`.
#'
#' Le fichier source est donné en entrée. Le chemin est relatif au projet, mais si il n'est pas trouvé dans le projet, il est cherché en partant de la racine.
#' Si le paramètre `src_in` est mis à `"file"`, alors le source est cherché à partir du qmd (ou du wd si il n'y pas encore de qmd) et les données sont stockées à ce niveau.
#' Ce cas correspond donc à des dossiers qui ne partagent pas de code (le blog de l'OFCE), alors que l'autre cas correspond à des codes pouvant être partagés (la prévision)
#'
#' Le code est exécuté (dans un environnement local) et le résultat est mis en cache. Il est important que le code se termine par un return(les_donnees).
#' Si return() n'est pas présent dans le code, il n'est pas exécuté et un message d'erreur est envoyé ("NULL" est retourné).
#' le code est exécuté avec un contrôle d'erreur, donc si il bloque, "NULL" est renvoyé, mais sans erreur ni arrêt.
#' les appels suivants seront plus rapides et sans erreur (sauf si l'erreur n'est pas corrigée).
#'
#' Une modification du code est détectée, invalide le cache et déclenche l'éxécution si sourcoise est exécutée.
#'
#' Suivant le paramètre lapse on peut déclencher une exécution périodique.
#' Par exemple, pour ne pas rater une MAJ, on peut mettre `lapse = "1 day"` ou `"day"` et une fois par jour le code sera exécuté.
#' Cela permet d'éviter une exécution à chaque rendu, mais permet de vérifier fréquemment la MAJ.
#' On peut spécifier l'intervalle en heures (`hours`), en jours (`days`), en semaines (`weeks`), en mois (`months`) ou en trimestres (`quarters`).
#'
#' On peut bloquer l'exécution en renseignant la variable d'environnement `PREVENT_EXEC` par `Sys.setenv(PREVENT_EXEC = "TRUE")` ou dans `.Renviron`.
#' Ce blocage est prioritaire sur tous les autres critères (sauf en cas d'absence de cache ou l'exécution est essayée).
#'
#' Des métadonnées peuvent être renvoyées (paramètre `metadata`) avec la date de la dernière exécution (`$date`), le temps d'exécution (`$timing`),
#' la taille des données (`$size`), le chemin de la source (`$where`), le hash du source (`$hash_src`) et bien sûr les données (`$data`).
#' Cela peut servir pour renseigner un graphique.
#'
#' Les valeurs par défaut peuvent être modifiées simplement par `options(sourcoise.hash = FALSE)` par exemple et persistent pour une session.
#' Typiquement cela peut être mis dans rinit.r (et donc être exécuté par `ofce::init_qmd()`) et cela sera l'option par défaut du projet.
#'
#' Le paramètre `wd` perment de spécifier le répertoire d'exécution du source.
#' Si il est mis à `"file"`, les appels à l'intérieur du code source, comme par exemple un save ou un load seront compris dans le répertoire où se trouve le fichier source.
#' L'intérêt est que le code peut avoir des éléments persistants, locaux
#' L'alternative est d'utiliser `wd="project"` auquel cas, le répertoire d'exécution sera independant de l'endroit où est appelé le code source.
#' Les éléments persistants peuvent alors être dasn un endroit commun et le code peut appeler des éléments persistants d'autres codes sources.
#' En le mettant à `qmd`l'exécution part du fichier qmd, ce qui est le comportement standard de `quarto`.
#' Toute autre valeur pour wd laisse le working directory inchnagé et donc dépendant du contexte d'exécution. Pour ceux qui aiment l'incertitude.
#'
#' En donnant des fichers à suivre par `track`, on peut déclencher l'exécution du source lorsque ces fichiers sont modifiés, c'est utile pour des fichiers sources sous excel (ou csv).
#'
#' `unfreeze` permet d'invalider le cache de quarto et de déclencher l'exécution du qmd pour mettre à jour la publication (et pas seulement les données en cache).
#'
#' @param path (character) le chemin vers le code à exécuter (sans extension .r ou .R ou avec au choix), ce chemin doit être relatif au projet (voir détails)
#' @param args (list) une liste d'arguments que l'on peut utliser dans source (args$xxx)
#' @param hash (boléen) Si TRUE (défaut) un changement dans le code déclenche son exécution
#' @param track (list) une liste de fichiers (suivant la même règle que src pour les trouver) qui déclenchent l'exécution.
#' @param lapse (character) peut être "never" (défaut) "x hours", "x days", "x weeks", "x months", "x quarters", "x years"
#' @param force_exec (boléen) Si TRUE alors le code est exécuté.
#' @param prevent_exec (boléen) Si TRUE alors le code n'est pas exécuté, ce flag est prioritaire sur les autres, sauf si il n'y a pas de données en cache
#' @param metadata (boléen) Si TRUE (FALSE par défaut) la fonction retourne une liste avec des métadonnées et le champ data qui contient les données elles même
#' @param wd (character) si 'project' assure que le wd est le root du project, si 'file' (défaut) c'est le fichier sourcé qui est le wd, si "qmd", c'est le qmd qui appelle
#' @param src_in (character) si "project" cherche le source dans le projet puis les sous dossiers, si "file" cherche dans le dossier du qmd (ou le wd). Dans ce cas, les données sont stockées dans le dossier en question.
#' @param exec_wd (character) NULL par défaut sauf usage particulier
#' @param quiet (boléen) pas de messages
#' @param root (character) force le root (usage non recommandé)
#' @param nocache (boléen) n'enregistre pas le cache même si nécessaire
#' @param cache_rep (character) défaut .data sauf usage particulier
#'
#' @family sourcoise
#' @return data (list ou ce que le code retourne)
#' @export
#'

sourcoise <- function(
    path,
    args = list(),
    hash = getOption("sourcoise.hash"),
    track = list(),
    lapse = getOption("sourcoise.lapse"),
    force_exec = getOption("sourcoise.force_exec"),
    prevent_exec = getOption("sourcoise.prevent_exec"),
    metadata = getOption("sourcoise.metadata"),
    wd = getOption("sourcoise.wd"),
    src_in = getOption("sourcoise.src_in"),
    exec_wd = NULL,
    cache_rep = NULL,
    root = NULL,
    quiet = TRUE, nocache = FALSE) {

  if(is.null(args))
    args <- list()

  if(is.null(track))
    track <- list()

  # on trouve le fichier
  name <- remove_ext(path)
  paths <- find_project_root()
  root <- try_find_root(root, src_in)

  if(!quiet)
    cli::cli_alert_info("root: {root}")
  uid <- digest::digest(root, algo = "crc32")
  if(!quiet)
    cli::cli_alert_info("uid: {uid}")
  if(is.null(cache_rep))
    root_cache_rep <- fs::path_join(c(root, ".data")) |> fs::path_norm()
  else
    root_cache_rep <- fs::path_abs(cache_rep)
  if(!quiet)
    cli::cli_alert_info("cache: {root_cache_rep}")


  src <- find_src(root, name)
  if(is.null(src)) {
    src <- try_find_src(root, name)
    if(length(src)==0) {
      if(!quiet)
        cli::cli_alert_warning("Le fichier n'existe pas en .r ou .R, vérifier le chemin")
      return(NULL)
    }
    if(length(src)>1) {
      if(!quiet)
        cli::cli_alert_warning("Plusieurs fichiers src sont possibles")
      l_src <- purrr::map(src, length)
      src <- src[[which.max(l_src)]]
    }
  }

  if(!quiet)
    cli::cli_alert_info("{.file {src}} comme source")

  if(length(check_return(src))==0) {
    cli::cli_alert_danger("Pas de return() d\u00e9t\u00e9ct\u00e9 dans le fichier {.file {src}}")
  }

  if(length(check_return(src))>1) {
    if(!quiet)
      cli::cli_alert_info("Plusieurs return() dans le fichier {src}, attention !")
  }

  basename <- fs::path_file(name)
  relname <- fs::path_rel(src, root)
  reldirname <- fs::path_dir(relname)
  full_cache_rep <- fs::path_join(c(root_cache_rep, reldirname)) |>
    fs::path_norm()
  qmd_path <- paths$doc_path
  if(Sys.getenv("QUARTO_DOCUMENT_PATH") != "") {
    qmd_file <- fs::path_join(c(qmd_path, knitr::current_input())) |>
      fs::path_ext_set("qmd") |>
      fs::path_norm()
  } else {
    qmd_file <- NULL
  }

  if(is.null(exec_wd)) {
    exec_wd <- getwd()
    if(wd=="project")
      exec_wd <- root
    if(wd=="file")
      exec_wd <- fs::path_dir(src)
    if(wd=="qmd") {
      if(!is.null(qmd_path)) {
        exec_wd <- qmd_path
      } else {
        cli::cli_alert_warning("Pas de document identifié, probablement, non exectu\u00e9 de quarto")
        exec_wd <- fs::path_dir(src)
      }
    }
  }

  if(is.null(force_exec)) force <- FALSE else if(force_exec=="TRUE") force <- TRUE else force <- FALSE
  if(is.null(prevent_exec)) prevent <- FALSE else if(prevent_exec=="TRUE") prevent <- TRUE else prevent <- FALSE

  src_hash <- hash_file(src)
  arg_hash <- digest::digest(args, "crc32")
  track_hash <- 0

  if(length(track) > 0) {
    track_files <- purrr::map(track, ~fs::path_join(c(root, .x)))
    ok_files <- purrr::map_lgl(track_files, fs::file_exists)
    if(any(ok_files))
      track_hash <- hash_file(as.character(track_files[ok_files]))
    else {
      cli::cli_alert_warning("Les fichiers de track sont invalides, v\u00e9rifiez les chemins")
    }
  }
  meta_datas <- get_mdatas(basename, full_cache_rep)
  qmds <- purrr::map(meta_datas, "qmd_file") |>
    purrr::list_flatten() |>
    purrr::discard(is.null) |>
    unlist() |>
    unique()
  new_qmds <- unique(c(qmds, qmd_file))
  if(force&!prevent) {
    our_data <- exec_source(src, exec_wd, args)
    if(our_data$ok) {
      our_data$lapse <- lapse
      our_data$src <- relname
      our_data$src_hash <- src_hash
      our_data$arg_hash <- arg_hash
      our_data$track_hash <- track_hash
      our_data$track <- track
      our_data$wd <- wd
      our_data$qmd_file <- new_qmds
      our_data$src_in <- src_in
      our_data$ok <- "exec"
      our_data$root <- fs::path_rel(root, paths$project_path)

      our_data <- cache_data(our_data, cache_rep = full_cache_rep, root = root, name = basename, uid = uid)
      if(!quiet)
        cli::cli_alert_warning("Ex\u00e9cution du source")

      if(metadata) {
        return(our_data)
      } else {
        return(our_data$data)
      }
    } else {
      if(!quiet)
        cli::cli_alert_warning("le fichier {src} retourne une erreur, on cherche dans le cache")
    }
  }

  meta_datas <- valid_metas(meta_datas, src_hash = src_hash, arg_hash = arg_hash,
                            track_hash = track_hash, lapse = lapse, root = root)


  good_datas <- meta_datas |> purrr::keep(~.x$valid)
  if(length(good_datas)==0) {
    if(prevent) {
      if(!quiet)
        cli::cli_alert_warning("Pas de donn\u00e9es en cache et pas d'ex\u00e9cution")
      return(NULL)
    }
    our_data <- exec_source(src, exec_wd, args)
    if(our_data$ok) {
      our_data$lapse <- lapse
      our_data$src <- relname
      our_data$src_hash <- src_hash
      our_data$qmd_file <- new_qmds
      our_data$arg_hash <- arg_hash
      our_data$track_hash <- track_hash
      our_data$track <- track
      our_data$wd <- wd
      our_data$src_in <- src_in
      our_data$ok <- "exec"
      our_data$root <- fs::path_rel(root, paths$project_path)

      our_data <- cache_data(our_data, cache_rep = full_cache_rep, name = basename, uid = uid, root = root)
      if(!quiet)
        cli::cli_alert_warning("Ex\u00e9cution du source")

      if(metadata) {
        return(our_data)
      } else {
        return(our_data$data)
      }
    } else {
      return(NULL)
    }
  }

  dates <- purrr::map(good_datas, "date") |>
    unlist() |>
    lubridate::as_datetime()
  mdd <- which.max(dates)
  good_good_data <- good_datas[[mdd]]
  fnm <- names(good_datas)[[mdd]]
  fnd <- fs::path_join(c(root, good_good_data$data_file))

  if(!quiet)
    cli::cli_alert_warning("M\u00e9tadonnées lues dans {.file {fnd}}")

  ggd_lapse <- good_good_data$lapse %||% "never"
  ggd_wd <- good_good_data$wd %||% "file"
  ggd_qmds <- setequal(good_good_data$qmd_file, new_qmds)
  ggd_track <- setequal(good_good_data$track, track)
  ggd_src_in <- src_in == good_good_data$src_in %||% "project"

  if(ggd_lapse != lapse | ggd_wd != wd | !ggd_qmds | !ggd_track | !ggd_src_in) {
    newmdata <- good_good_data
    newmdata$file <- NULL
    newmdata$lapse <- lapse
    newmdata$wd <- wd
    newmdata$qmd_file <- new_qmds
    newmdata$track <- track
    newmdata$src_in <- src_in
    jsonlite::write_json(newmdata, path = fnm)
  }
  if(!quiet)
    cli::cli_alert_warning("Donn\u00e9es en cache")

  good_good_data$ok <- "cache"
  good_good_data$data <- qs::qread(fnd)
  if(metadata) {
    return(good_good_data)
  } else {
    return(good_good_data$data)
  }
}
