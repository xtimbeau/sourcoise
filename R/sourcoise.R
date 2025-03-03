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
#' @param cache_rep (character) défaut .sourcoise sauf usage particulier
#' @param log ("OFF" par défaut) niveau de cache (voir `logger::log_treshold()`)
#' @param grow_cache ("Inf" par défaut) stratégie de cache
#' @param limit_mb (50 par défaut) limite le fichier de données à x mb (pour github). Si au dessus de la limite, **pas de cache**.
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
    quiet = FALSE,
    nocache = FALSE,
    log = getOption("sourcoise.log"),
    grow_cache = getOption("sourcoise.grow_cache"),
    limit_mb = getOption("sourcoise.limit_mb")) {

  ctxt <- setup_context(
    path = path,
    root = root,
    src_in = src_in,
    cache_rep = cache_rep,
    exec_wd = exec_wd,
    wd = wd,
    track = track,
    args = args,
    lapse = lapse,
    nocache = nocache,
    grow_cache = grow_cache,
    limit_mb = limit_mb,
    quiet = quiet)

  ctxt <- startup_log(log, ctxt)

  if(is.null(ctxt)) {
    logger::log_error("Impossible de trouver le ficher {path}")
    return(list(error = "file not found", ok = FALSE))
  }

  if(is.null(force_exec)) force <- FALSE else if(force_exec=="TRUE") force <- TRUE else force <- FALSE
  if(is.null(prevent_exec))
    prevent <- FALSE
  else if(prevent_exec=="TRUE")
    prevent <- TRUE
  else
    prevent <- FALSE

  if(force&!prevent) {
    our_data <- exec_source(ctxt)
    if(our_data$ok=="exec") {
      our_data <- cache_data(our_data, ctxt)
      prune_cache(ctxt)
      logger::log_success("exécution forcée et réussie en {round(our_data$timing)} s. ({scales::label_bytes()(our_data$size)})")
      if(metadata) {
        return(our_data)
      } else {
        return(our_data$data)
      }
    } else {
      msg <- "exécution de {ctxt$src} échouée : {our_data$error$message}"
      if(!quiet)
        cli::cli_alert_danger(msg)
      logger::log_error(msg)
    }
  }

  ctxt <- valid_metas(ctxt)

  good_datas <- ctxt$meta_datas |> purrr::keep(~.x$valid)

  if(length(good_datas)==0) {
    if(prevent) {
      logger::log_warn("Pas de données en cache et exécution suspendue, erreur retournée")
      return(list(error = "No cache&prevent"))
    }
    our_data <- exec_source(ctxt)
    if(our_data$ok=="exec") {
      our_data <- cache_data(our_data, ctxt)
      prune_cache(ctxt)
      logger::log_success("Pas de cache valide, exécution réussie ({scales::label_bytes()(our_data$size)})")
      if(metadata) {
        return(our_data)
      } else {
        return(our_data$data)
      }
    } else {
      msg <- "exécution de {ctxt$src} échouée, pas de cache : {our_data$error$message}"
      if(!quiet)
        cli::cli_alert_danger(msg)
      logger::log_error(msg)
      return(our_data)
    }
  }

  one_gooddata <- pick_gooddata(good_datas, ctxt)
  prune_cache(ctxt)
  logger::log_success("Données en cache trouvées ({scales::label_bytes()(one_gooddata$size)})")

  if(metadata) {
    return(one_gooddata)
  } else {
    return(one_gooddata$data)
  }
}
