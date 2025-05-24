# sourcoise 0.6.1

# sourcoise 0.6.0

## Correction de bugs

* enregistre correctement le champ date (quand cache est retourné).
* `ignore.case` dans les recherches de fichiers.
* enregistre correctement le chemin du log si le cache est invalide.
* les arguments sont employés (par un *hash*) pour différencier les caches, ce qui permet de les utiliser sans problème.
* le log est maintenant spécifique à chaque utilisateur.
* cas où le nom de fichier comporte un "." (ne considère pas ça comme une extension).
* cas où il n'y a pas de projet (root=wd dans ce cas) et `sourcoise()` fonctionne (merci François).
* nommage correct du fichier si il n'y a pas d'arguments.
* différencie bien les scripts selon les arguments lors de `status` ou `refresh`
* vérifie que le json est correctement formé à la lecture (merci Elliot)
* force l'encodage à être selon l'otion `sourcoise.encoding` et à être `UTF-8` pour foncitonner sur le multiplateforme avec macOS et windows

## ajouts

* le paramètre `priority` permet de controller l'ordre d'exécution dans le cas d'un refresh, afin de traiter les cascades d'exécution.
* simplification des paramètres de `sourcoise()`. Les paramètres enlevés sont fixés globalement par `options()`.
* la fonction `set_sourcoise_root()` permet de fixer la racine de `sourcoise`.
* la fonction `sourcoise_meta()` renvoie les métadonnées directement (sans les datas donc).
* introduit une nouvelle convention d'accès aux fichiers sources (sera étendue pour les autres) : "/chemin/src" part de la racine du projet systématiquement, "src.r" cherche dans le dossier de l'appelant

## autres

* tests inclus, `{covr}` implementé.

# sourcoise 0.5.0

* CRAN version

# sourcoise 0.4.0

* CRAN version

# sourcoise 0.3.3

## Correction de bugs

* traite correctement les sources multiples et trouve le plus proche mieux.

### Ajouts

* utilise `{memoise}` pour cacher en mémoire les données sur disque

# sourcoise 0.3.2

## Correction de bugs

* retourne un cache invalide si l'éxécution a échouée.
* n'exécute plus deux fois le source lorsque l'éxécution échoue.
* meilleurs messages d'erreur ou de succès

# sourcoise 0.3.1

## Correction de bugs

* fonctionne correctement avec `src_in="file"` qui enregistre le cache au niveau du script R sourcé

# sourcoise 0.3.0

## Ajouts

* `sourcoise_reset()` qui est efface tout (enfin, tout ce que `sourcoise` fabrique).

## Améliorations

* `sourcoise_clear()` est plus prudent et plus efficace.
* `sourcoise_status()` est plus robuste et nettoie les data_file orphelins.
* `{qs}` est remplacé par `{qs2}`, ce qui corrige le bug de `{gt}` (-;) 
* le cache est enregistré dans .sourcoise (ainsi que le log)

# sourcoise 0.2.0

## Ajouts

* `sourcoise()` a son propre package `{sourcoise}`, qui est importé dans `{ofce}`.
* `sourcoise()` a un système de log (par `{logger}`)
* `sourcoise()` a un système de prune
* `sourcoise()` a une limite la taille des données sauvegardées

## Correction de bugs

plus quelques bugs réparés.

# sourcoise 0.1.0

`sourcoise()` sort du package `{ofce}` et devient indépendant. Il change de nom (anciennement `source_data()` qui reste un alias pour la compatibilité descendante)
