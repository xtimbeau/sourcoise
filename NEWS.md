## sourcoise 0.3.0

* ajout de `sourcoise_reset()` qui est efface tout (enfin, tout ce que `sourcoise` fabrique).
* `sourcoise_clear()` est plus prudent et plus efficace.
* `sourcoise_status()` est plus robuste et nettoie les data_file orphelins.
* `{qs}` est remplacé par `{qs2}`, ce qui corrige le bug de `{gt}` (-;)

## sourcoise 0.2.0

* `sourcoise()` a son propre package `{sourcoise}`, qui est importé dans `{ofce}`.
* `sourcoise()` a un système de log (par `{logger}`)
* `sourcoise()` a un système de prune
* `sourcoise()` a une limite la taille des données sauvegardées

plus quelques bugs réparés.

## sourcoise 0.1.0

`sourcoise()` sort du package `{ofce}` et devient indépendant. Il change de nom (anciennement `source_data()` qui reste un alias pour la compatibilité descendante)
