# sourcoise <a href="https://xtimbeau.github.io/sourcoise/"><img src="man/figures/logo.png" align="right" height="102" alt="sourcoise website" /></a>

`{sourcoise}` est un package qui fournit des outils pour exécuter un script R et mettre en cache les résultats. Le but est de pouvoir exécuter très rapidement un code qui accède à des fichiers ou une API et qui, en l'absence de mises à jour, produit toujours le même résultat. Lorsque l'API est suceptible de bloquer (ou si on a pas de connection internet), cela évite de bloquer le rendu d'un document ou d'un site quarto.

Accessoirement, cela oblige à isoler le code du script qui récupère les données dans un fichier afin d'améliorer la reproductibilité. `sourcoise()` peut être appelé dans un `sourcoise()` ce qui permet la modularité. Il fournit des outils pour vérifier le cache et le rafraichir à la demande.

## installation

Pour installer `{sourcoise}` il faut le faire depuis *github* :

```r
devtools::install_gitub("xtimbeau/sourcoise")

# alternativement
pak::pak("xtimbeau/sourcoise")
```

## utilisation

La structure est donc :

-   un projet R ou quarto.

-   des qmd, appelant `sourcoise()` pour l'acquisition des données et produisant tableaux ou graphiques à partir de ces données.

-   des scripts `R`, dans le même dossier ou ailleurs, qui produisent les données à partir de calculs, d'interrogation de bases externes ou d'API. Des objets plus complexes peuvent être renvoyés, comme par exemple des listes d'objets, des graphiques, des tableaux, des fonctions fabriquant des tableaux ou des graphiques, etc...

-   lorsqu'un cache est disponible et valide (voir plus bas) il est utilisé et la fonction répond très rapidement suivant la taille des données (0.006 secondes pour des données de 2Mb).

-   tout cela est conçu pour fonctionner avec *github* et donc partager le cache entre utilisateurs d'un même dépot. On peut donc mettre à jour les données sur une machine et les utiliser sur une autre.

-   une dernière chose, `sourcoise()` est doté d'une heuristique maline trouve le fichier source même si il est caché (i.e. que le chemin est approximatif, ce qui déclenche une erreur normalement, mais là ça passe), ce qui augmente la portabilité des fichiers sources et facilite l'orgnisation d'un projet. Bien sûr, en cas d'ambiguité, `sourcoise()` prévient.

## un exemple

Par exemple, si le script `R` `prix_insee.r` utilise l'API de l'INSEE pour télécharger l'indice des prix à la consommation, et si il se termine par l'instruction `return(ipc)`, alors `sourcoise("prix_insee.r")` renvoie toujours les données correspondantes, et si elles sont en cache, le retour est très rapide et ne nécessite pas d'accès à internet.

```r
library(insee)
library(tidyverse)

ipchm <- get_idbank_list("IPCH-2015") |>
     filter(COICOP2016=="00", FREQ=="M", NATURE=="INDICE") |> 
     pull(idbank) |>
     get_insee_idbank() |>
     select(DATE, ipch = OBS_VALUE, IDBANK)

ipch <- ipchm |>
     mutate(DATE = floor_date(DATE, unit="quarter")) |>
     group_by(DATE) |>
     summarise(ipch = mean(ipch))

ipcha <- ipch |> 
     mutate(y = year(DATE)) |> 
     group_by(y) |>
     summarize(ipch = mean(ipch)) |> 
     mutate(ipch = ipch / ipch[y == 2023])

return(list(ipcha = ipcha, ipchm = ipchm, ipch = ipch))

```

Dans le `qmd` on a alors un chunk `r` :

```r
library(sourcoise)
ipc <- sourcoise("prix_insee.r")
ggplot(ipc$ipch) + ...
```

Le stockage des données a une faible empreinte disque (elles ne servent qu'à construire un graphique, il y a donc une série ou deux, trois dans cet exemple), ce qui ne pose pas de problème pour *github*. Si l'API de l'INSEE est en panne, alors le cache sera utilisé. On peut réutiliser cette instruction de nombreuses fois, puisqu'elle ne sera exécuté réellement qu'une fois et que les autres fois, c'est le cache qui est utilisé.

`sourcoise()` exécute le script en local, ce qui limite les effets de bord.

## par rapport à *memoise*

`memoise::memoise()` propose une solution assez proche, avec la possibilité de rendre le cache persistant entre sessions.

Mais `memoise()` répond au besoin de l'évaluation d'une fonction, qui pour un même jeu de paramètres renvoie toujours la même chose. Ce qui est la définition d'une fonction dans la paradigme fonctionnel sans effet de bord. Le cache permet alors d'échanger espace disque contre performance. 

`sourcoise()` part d'une hypothèse différente. Le bout de code appelé ressemble à une fonction mais n'en est pas une : les mêmes arguments peuvent renvoyer une valeur différente. C'est le cas notamment lors de l'accès à une API à des données qui renvoie souvent la même chose, mais périodiquement propose une nouvelle version, avec un effet de bord. Le temps d'accès à l'API eut être long et évebntuellement l'accès hasardeux. Or les appels à l'API peuvent être dans un workflow typique quarto/R très fréquent (à chaque rendu par exemple), `sourcoise()` permet donc de mettre en cache et de déclencher une "vraie" exécution périodiquement mais pas tgrop souvent. Si l'appel à l'API est très long, le gain en performance peut être considérable et si l'API bloque parfois, alors `sourcoise()` permet de continuer le reste du flow sans interruption. L'exécution de l'API peut ainsi être asynchrone, dans un process différent ou sur une machine différente, la synchronisation étant assurée par 
github ou ``{pins}`.

Par rapport à `memoise::memoise()`, `sourcoise()` utilise systématiquement un cache sur disque, persistant, localisé dans le projet R ou quarto et destiné à être synchronisé par *github*. Il s'applique à un script et non à une fonction et utilise des règles d'invalidation du cache différentes :

-   le cache est conçu principalement pour être passé entre session. Il est utilisé lors du rendu d'un `.qmd` et il est transportable avec les fichiers associés.
-   le cache est invalidé si le script est modifié (similaire à l'invalidation par le corps de la fonction dans `memoise::memoise()`).
-   le cache est invalidé en fonction du delai entre deux exécutions. Il est possible de ré-exécuter le code si il n'a pas été exécuté depuis une heure, une journée, une semaine, etc...
-   le cache est invalidé si un ou plusieurs fichiers (définis au préalable) ont été modifiés. Cela sert en particulier à invalider le cache si un fichier de données (`.csv` ou `.xls`) a été modifié.
-   le cache est invalidé si les arguments passés à `sourcoise()` pour le script ont été modifiés (comme dans `memoise::memoise()`).
-   on peut également forcer l'invalidation du cache par un paramètre passé à la fonction, éventuellement un paramètre global ou en utilisant la fonction `sourcoise_refresh()`. Ce dernier point est une différence important par rapport à `memoise::memoise()` et permet une exécution régulière du rafraichissement du cache.
-   on peut *logger* les accès à `sourcoise()` ce qui permet de comprendre pourquoi le cache n'est pas invalidé et quels sont fichiers qui ont déclenché `sourcoise()`. Un dossier `.logs` est ajouté au dossier du projet.
-   on peut limiter la taille du cache, par le paramètre `grow_cache` qui contraint l'historique du cache et par `limit_mb` qui empêche de mettre en cache des données au delà d'une taille limite ; par défaut 50mb, pour ne pas fâcher *github*.

## autres fonctionalités

`sourcoise()` peut également invalider les *freeze* employés par quarto. Les qmd appelant le script sont enregistrés avec le cache et lorsque celui-ci est rafraichit, alors le `qmd` est *unfreezé*, ce qui assure que le qmd sera bien re rendu avec les données mises à jour.

`sourcoise()` conserve l'histoirique des données téléchargées et permet donc théoriquement d'y acceder.

`sourcoise()` utilise une heuristique pour trouver la racine du projet, et localiser le script R qui est appelé. cela permet la transportabilité des scripts à l'intérieur d'un projet.

## à venir

Seront bientôt implémentés :

-   la possibiité de stocker les données cachées hors de *github* et d'utiliser `{pins}` pour les partager (mais au prix d'un accès plus lent peut être).

-   et éventuellement une interface *shiny* de mise à jour (*gui* pour `sourcoise_refresh()`)
