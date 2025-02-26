# mise en cache avec sourcoise

`{sourcoise}` est un package qui fournit des outils pour exécuter un script R et mettre en cache les résultats. Le but est de pouvoir exécuter très rapidement un code qui accède à des fichiers ou une API et qui, en l'absence de mises à jour, produit toujours le même résultat. Lorsque l'API est suceptible de bloquer (ou si on a pas de connection internet), cela évite de bloquer le rendu d'un document ou d'un site quarto.

Accessoirement, cela oblige à isoler le code du script qui récupère les données dans un fichier afin d'améliorer la reproductibilité. `sourcoise()` peut être appelé dans un `sourcoise()` ce qui permet la modularité. Il fournit des outils pour vérifier le cache et le rafraichir à la demande.

## utilisation

La structure est donc :

-   un projet R ou quarto.

-   des qmd, appelant `sourcoise()` pour l'acquisition des données et produisant tableaux ou graphiques à partir de ces données.

-   des scripts `R`, dans le même dossier ou ailleurs, qui produisent les données à partir de calculs, d'interrogation de bases externes ou d'API. Des objets plus complexes peuvent être renvoyés, comme par exemple des listes d'objets, des graphiques, des tableaux, des fonctions fabriquant des tableaux ou des graphiques, etc...

-   lorsqu'un cache est disponible et valide (voir plus bas) il est utilisé et la fonciton répond très rapidement (0.01 secondes) suivant la taille des données.

-   tout cela est conçu pour fonctionner avec *github* et donc partager le cache entre utilisateurs d'un même dépot. On peut donc mettre à jour les données sur une machine et les utiliser sur une autre.

Par exemple, si le script `R` `prix_insee.r` utilise l'API de l'INSEE pour télécharger l'indice des prix à la consommation, et si il se termine par l'instruction `return(ipc)` alors sourcoise("prix_insee.r") renvoie toujours les données correspondantes, et si elles sont en cache, le retour est très rapide et ne nécessite pas d'accès à internet.

```{r}
library(insee)

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

```{r}
ipc <- sourcoise("prix_insee.r")
ggplot(ipc$ipch) + ...
```

Le stockage des données a une faible empreinte disque (elles ne servent qu'à construire un graphique, il y a donc une série ou deux, trois dans cet exemple), ce qui ne pose pas de problème pour *github*. Si l'API de l'INSEE est en panne, alors le cache sera utilisé. On peut réutiliser cette instruction de nombreuses fois, puisqu'elle ne sera exécuté réellement qu'une fois et que les autres fois, c'est le cache qui est utilisé.

## par rapport à *memoise*

Par rapport à `memoise::memoise()`, `sourcoise()` utilise systématiquement un cache, localisé dans le projet R ou quarto et destiné à être synchronisé par github. Il s'applique à un script et non à une fonction et utilise des règles d'invalidation du cache légèrement différentes :

-   le cache est conçu principalement pour être passé entre session. Il est utilisé lors du rendu d'un `.qmd` et il est transportable avec les fichiers associés.
-   le cache est invalidé si le script est modifié (similaire à l'invalidation par le corps de la fonction dans `memoise::memoise()`)
-   le cache est invalidé en fonction du delai entre deux exécutions. Il est possible de ré-exécuter le code si il n'a pas été exécuté depuis une heure, une journée, une semaine, etc...
-   le cache est invalidé si un ou plusieurs fichiers (définis au préalable) ont été modifiés. Cela sert en particulier à invalider le cache si un fichier de données (`.csv` ou `.xls`) a été modifié.
-   le cache est invalidé si les arguments passés à `sourcoise()` pour le script ont été modifiés (comme dans `memoise::memoise()`)
-   on peut également forcer l'invalidation du cache par un paramètre passé à la fonction, éventuellement un paramètre global ou en utilisant la fonction `sourcoise_refresh()`. Ce dernier point est une différence important par rapport à `memoise::memoise()` et permet une exécution régulière du rafraichissement du cache.

## autres fonctionalités

`sourcoise()` peut également invalider les *freeze* employés par quarto. Les qmd appelant le script sont enregistrés avec le cache et lorsque celui-ci est rafraichit, alors le `qmd` est *unfreezé*, ce qui assure que le qmd sera bien re rendu avec les données mises à jour.

`sourcoise()` conserve l'histoirique des données téléchargées et permet donc théoriquement d'y acceder.

`sourcoise()` utilise une heuristique pour trouver la racine du projet, et localiser le script R qui est appelé. cela permet la transportabilité des scripts à l'intérieur d'un projet. Par exemple, si on utilise

## à venir

Seront bientôt implémentés :

-   un log optionel qui enregistre les mises à jour des données et les accès aux fichiers de données.

-   la possibiité de stocker les données cachées hors de *github* et d'utiliser `{pins}` pour les partager.

-   la possibilité de limiter la taille du cache et l'historique des données.

-   et éventuellement une interface *shiny* de mise à jour (*gui* pour `sourcoise_refresh()`)
