# sourcoise <a href="https://xtimbeau.github.io/sourcoise/"><img src="man/figures/logo.png" align="right" height="102" alt="sourcoise website" /></a>

`{sourcoise}` est un package qui fournit des outils pour exécuter un script R et mettre en cache les résultats. Le but est de pouvoir exécuter très rapidement un code qui accède à des fichiers ou une API et qui, en l'absence de mises à jour, produit toujours le même résultat. Lorsque l'API est suceptible de bloquer (ou si on a pas de connection internet), cela évite de bloquer le rendu d'un document ou d'un site quarto.

Accessoirement, cela oblige à isoler le code du script qui récupère les données dans un fichier afin d'améliorer la reproductibilité. `sourcoise()` peut être appelé dans un `sourcoise()` ce qui permet la modularité. Il fournit des outils pour vérifier le cache et le rafraichir à la demande.

## Installation

Pour installer `{sourcoise}` il faut le faire depuis *github* :

```r
devtools::install_gitub("xtimbeau/sourcoise")

# alternativement
pak::pak("xtimbeau/sourcoise")
```

## Utilisation

Pour alimenter un graphique ou un tableau en données, on met le code dans un script `r` (`"mon_script.r"`) en terminant le script par un `return(data_pour_le_graphique)`. 
Dans le `.qmd` ou `.rmd` (ou aussi un scirpt R) on a les instructions du graphique :

````qmd
```{r}
library(sourcoise)
mes_datas <- sourcoise("mon_script.r")
ggplot(mes_datas) + instructions du graphique

```
````

A la première exécution le script est exécuté, les appels suivants utiliseront le cache, sauf si le cache est invalidé.

## Bénéfices

Les bénéfices sont nombreux :

1.  un gain de temps lorsque l'exécution du code est longue (accès à une API, téléchargement de grosses données, traitements importants). La lecture d'un fichier excel peut aussi être assez longue. Le temps d'accès aux données en cache dépend de leur taille, mais même pour des données volumineuses (et il n'y a pas de raisons qu'elles le soient tant que ça), l'ordre de grandeur est de qualques millisecondes, grâce aux optimisations.

2.  le cache est transférable par github. Il se trouve dans un dossier (caché), mais enregistré dans le dossier de projet et *commité* par github. Le cache produit sur un poste est donc accessible par `pull` sur les autres postes.

3.  si le code source déclenche une erreur, on peut passer outre : En cas de package non installé, données absentes (par exemple un chemin absolu dans le code), ou une API qui bloque (comme celle de l'OCDE) alors `sourcoise()` essaye de prendre la dernière exécution résussie. Bien que cela puisse être problématique, c'est-à-dire une erreur non signalée, cela a l'énorme avantage de ne pas bloquer le processus et de permettre de traiter l'erreur en parallèle.

4.  `sourcoise()` cherche de façon astucieuse le fichier source dans le projet et exécute le code dans un environnement local, en changeant le répertoire de travail pour être celui où se trouve le code source. Cela permet d'appeler dans le code source (le script `r` `mon_script.r` passé en paramètre à `sourcoise("mon_script.r")`, des scripts `r`, des fichiers de données `.csv` ou `.xlsx` qui sont enregistré dans le même répertoire que le fichier `mon_script.r`. On peut donc réutiliser le code sans se soucier de modifier les chemins qui sont relatifs au dossier où se trouve `mon_script.r`.

5.  cela fournit un embryon de reproductibilité en désignant le script qui fabrique les données.

## A venir

Seront bientôt implémentés :

-   la possibilité de stocker les données cachées hors du dossier de projet (et donc hors de *github*) et d'utiliser `{pins}` pour le stcokage (mais peut être au prix d'un accès plus lent).

-   un schéma pour déclarer les dépendances entre dess appels à `sourcoise()` et déclencher les exécutions en cascade.

-   et éventuellement une interface *shiny* de mise à jour (*gui* pour `sourcoise_refresh()`)
