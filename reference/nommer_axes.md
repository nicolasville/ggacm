# Nommer les axes d'une représentation graphique d'ACM

Lorsque l'on représente une ACM, il est courant de vouloir donner un nom
aux axes, ou au moins les pôles de chacun de ces axes. Cette fonction
permet de générer les labels pour chaque axe à utiliser lorsqu'on génère
des graphiques axec ggplot

## Usage

``` r
nommer_axes(
  resultat_acm,
  en_haut = "",
  en_bas = "",
  a_gauche = "",
  a_droite = ""
)
```

## Arguments

- resultat_acm:

  le résultat d'une ACM sortie de
  [`MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html)

- en_haut:

  à quoi correspond le haut du graphique ? entre guillemet.

- en_bas:

  idem mais en bas

- a_gauche:

  idem mais à gauche

- a_droite:

  idem mais a droite

## Value

une couche ggplot à ajouter. Sous le capot cette fonction fait génère
des annotations pour graphique ggplot avec
[`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)

## Examples

``` r
#
```
