# Visualiser les résultats d'une ACM

Visualiser les résultats d'une ACM

## Utilisation

``` r
ggacm(data, mapping = aes(dim1_coord, dim2_coord), ...)
```

## Arguments

- data:

  Le résultat d'une ACM après qu'il soit passé par la fonction
  `get_individus()` ou `get_variables()`.

- mapping:

  Par défaut prend les coordonnée de la dimension 1 en abcisse
  (`x = dim1coord`) et de la dimension 2 en ordonné (`y = dim2coord`).
  Il est possible d'utiliser d'autres aesthetics. Pour plus
  d'information voir la vignette et la documentation de ggplot. Cet
  argument doit toujours être écrit avec la fonction
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) comme :
  `aes(x = ..., y = ..., label = ..., etc.)`

- ...:

  Arguments optionnels passés à
  [`coord_acm()`](https://nicolasville.github.io/ggacm/reference/coord_acm.md)
  qui seront passé à
  [`ggplot2::coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html).

## Valeur de retour

un graphique ggplot

## Exemples

``` r
# un exemple pas piqué des annetons
```
