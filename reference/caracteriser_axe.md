# Caractériser ses axe La fonction suivante permet de sortir un tableau résumant pour un axe les modalités les plus contribuantes à sa construction.

Caractériser ses axe La fonction suivante permet de sortir un tableau
résumant pour un axe les modalités les plus contribuantes à sa
construction.

## Usage

``` r
caracteriser_axe(resultat_acm, axe, contribution_minimum)
```

## Arguments

- axe:

  Numéro de l'axe à décrire. (1, 2, 3, ...)

- resultat_modalites:

  Tableau de donnée reprennant l'extraction des résultats d'une ACM par
  modalité

- seuil:

  Seuil de la contribution à l'axe en dessous duquel les modalités ne
  seront pas prises en compte. Vaut 0 par défaut, mais il peut être
  utile de l'augmenter si l'on a beaucoup de modalité et que l'on ne
  souhaite pas qu'elles apparaissent toutes dans le tableau.

## Value

Un tableau de donnée prêt à être publié. Il s'agit d'une objet venant du
package gt.

## Examples

``` r
#
```
