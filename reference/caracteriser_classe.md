# Caractériser les classes

La fonction présenter ici permet d'obtenir plus d'information sur une
classification ascendante hiérarchique. Elle produit des tableaux qui
reprennent pour une classe donnée, les caractéristiques typiques et
atypiques dans la classe.

## Usage

``` r
caracteriser_classe(resultat_classification, numero_de_la_classe)
```

## Arguments

- resultat_classification:

  Un résultat de classification réalisé avec
  [`FactoMineR::HCPC()`](https://rdrr.io/pkg/FactoMineR/man/HCPC.html)

- numero_de_la_classe:

  Le numéro de la classe que l'on souhaite caractériser.

## Value

Un tableau de présentation de la classe. Il s'agit d'un objet gt.

## Examples

``` r
#
# une super exemple
```
