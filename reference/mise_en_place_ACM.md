# Mettre en place une session R adapté

R affiche par défaut beaucoup trop de chiffres après la virgule (qui est
d'ailleurs un point en général) et tend à utiliser l'écriture
scientifique... ce qui n'arrange pas vraiment les chercheurs en sciences
sociales... Cette fonction permet de modifier ces options d'un seul
coup. Un nombre qui s'affichait comme 0.456789 s'affichera alors comme
0,457.

## Usage

``` r
mise_en_place_ACM(...)
```

## Arguments

- ...:

  Il est possible de faire passer d'autres arguments à
  [`base::options()`](https://rdrr.io/r/base/options.html).

## Value

Une liste d'option

## Details

Concrètement cette fonction modifie les options de la session R et les
options des objets du package gtsummary en ayant recours à
[`gtsummary::theme_gtsummary_language()`](https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html).

## Examples

``` r

# ne fait pas parti  de l'exemple
old <- options()

# Exemple : ------------------------

# Avant :
c(0.123456, 10e20)
#> [1] 1.23456e-01 1.00000e+21

mise_en_place_ACM()
#> 
#> ── Les options générales de la session R ont été modifié. 
#> ✔ Les nombres décimaux s'afficheront avec une virgule.
#> 
#> ── Le thème des tableaux produit par gtsummary a été modifié. 
#> ✔ Ils s'afficheront en français.
#> ✔ En utilisant une virgule pour les nombres décimaux.

# Après :
c(0.123456, 10e20)
#> [1] 1,23e-01 1,00e+21

# Fin de l'exemple -----------------
options(old)
```
