# Visualiser les positions des individus en fonction des modalités

Cette fonction permet de faire apparaître plusieurs fois le nuage
d'individu en colorant à chaque fois les individus qui partagent une
même modalité.

## Utilisation

``` r
nuage_individus_facette(
  resultat_individus,
  variables_a_representer,
  forme = "square",
  couleur = "#e41a1c",
  longueur_max_labels = 20,
  taille = 2,
  ...
)
```

## Arguments

- resultat_individus:

  Un tableau de donnée avec les coordonnées des individus qui a été
  produit par
  [`extraire_individus()`](https://nicolasville.github.io/ggacm/reference/extraire_individus.md).
  La fonction prend également en charge un résultat d'ACM directement si
  besoin.

- variables_a_representer:

  La liste des variables à représenter : on si on souhaite sélectionner
  le `genre`, la `classe` et le `niveau_de_revenu` on peut par exemple
  écire `c(genre, classe, niveau_de_revenu)`

- forme:

  Quelles formes pour les individus qui partagent une même modalité ?
  Par défaut un carré ("square"). Voir la [documentation de
  ggplot](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html#point)
  sur les "shapes" pour les possibilités.

- couleur:

  Quelle couleur doit-être utilisée ? doit-être une chaiîne de caractère
  qui encode la couleur comme "#e41ac" ou "yellow" par exemple.

- longueur_max_labels:

  Longeur maximum des labels de chaque facette. Par défaut chaque label
  ira à la ligne au bout de 20 caractères.

- ...:

  Arguments supplémentaires pour les utilisateurs avancés qui seront
  passés à
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

## Valeur de retour

Un objet ggplot2.

## Exemples

``` r

res.ACM <- createurs_de_contenus_pol |> ACM()
#> ℹ Réalisation de l'ACM avec `FactoMineR::MCA()`
#> ✔ L'ACM a été réalisé avec succès avec `FactoMineR::MCA()` [23ms]
#> 
#> ℹ 
#> • Sur 56 individus...
#> ℹ 
#> • et 24 variables.
#> ℹ 
#> • Toutes les variables sont actives
#> ℹ Vous pouvez préciser des variables supplémentaires avec l'argument `variables
#>   supplementaires`.
#> [45ms]
#> 

res.ind <- res.ACM |> extraire_individus()

res.ind |>
  nuage_individus_facette(
    variables_a_representer = c(CNC, partenariat_entreprise)
   )
```
