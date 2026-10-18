# Réaliser une ACM

Cette fonction permet de réaliser une ACM en se reposant sur le package
FactoMineR. Elle sera sûrement plus simple à utiliser que
[`FactoMineR::MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html).

## Utilisation

``` r
ACM(donnees, variables_supplementaires, graph = FALSE, ...)
```

## Arguments

- donnees:

  Les données de l'ACM

- variables_supplementaires:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  La liste des variables supplémentaires. Si vous voulez sélectionner
  plusieurs variables, vous pouvez les combinez par avec
  `c(variable1, variable2)`. Pour plus de détails sur les manières de
  sélectionner des colonnes dans cet argument, voir
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

- graph:

  Faut-il afficher les graphiques produit par
  [`FactoMineR::MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html) ?
  par défaut sur `FALSE`.

- ...:

  Arguments supplémentaires qui seront passé à
  [FactoMineR::MCA](https://rdrr.io/pkg/FactoMineR/man/MCA.html).

## Valeur de retour

Le résultat d'une ACM.

## Exemples

``` r
resultat <- createurs_de_contenus_pol |>
  ACM(variables_supplementaires = c(titre, personne))
#> ℹ Réalisation de l'ACM avec `FactoMineR::MCA()`
#> ✔ L'ACM a été réalisé avec succès avec `FactoMineR::MCA()` [39ms]
#> 
#> ℹ 
#> • Sur 56 individus...
#> ℹ 
#> • et 24 variables.
#> ℹ 
#> 
#> ℹ 
#> ── Variables actives : 22 
#> ℹ 
#> `orientation`, `orientation2`, `debut`, `visage_decouvert`, `abonnes`, `genre`,
#> `age`, `streamer_plein_temps`, `collectif`, `livre`, `podcast`, `twitter`,
#> `facebook`, `instagram`, `tiktok`, `discord`, `twitch`, `site_perso`, …,
#> `partenariat_entreprise`, and `autres_services`.
#> 
#> ℹ 
#> ── Variables supplémentaires : 2 
#> ℹ 
#> `titre` and `personne`.
#> [47ms]
#> 

```
