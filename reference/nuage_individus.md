# Visualiser rapidement le nuage d'individus

On veut souvent visualiser rapidement un nuage d'individus sans trop y
passer du temps. Cette fonction est un raccourci à cela, elle permet de
faire

## Usage

``` r
nuage_individus(resultat_individus, couleur)
```

## Arguments

- couleur:

  on peut optionnellement coloré le nuage d'individus en fonction d'une
  variable.

- resultat_acm:

  Le résultat d'une acm

## Value

un objet ggplot

## Examples

``` r

# on commence par créer un résultat d'ACM :
resultat <- createurs_de_contenus_pol |>
  ACM(variables_supplementaires = personne)
#> ℹ Réalisation de l'ACM avec `FactoMineR::MCA()`
#> ✔ L'ACM a été réalisé avec succès avec `FactoMineR::MCA()` [33ms]
#> 
#> ℹ 
#> • Sur 56 individus...
#> ℹ 
#> • et 24 variables.
#> ℹ 
#> 
#> ℹ 
#> ── Variables actives : 23 
#> ℹ 
#> `titre`, `orientation`, `orientation2`, `debut`, `visage_decouvert`, `abonnes`,
#> `genre`, `age`, `streamer_plein_temps`, `collectif`, `livre`, `podcast`,
#> `twitter`, `facebook`, `instagram`, `tiktok`, `discord`, `twitch`, …,
#> `partenariat_entreprise`, and `autres_services`.
#> 
#> ℹ 
#> ── Variables supplémentaires : 1 
#> ℹ 
#> `personne`.
#> [44ms]
#> 

# puis on peut sortir le graphique
resultat |>
  nuage_individus() +
  theme_acm()

```
