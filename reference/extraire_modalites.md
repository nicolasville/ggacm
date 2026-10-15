# Obtenir les résultats d'une ACM pour chaque modalités.

Cette fonction permet de renvoyer un

## Usage

``` r
extraire_modalites(resultat_acm)
```

## Arguments

- resultat_acm:

  Une résultat d'ACM sortie de
  [`FactoMineR::MCA()`](https://rdrr.io/pkg/FactoMineR/man/MCA.html) ou
  [`ACM()`](https://nicolasville.github.io/ggacm/reference/ACM.md).

- data_acm:

  le tableau de donnée d'origine

## Value

Un tableau avec pour chaque modalité les coordonnées, les contributions,
les cos2 et les vtest pour les 5 premières dimensions.

Dans le détail, le tableau comporte les variables :

- `type` : variables actives ou supplémentaires ?

- `variable` : Le nom de la variable.

- `modalites` : la modalité en question.

- `n` : Le nombre d'individus au sein du tableau de données qui
  partagent cette modalité.

- `pourcentage` : Par des individus au sein du tableau de données qui
  partagent cette modalité

- `dim1_coord`, `dim2_coord`, ... : Coordonnée de la modalité sur l'axe
  1, 2, ...

- `dim1_contrib`, `dim2_contrib`,... : Contribution de chaque modalité à
  la contruction des axes sur l'axe 1, 2, ...

- `dim1_cos2`, `dim2_cos2`, ... : Cosinus carré des modalités sur l'axe
  1, 2,...

- `dim1_vtest`, `dim2_vtest`, ... : Vtest des coordonnées de la modalité
  sur l'axe 1, 2,...

## Examples

``` r
# on commence par créer un résultat d'ACM :
resultat <- createurs_de_contenus_pol |>
  ACM(variables_supplementaires = personne)
#> ℹ Réalisation de l'ACM avec `FactoMineR::MCA()`
#> ✔ L'ACM a été réalisé avec succès avec `FactoMineR::MCA()` [15ms]
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
#> [24ms]
#> 

# Puis on peut en extraire le tableau descriptif de chaque modalité :
resultat |> extraire_modalites()
#> # A tibble: 165 × 21
#>    type    variables modalites     n pourcentage dim1_coord dim1_cos2 dim1_vtest
#>    <chr>   <chr>     <chr>     <int>       <dbl>      <dbl>     <dbl>      <dbl>
#>  1 variab… titre     Antoine …     1         1.8     -0.530  0.00511      -0.530
#>  2 variab… titre     BLAST, L…     1         1.8     -0.861  0.0135       -0.861
#>  3 variab… titre     Bolchege…     1         1.8     -0.228  0.000944     -0.228
#>  4 variab… titre     Canard R…     1         1.8     -0.337  0.00206      -0.337
#>  5 variab… titre     Cass And…     1         1.8     -1.14   0.0235       -1.14 
#>  6 variab… titre     Cercle A…     1         1.8      0.695  0.00879       0.695
#>  7 variab… titre     Christop…     1         1.8      1.19   0.0258        1.19 
#>  8 variab… titre     Clément …     1         1.8     -0.247  0.00111      -0.247
#>  9 variab… titre     DANY et …     1         1.8     -1.15   0.0240       -1.15 
#> 10 variab… titre     Data Gue…     1         1.8     -0.481  0.00422      -0.481
#> # ℹ 155 more rows
#> # ℹ 13 more variables: dim1_contrib <dbl>, dim2_coord <dbl>, dim2_cos2 <dbl>,
#> #   dim2_vtest <dbl>, dim2_contrib <dbl>, dim3_coord <dbl>, dim3_cos2 <dbl>,
#> #   dim3_vtest <dbl>, dim3_contrib <dbl>, dim4_coord <dbl>, dim4_cos2 <dbl>,
#> #   dim4_vtest <dbl>, dim4_contrib <dbl>


```
