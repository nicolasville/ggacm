# Obtenir les résultats pour chaque individus

Cette fonction permet quadd on lui donne le résultat d'une ACM de sortir
un tableau qui reprend le tableau de donnée initial en ajoutant pour
chaque individus leur coordonnées.

## Usage

``` r
extraire_individus(resultat_acm, ordre = NULL)
```

## Arguments

- resultat_acm:

  resultat acm

- ordre:

  ne pas toucher.

## Value

Le tableau de données surlequel vous avez réalisé l'ACM auquel a été
ajouté pour chaque individus : leur coordonnées sur chaque axe (stockés
dans des colonnes `dim1_coord`, `dim2_coord`, `dim3_coord`, etc), leur
contribution à la construction des axes (colonnes `dim1_contrib`,
`dim2_contrib`,...) et enfin leur cosinus carré (colonnes
`dim1_cos2`,...)

## Examples

``` r
resultat_acm <- createurs_de_contenus_pol |> ACM()
#> ℹ Réalisation de l'ACM avec `FactoMineR::MCA()`
#> ✔ L'ACM a été réalisé avec succès avec `FactoMineR::MCA()` [14ms]
#> 
#> ℹ 
#> • Sur 56 individus...
#> ℹ 
#> • et 24 variables.
#> ℹ 
#> • Toutes les variables sont actives
#> ℹ Vous pouvez préciser des variables supplémentaires avec l'argument `variables
#>   supplementaires`.
#> [30ms]
#> 

resultat_individus <- resultat_acm |> extraire_individus()

resultat_individus
#> # A tibble: 56 × 39
#>    dim1_coord dim2_coord personne           titre orientation orientation2 debut
#>         <dbl>      <dbl> <chr>              <chr> <chr>       <chr>        <chr>
#>  1     -0.278    -0.200  Antoine Goya       Anto… De gauche   Progressiste 2015…
#>  2     -0.451     0.270  Salomé Saqué       BLAS… De gauche   Progressiste 2020…
#>  3     -0.119     0.466  Benjamin Patinaud  Bolc… De gauche   Progressiste 2014…
#>  4     -0.176     0.275  Aymric Dlavo       Cana… De gauche   Progressiste 2015…
#>  5     -0.596    -0.363  Cassandre Begous   Cass… De gauche   Progressiste 2020…
#>  6      0.364     0.130  Pierre-Yves Rouge… Cerc… De droite   Conservateur 2015…
#>  7      0.624    -0.368  Christopher Lannes Chri… De droite   Conservateur 2014…
#>  8     -0.129     0.0880 Clément Viktorovi… Clém… De gauche   Progressiste 2020…
#>  9     -0.602     0.252  Charlélie Jutan    DANY… De gauche   Progressiste 2020…
#> 10     -0.252     1.00   Julien Goetz       Data… De gauche   Progressiste 2014…
#> # ℹ 46 more rows
#> # ℹ 32 more variables: visage_decouvert <chr>, abonnes <chr>, genre <chr>,
#> #   age <chr>, streamer_plein_temps <chr>, collectif <chr>, livre <chr>,
#> #   podcast <chr>, twitter <chr>, facebook <chr>, instagram <chr>,
#> #   tiktok <chr>, discord <chr>, twitch <chr>, site_perso <chr>, CNC <chr>,
#> #   soutenir <chr>, partenariat_entreprise <chr>, autres_services <chr>,
#> #   dim3_coord <dbl>, dim4_coord <dbl>, dim5_coord <dbl>, dim1_contrib <dbl>, …
```
