# Extraire les résultats d'une classification ascendante hiérarchique

Lorsque l'on réalise une classification ascendante hiérarchique, et que
l'on souhaite utiliser les résultats, on a souvent envie d'avoir ces
résultats acollés à notre tableau d'origine.

## Usage

``` r
extraire_classes(resultat_classification, ...)
```

## Arguments

- resultat_classification:

  Un objet qui contient le résultat d'une classification réalisée par
  [`FactoMineR::HCPC()`](https://rdrr.io/pkg/FactoMineR/man/HCPC.html).

- ...:

  Colonnes placées en début de tableau. Argument inutile.

## Value

Le tableau de donnée initial utilisé pour l'ACM auquel a été rajouté une
colonne `classe` qui renseigne la classe des individus. Les colonnes de
résultat de l'ACM telle que décrite dans
[`extraire_individus()`](https://nicolasville.github.io/ggacm/reference/extraire_individus.md)
sont également incluses.

## Details

Cette fonction permet d'extraire d'un résultat d'une classification
réalisé avec
[`FactoMineR::HCPC()`](https://rdrr.io/pkg/FactoMineR/man/HCPC.html) le
tableau de données d'origines enrichi d'une colonne qui précise à quelle
classe appartient chaque individus.

Le jeu de données comporte également les colonnes des résultats de
l'ACM. Pour plus de détails sur ces colonnes, voir
[`extraire_individus()`](https://nicolasville.github.io/ggacm/reference/extraire_individus.md).

Le principal intérêt de cette fonction est de permettre de générer un
tableau prêt à être utilisé par
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
pour en faire un graphique.

## Examples

``` r

# 1. On réalise une ACM avec ACM() ou FactoMineR::MCA()
resultat_acm <- createurs_de_contenus_pol |> ACM()
#> ℹ Réalisation de l'ACM avec `FactoMineR::MCA()`
#> ✔ L'ACM a été réalisé avec succès avec `FactoMineR::MCA()` [27ms]
#> 
#> ℹ 
#> • Sur 56 individus...
#> ℹ 
#> • et 24 variables.
#> ℹ 
#> • Toutes les variables sont actives
#> ℹ Vous pouvez préciser des variables supplémentaires avec l'argument `variables
#>   supplementaires`.
#> [53ms]
#> 

# 2. On fait la classification sur le résultat de l'ACM
resultat_classification <- resultat_acm |> FactoMineR::HCPC(nb.clust = 2)

# 3. On peut extraire du résultat de la classification les classes.
resultat_classification |> extraire_classes()
#> # A tibble: 56 × 40
#>    classe dim1_coord dim2_coord personne    titre orientation orientation2 debut
#>    <fct>       <dbl>      <dbl> <chr>       <chr> <chr>       <chr>        <chr>
#>  1 1          -0.278    -0.200  Moufette    Mouf… De gauche   Progressiste 2020…
#>  2 1          -0.451     0.270  Marine Pér… Mari… De gauche   Progressiste 2015…
#>  3 1          -0.119     0.466  Arnaud Gan… Stup… De gauche   Progressiste 2015…
#>  4 1          -0.176     0.275  Anaïs Gare… Modi… De gauche   Progressiste 2015…
#>  5 1          -0.596    -0.363  Jean Massi… Jean… De gauche   Progressiste 2015…
#>  6 2           0.364     0.130  Mathieu Bu… math… De gauche   Progressiste 2020…
#>  7 2           0.624    -0.368  Ribo        Ribo… De gauche   Progressiste 2020…
#>  8 1          -0.129     0.0880 Seumboy Vr… Hist… De gauche   Progressiste 2020…
#>  9 1          -0.602     0.252  Charlélie … DANY… De gauche   Progressiste 2020…
#> 10 1          -0.252     1.00   Cassandre … Cass… De gauche   Progressiste 2020…
#> # ℹ 46 more rows
#> # ℹ 32 more variables: visage_decouvert <chr>, abonnes <chr>, genre <chr>,
#> #   age <chr>, streamer_plein_temps <chr>, collectif <chr>, livre <chr>,
#> #   podcast <chr>, twitter <chr>, facebook <chr>, instagram <chr>,
#> #   tiktok <chr>, discord <chr>, twitch <chr>, site_perso <chr>, CNC <chr>,
#> #   soutenir <chr>, partenariat_entreprise <chr>, autres_services <chr>,
#> #   dim3_coord <dbl>, dim4_coord <dbl>, dim5_coord <dbl>, dim1_contrib <dbl>, …
```
