# classification

``` r

library(ggacm)
```

Cette page servira à mettre en oeuvre une classification ascendante
hiérarchique sur les résultats de votre ACM.

Pour créer la classification, utilisez la fonction `FactoMineR:HCPC()`
sur les résultats de votre ACM. Le paramètre `nb.clust` détermine le
nombre de classe que vous souhaitez garder. Dans cette exemple, je vais
en garder 3. Si vous souhaitez que R choisisse automatiquement le nombre
de classe, indiquez -1.

``` r
# on commence par créer son acm en précisant les variables 
# supplémentaires
resultat_acm <- createurs_de_contenus_pol |> 
  ACM(variables_supplementaires = c(titre, personne, orientation))
#> ℹ Réalisation de l'ACM avec `FactoMineR::MCA()`
#> ✔ L'ACM a été réalisé avec succès avec `FactoMineR::MCA()` [69ms]
#> 
#> ℹ
#> • Sur 56 individus...
#> ℹ 
• et 24 variables.
#> ℹ 
#> ℹ 
── Variables actives : 21 
#> ℹ 
`orientation2`, `debut`, `visage_decouvert`, `abonnes`, `genre`, `age`,
#> `streamer_plein_temps`, `collectif`, `livre`, `podcast`, `twitter`, `facebook`,
#> `instagram`, `tiktok`, `discord`, `twitch`, `site_perso`, `CNC`, …,
#> `partenariat_entreprise`, and `autres_services`.
#> ℹ 
── Variables supplémentaires : 3 
#> ℹ 
`titre`, `personne`, and `orientation`.[78ms]

# puis on peut faire la classification
resultat_classification <- resultat_acm |> 
  FactoMineR::HCPC(nb.clust = 3)
```

## Décrire les classes

Pour décrire les classes, on peut produire un tableau récapitulatif en
utilisant la fonction
[`caracteriser_classe()`](https://nicolasville.github.io/ggacm/reference/caracteriser_classe.md).
L’argument `numero_de_la_classe` indique la classe que vous voulez
décrire.

``` r

resultat_classification |> 
  caracteriser_classe(numero_de_la_classe = 1)
```

[TABLE]

Vous pouvez répéter cela avec les autres classes.

``` r

resultat_classification |> 
  caracteriser_classe(numero_de_la_classe = 2)
```

[TABLE]
