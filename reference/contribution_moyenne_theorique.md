# Calculer la contribution moyenne théorique

Certains manuels recommandent de calculer la contribution moyenne
théorique pour s'en servir comme seuil afin de déterminer quelles sont
les variables à prendre en compte dans l'analyse. Cette fonction permet
de calculer cette contribution moyenne théorique : elle divise 100 % par
le nombre de modalité active.

## Utilisation

``` r
contribution_moyenne_theorique(resultat_acm)
```

## Arguments

- resultat_acm:

  Le résultat d'une ACM.

## Valeur de retour

Un nombre entre 0 et 100 qui indique le pourcentage de contribution
moyenne théorique.

## Exemples

``` r
resultat_acm <- createurs_de_contenus_pol |>
  FactoMineR::MCA(quali.sup = c("titre", "personne", "orientation"))

contribution_moyenne_theorique(resultat_acm)
#> ℹ Si toutes les modalités actives contribuaient également à la construction
#> d'un axe, chacune aurait une contribution de 2.04 %.
#> [1] 2.040816

```
