print_classe_axe <- function(classification,
                             numero_axe){

  nettoyer <- function(x) {
    as_tibble(x, rownames = "modalites") |>
      mutate(
        modalites = str_remove(modalites, ".+=")
      )
  }

  lien_classe_modalites <- classification$desc.var$category |>
    map(nettoyer)

  lien_classe_axes <- classification$desc.axes$quanti |> map(nettoyer)

  lien_classe_axes |>
    chuck(numero_axe) |>
    mutate(
      Axe = modalites,
      `Coordonnées moyennes` = `Mean in category`|> as_0.01(),
      `Ecart-type` = `sd in category` |> as_0.01(),
      `V-Test` = v.test |> as_0.01(),
      `P-Valeur` = p.value |> as_pvalue(),
      .keep = "none"
    )
}
