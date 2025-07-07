

#' Caractériser ses axe
#' La fonction suivante permet de sortir un tableau résumant pour un axe les modalités les plus contribuantes à sa construction.
#'
#' @param resultat_modalites Tableau de donnée reprennant l'extraction des résultats d'une ACM par modalité
#' @param axe Numéro de l'axe à décrire. (1, 2, 3, ...)
#' @param seuil Seuil de la contribution à l'axe en dessous duquel les modalités ne seront pas prises en compte. Vaut 0 par défaut, mais il peut être utile de l'augmenter si l'on a beaucoup de modalité et que l'on ne souhaite pas qu'elles apparaissent toutes dans le tableau.
#'
#' @returns Un tableau de donnée prêt à être publié. Il s'agit d'une objet venant du package gt.
#' @export
#'
#' @examples #
caracteriser_axe <- function(resultat_acm, axe, contribution_minimum) {

  if (missing(axe)) {
    cli::cli_abort(c(
      "x" = "L'argument {.arg axe} n'a pas été fourni à la fonction.",
      "i" = "L'avez-vous oublié ?",
      ">" = "Incluez le numéro de l'axe que vous voulez caractériser :",
      " " = "Par exemple : {.code axe = 2}"
    ))
  }

  if (!is.numeric(axe)) {
    cli::cli_abort(c(
      "L'argument {.arg axe} doit être un numéro",
      ">" = "`axe = 1` ou `axe = 2` ou encore `axe = 3`, etc."))
  }

  if (missing(contribution_minimum)) {
    cli::cli_inform(c(
      "Vous n'axez pas indiqué de contribution minimum pour afficher les résultats",
      "Seules les modalités dont la contribution est supérieur à la moyenne théorique seront affichées.",
      "i" = "Si vous souhaitez modifier cela, vous pouvez changer l'argument contribution_minimum",
      ">" = "{.code contribution_minimum = 0} si vous souhaitez afficher toutes les modalités par exemple"
    ))

    cli::cli_h3("Contribution moyenne théorique")
    contribution_minimum <- contribution_moyenne_theorique(resultat_acm)
  }

  resultat_modalites <- extraire_modalites(resultat_acm = resultat_acm)

  # on sort les tableaux pour l'axe en question
  carac_axe <- tableau_axe(resultat_modalites = resultat_modalites, axe = axe, seuil = contribution_minimum)

  # on sépare en deux tableau (un pour les coordonnées positives, l'autre pour les négatives)
  carac_axe_coord_positive <- carac_axe |> filter(Coordonnée > 0) |> as_gtsummary()
  carac_axe_coord_negative <- carac_axe |> filter(Coordonnée < 0) |> as_gtsummary()

  # on combine les deux tableaux
  tableau_sortie <- tbl_stack(
    tbls = list(carac_axe_coord_positive, carac_axe_coord_negative),
    group_header = c("Coordonnées positives", "Coordonnées négatives")
  )

  # enfin du formattage
  tableau_sortie <- tableau_sortie |>
    # colonne Modalités aligné à gauche
    gtsummary::modify_column_alignment(
      columns = Modalité,
      align = "left"
    ) |>
    gtsummary::modify_column_alignment(
      columns = -Modalité,
      align = "right"
    ) |>
    gtsummary::modify_footnote_header(
      footnote = "Moyenne des coordonnées des individus partageant la modalité",
      columns = Coordonnée
    )

  # inclure une note de bas de tableau si ce dernier est filtré
  if(contribution_minimum > 0) {
    tableau_sortie <- tableau_sortie |>
      gtsummary::modify_footnote_header(
        footnote = paste0("Seules les modalités actives dont la contribution est supérieure à ", contribution_minimum |> round(digits = 2), " % sont représentées."),
        columns = Modalité
    )
  }

    # column header bold
  tableau_sortie |>
    gtsummary::as_gt() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(everything())
    ) |>
    # label positif et négatif en gras
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups(groups = everything())
    )
}


# Puis une fonction pour extraire le tableau de données.
tableau_axe <- function(resultat_modalites, axe, seuil) {

  # obtenir la chaine de caractère du nom de l'axe  (dim1/dim2 ou dim3 par ex)
  dim.chr <- paste0("dim", axe)


  resultat_modalites |>
    select(modalites,
           n,
           pourcentage,
           starts_with(dim.chr)) |>
    mutate(
      Effectif = paste0(n, " (",
                           en_pourcentage(pourcentage, digits = 0),
                           ")"),
      .keep = "unused"
    ) |>
    rename(
      `Modalité` = modalites,
      Contribution = contains(paste0(dim.chr, "_contrib")),
      Coordonnée = contains((paste0(dim.chr, "_coord"))),
      `Cos²` = contains((paste0(dim.chr, "_cos2"))),
      `V-test` = contains((paste0(dim.chr, "_vtest"))),
      Effectif = Effectif
    ) |>
    filter(Contribution > seuil) |>
    arrange(desc(Contribution)) |>
    mutate(
      Contribution = Contribution |> en_pourcentage()
    ) |>
    relocate(Modalité, Contribution, Coordonnée)
}

