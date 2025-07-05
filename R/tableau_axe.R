

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
caracteriser_axe <- function(resultat_modalites, axe, seuil = 0) {

  # check ---------------------
  if (!is.numeric(axe)) stop("L'argument axe est manquant/mal renseigné. Veuillez choisir l'axe à décrire en renseignant par exemple `axe = 1` si vous voulez décrire le premier axe.")

  # on sort les tableaux pour l'axe en question
  carac_axe <- tableau_axe(resultat_modalites = resultat_modalites, axe = axe, seuil = seuil)

  # on sépare en deux tableau (un pour les coordonnées positives, l'autre pour les négatives)
  carac_axe_coord_positive <- carac_axe |> filter(Coordonnée > 0) |> as_gtsummary()
  carac_axe_coord_negative <- carac_axe |> filter(Coordonnée < 0) |> as_gtsummary()

  # on combine les deux tableaux
  tableau_sortie <- tbl_stack(
    tbls = list(carac_axe_coord_positive, carac_axe_coord_negative),
    group_header = c("Coordonnées positives", "Coordonnées négatives")
  )

  # enfin du formattage
  tableau_sortie |>
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
    ) |>
    # column header bold
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
tableau_axe <- function(resultat_modalites, axe, seuil = 0) {

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

