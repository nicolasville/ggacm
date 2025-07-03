#' Caractériser les classes
#'
#' La fonction présenter ici permet d'obtenir plus d'information sur une classification ascendante hiérarchique.
#' Elle produit des tableaux qui reprennent pour une classe donnée, les caractéristiques typiques et atypiques dans la classe.
#'
#' @param resultat_classification Un résultat de classification réalisé avec `FactoMineR::HCPC()`
#' @param numero_de_la_classe Le numéro de la classe que l'on souhaite caractériser.
#'
#' @returns Un tableau de présentation de la classe. Il s'agit d'un objet gt.
#' @export
#'
#' @importFrom gt tab_style
#' @importFrom gt cell_text
#' @importFrom gt cells_row_groups
#' @examples #
#' # une super exemple
caracteriser_classe <- function(resultat_classification, numero_de_la_classe) {

  # 1 On extrait le résultat de la classification :
  caracterisation_classe.df <- resultat_classification$desc.var$category |>
    pluck(numero_de_la_classe)

  # 2 On nettoie le nom des modalités (qui s'affichent comme VARIABLE=Modalité)
  caracterisation_classe.df <- caracterisation_classe.df |>
    as_tibble(x, rownames = "modalites") |>
    mutate(
      modalites = str_remove(modalites, ".+=")
    )

  # 3 obtenir le nb de membre de la classe
  n_classe <- resultat_classification$data.clust |>
    count(clust) |>
    filter(clust == numero_de_la_classe) |>
    pull(n)

  # 4 obtenir le nb de personnes dans l'ensemble du corpus
  n_ensemble <- resultat_classification$data.clust |>
    count() |>
    pull(n)

  # 5 changer le formableau pour obtenir quelque chose de lisible
  caracterisation_classe.df <- caracterisation_classe.df |>
    # les caractéristiques d'abord, les anti caractéristiques après
    mutate(
      anti.profil = if_else(v.test > 0, FALSE, TRUE)
    ) |>
    arrange(anti.profil, p.value) |>
    # on créer les colonnes
    mutate(
      modalites = modalites,
      dans_la_classe = paste0(
        `Mod/Cla` / 100 * n_classe, # = n dans la classe
        " (",
        `Mod/Cla` |> round(digits = 0), # = % dans la classe
        " %)"
      ),
      Ensemble = paste0(
        Global / 100 * n_ensemble, # = n sur le corpus
        " (",
        Global |> round(digits = 0), # % dans le corpus
        " %)"
      ),
      dans_l_ensemble = paste0(
        `Cla/Mod` / 100 * Global / 100 * n_ensemble, # n ayant partageant la modalité
        "/",
        Global / 100 * n_ensemble, # = n sur le corpus
        " (",
        `Cla/Mod` |> round(digits = 0), # %
        " %)"
      ),
      p.value = scales::label_pvalue(decimal.mark = ",")(p.value),
      v.test = v.test,
      .keep = "none"
    ) |>
    # Les colonnes importantes d'abord__________________________________
    relocate(v.test, p.value, .after = everything())

  profil.df <- caracterisation_classe.df |>
    filter(v.test > 0) |> as_gtsummary()

  anti_profil.df <- caracterisation_classe.df |>
    filter(v.test < 0) |> as_gtsummary()

  # on combine les deux tableaux :
  tableau_sortie <- tbl_stack(
    tbls = list(profil.df, anti_profil.df),
    group_header = c("Caractéristiques typiques", "Caractéristiques atypiques")
  )

  # quelques modifications pour la lecture __________________________
  tableau_sortie |>
    modify_header(
      modalites = "**Modalités**",
      dans_la_classe = paste0("**Composition de la classe**  \nN = ", n_classe),
      Ensemble = paste0("**Dans l'ensemble**  \nN = ", n_ensemble),
      dans_l_ensemble = "**Part des membres de la classe**",
      v.test = "**V-Test**",
      p.value = "**P-Valeur**"
    ) |>
    # éclaircissement des notes de bas de tableau ______________________
    modify_footnote_header("Nombre et part des membres de la classe partageant la modalité",
                           columns = dans_la_classe
    ) |>
    modify_footnote_header("Nombre et part des membres de l'ensemble des individus partageant la modalité",
                           columns = Ensemble
    ) |>
    modify_footnote_header("Part des membres de la classe au sein des individus partageant la même modalité",
                           columns = dans_l_ensemble
    ) |>
    modify_column_alignment(
      columns = modalites,
      align = "left"
    ) |>
    as_gt() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups(groups = everything())
    )
}


