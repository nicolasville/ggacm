#' Obtenir les résultats d'une ACM pour chaque modalités.
#'
#' Cette fonction permet de renvoyer un
#'
#' @param resultat_acm Une résultat d'ACM sortie de [FactoMineR::MCA()] ou [ACM()].
#' @param data_acm le tableau de donnée d'origine
#'
#' @returns Un tableau avec pour chaque modalité les coordonnées,
#'  les contributions, les cos2 et les vtest pour les 5 premières dimensions.
#'
#'  Dans le détail, le tableau
#'  comporte les variables :
#'  * `type` : variables actives ou supplémentaires ?
#'  * `variable` : Le nom de la variable.
#'  * `modalites` : la modalité en question.
#'  * `n` : Le nombre d'individus au sein du tableau de données qui partagent
#'   cette modalité.
#'  * `pourcentage` : Par des individus au sein du tableau de données qui partagent
#'   cette modalité
#'  * `dim1_coord`, `dim2_coord`, ... : Coordonnée de la modalité sur l'axe 1, 2, ...
#'  * `dim1_contrib`, `dim2_contrib`,... : Contribution de chaque modalité à la
#'   contruction des axes sur l'axe 1, 2, ...
#'  * `dim1_cos2`, `dim2_cos2`, ... : Cosinus carré des modalités sur l'axe 1, 2,...
#'  * `dim1_vtest`, `dim2_vtest`, ... : Vtest des coordonnées de la modalité sur
#'   l'axe 1, 2,...
#'
#'
#' @export
#' @import stringr
#' @import dplyr
#'
#' @examples
#' # on commence par créer un résultat d'ACM :
#' resultat <- createurs_de_contenus_pol |>
#'   ACM(variables_supplementaires = personne)
#'
#' # Puis on peut en extraire le tableau descriptif de chaque modalité :
#' resultat |> extraire_modalites()
#'
#'
#'
extraire_modalites <- function(resultat_acm) {

  # Checks
  check_resultat_acm(resultat_acm)

  resultats_actives <- resultat_acm$var |>
    purrr::discard_at("eta2") |>  # on enlève les "eta2" qui sont sur les variables
    get_indicateurs("modalites")

  resultats_sup     <- if (is.null(resultat_acm$quali.sup)) NULL else {
    resultat_acm$quali.sup |>
      purrr::discard_at("eta2") |>
      get_indicateurs(quoi = "modalites")
  }

  resultat_complet <- bind_rows(
    `Variable active`         = resultats_actives,
    `Variable supplémentaire` = resultats_sup,
    .id = "type")

  frequences <- get_frequence(resultat_acm$call$X)

  resultat_complet |>
    mutate(
      modalites = if_else(modalites == "NA", NA, modalites)
    ) |>
    left_join(frequences, by = "modalites") |>
    dplyr::filter_out(is.na(variables) & is.na(n)) |>
    select(
      type,
      variables,
      modalites,
      n,
      pourcentage,
      contains("dim1"),
      contains("dim2"),
      contains("dim3"),
      contains("dim4"),
      contains("dim5")
    ) |> # conserver et réorganiser les variables pertinentes axe par axe
  mutate(
    modalites = modalites |>
      str_remove(
        str_c("^", str_escape(variables), "_"))
            # "^[nom de variable]_" à enlever au début.)
  )

}
