#' Obtenir les résultats d'une ACM pour chaque modalités.
#'
#' @param resultat_acm résultat sortie de factominer::MCA
#' @param data_acm le tableau de donnée d'origine
#'
#' @returns un tableau avec pour chaque modalité les coordonnées, les contributions, les cos2 et les vtest pour les 5 premières dimensions
#' @export
#' @import stringr
#' @import dplyr
#'
#' @examples
#' # une liste d'exemple
get_variables <- function(resultat_acm, data_acm) {
  resultats_actives <- resultat_acm$var |> get_res(data_acm, active = TRUE)
  resultats_sup <- resultat_acm$quali.sup |> get_res(data_acm, active = FALSE)
  resultats_complet <- bind_rows(resultats_actives, resultats_sup)

  # obtenir le tableau de fréquence des modalités depuis les données
  frequences <- get_frequence(data_acm)

  # un certain nombre de choses ne se sont pas passé comme prévu, notamment la
  # jonction entre le tableau de fréquence et les autres. C'est du au fait que
  # certaines modalités sont stockés sous la forme [variable]_[modalité]
  # et d'autres pas. La suite de la fonction a pour but de traiter les cas
  # où les modalités ont été stockés sous cette forme. Que l'on détecte
  # par le fait que la jonction avec le tableau de fréquence a justement échoué

  # deux petites fonctions qui servent à uniformiser les noms et les modalités
  extraire_modalite <- function(x) {
    x |>
      str_replace("\\.NA", "_NA") |> str_extract("_([^_]+)$", group = 1)
  }
  extraire_variable <- function(x) {
    x |>
      str_replace("\\.", "_") |>
      str_extract("(.+)_(.+)$", group = 1)
  }

  resultats_complet <- resultats_complet |>
    mutate(
      # quand la jonction avec le tableau de fréquence a échoué :
      variables = if_else(is.na(n),
                          extraire_variable(modalites),
                          variables),
      modalites = if_else(is.na(n),
                          extraire_modalite(modalites),
                          modalites)
    ) |>
    mutate(
      modalites = if_else(modalites == "NA", NA, modalites)
    ) |>
    left_join(frequences, by = c("variables", "modalites")) |>
    # on a maintenant deux colonnes de pourcentage.x et .y. Quand l'information
    # est dans une des colonnes, elle n'est pas dans l'autre.
    # On uniformise cela :
    mutate(
      n = if_else(is.na(n.x), n.y, n.x),
      pourcentage = if_else(is.na(pourcentage.x), pourcentage.y, pourcentage.x),
      .keep = "unused",
      .before = starts_with("dim")
    )

  return(resultats_complet)
}
