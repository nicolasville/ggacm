#' Obtenir les résultats pour chaque individus
#'
#' Cette fonction permet quadd on lui donne le résultat d'une ACM de sortir un
#' tableau qui reprend le tableau de donnée initial en ajoutant pour chaque
#' individus leur coordonnées.
#'
#' @param resultat_acm resultat acm
#' @param ordre ne pas toucher.
#'
#' @returns Le tableau de données surlequel vous avez réalisé l'ACM auquel a été
#'  ajouté pour chaque individus : leur coordonnées sur chaque axe (stockés dans des
#'  colonnes `dim1_coord`, `dim2_coord`, `dim3_coord`, etc), leur contribution à
#'  la construction des axes (colonnes `dim1_contrib`, `dim2_contrib`,...) et
#'  enfin leur cosinus carré (colonnes `dim1_cos2`,...)
#'
#' @export
#' @import dplyr
#' @importFrom FactoMineR MCA
#' @import tidyr
#'
#' @examples
#' resultat_acm <- createurs_de_contenus_pol |> ACM()
#'
#' resultat_individus <- resultat_acm |> extraire_individus()
#'
#' resultat_individus
extraire_individus <- function(resultat_acm, ordre = NULL) {

  check_resultat_acm(resultat_acm)

  # On vient chercher le tableau de donnée originel
  data_acm <- resultat_acm$call$X  |>
    supprimer_var_() |>
    as_tibble()

  # Utilisé pour extraire_classe(). En effet, les tableaux ne sont plus dans
  # l'ordre une fois passés par HCPC(). Il faut donc trouver une manière
  # d'extraire les individus selon un ordre particulier
  ids <- if (is.null(ordre)) rownames(data_acm) else ordre

  # fonction pour transformer les Dim 1 et autres joyeseté en noms de colonnes valides
  nettoyage <- function(x, quoi) {

    x <- as.data.frame(x)[ids, , drop = FALSE] # met dans l'ordre

    x |>
      as_tibble() |>
      rename_all(stringr::str_to_lower) %>%
      rename_all(~ stringr::str_replace(., " ", "")) %>%
      rename_all(~ stringr::str_c(., quoi, sep = "_"))
  }

  # assemblage du tableau final
  dplyr::bind_cols(
    data_acm,
    nettoyage(resultat_acm$ind$coord,   "coord"),
    nettoyage(resultat_acm$ind$contrib, "contrib"),
    nettoyage(resultat_acm$ind$cos2,    "cos2")
  ) |>
    relocate(dim1_coord, dim2_coord)

}
