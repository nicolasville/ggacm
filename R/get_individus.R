#' Obtenir les résultats pour chaque individus
#'
#' Cette fonction permet quadd on lui donne le résultat d'une ACM de sortir un
#' tableau qui reprend le tableau de donnée initial en ajoutant pour chaque
#' individus leur coordonnées.
#'
#' @param resultat_acm resultat acm
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
extraire_individus <- function(resultat_acm) {

  if (inherits(resultat_acm, "data.frame")) {

    cli::cli_abort(c(
      "x" = "{.arg resultat_acm} doit être un résultat de {.fun FactoMineR::MCA} ou {.fun ACM}, et non pas un tableau de donnée (data.frame)"
    ))

  }

  # On vient chercher le tableau dde donnée originel
  data_acm <- resultat_acm$call$X  |> supprimer_var_()

  # puis on numérote les lignes dans une colonne id.
  data_acm <- data_acm |> as_tibble(rownames = "id")

  # fonction pour transformer les Dim 1 et autres joyeseté en noms de colonnes valides
  nettoyage <- function(x, quoi) {

    x |>
      as_tibble() |>
      rename_all(stringr::str_to_lower) %>%
      rename_all(~ stringr::str_replace(., " ", "")) %>%
      rename_all(~ stringr::str_c(., quoi, sep = "_"))

  }

  # ensuite on va chercher les coordonnées des individus
  # as_tibble(rownames = "id") fonctionne de la même manière que row_id_to_column
  coordonnees <- resultat_acm$ind$coord|>
    nettoyage(quoi = "coord") |>
    as_tibble(rownames = "id")

  contribution <- resultat_acm$ind$contrib|>
    nettoyage(quoi = "contrib") |>
    as_tibble(rownames = "id")

  cos2 <- resultat_acm$ind$cos2 |>
    nettoyage(quoi = "cos2") |>
    as_tibble(rownames = "id")

  # puis on combine les tableaux
  data_acm |>
    left_join(coordonnees, by = "id") |>
    left_join(contribution, by = "id") |>
    left_join(cos2, by = "id") |>
    select(-id) |>
    relocate(dim1_coord, dim2_coord)

}
