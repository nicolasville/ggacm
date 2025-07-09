#' Obtenir les résultats pour chaque individus
#'
#'
#' @param resultat_acm resultat acm
#' @param nom_variable_individu une chaîne de caractère (qui s'écrit donc entre guillemets anglais "") qui donne le nom de la variable du tableau qui identifie les individus.
#'
#' @returns un tableau data.frame au format tidy, prêt à être utilisé pour ggplot
#' @export
#' @import dplyr
#' @import FactoMineR
#' @import tidyr
#'
#' @examples
#' # un exemple pas piqué des annetons
extraire_individus2 <- function(resultat_acm, nom_variable_individu) {

  if (missing(nom_variable_individu)) {
    cli::cli_abort(c(
      "x" = "L'argument {.arg nom_variable_individu}, n'a pas été fourni a la fonction.",
      "i" = 'Avez-vous oublié d\'inclure {.code nom_variable_individu = "votre_variable"} ?',
      ">" = "Indiquez-y la variable qui permet d'identifier chaque individu.",
      " " = 'Par exemple : le nom_prenom ou l\'identifiant ou le titre_de_la_chaine, etc.'
    ))
  }

  if (!is.character(nom_variable_individu)) {
    cli::cli_abort(c(
      "x" ="L'argument {.arg nom_variable_individu} doit être un chaîne de caractère.",
      "i" = "Avez vous oublié de mettre des guillemets à l'argument ?"
      ))
  }

  # on sors pour commence le data.frame d'entrée original
  data_acm <- supprimer_var_(resultat_acm$call$X)

  # on extrait ensuite les résultats pour chaque modalité
  resultats_complet <- extraire_modalites(resultat_acm)


  output <- resultats_complet |>
    filter(variables == nom_variable_individu) |>
    rename(!!nom_variable_individu := modalites) |>
    # ne cherche pas la colonne nom_variable_individu mais celle qu'on a défini
    # comme nom_variable_individus
    left_join(data_acm) |>
    rename(modalites = nom_variable_individu) |>
    relocate(type, variables, n, pourcentage, ends_with("contrib") ,.after = everything()) |>
    tibble::as_tibble()

  return(output)
}

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
