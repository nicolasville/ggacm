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
extraire_individus <- function(resultat_acm, nom_variable_individu) {

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
