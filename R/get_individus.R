#' Obtenir les résultats pour chaque individus
#'
#'
#' @param resultat_acm resultat acm
#' @param data_acm donnée acm
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
get_individus <- function(resultat_acm, data_acm, nom_variable_individu) {
  if (!is.character(nom_variable_individu)) {
    stop("L'argument nom_variable_individu doit être un chaîne de caractère.")
  }
  resultats_complet <- get_variables(resultat_acm, data_acm)
  output <- resultats_complet |>
    filter(variables == nom_variable_individu) |>
    rename(!!nom_variable_individu := modalites) |>
    # ne cherche pas la colonne nom_variable_individu mais celle qu'on a défini
    # comme nom_variable_individus
    left_join(data_acm) |>
    rename(modalites = nom_variable_individu) |>
    relocate(type, variables, n, pourcentage, ends_with("contrib") ,.after = everything())

  return(output)
}
