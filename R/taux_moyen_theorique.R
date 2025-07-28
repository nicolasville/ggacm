
#' Calculer la contribution moyenne théorique
#'
#' Certains manuels recommandent de calculer la contribution moyenne théorique pour s'en servir comme seuil afin de déterminer quelles sont les variables à prendre en compte dans l'analyse. Cette fonction permet de calculer cette contribution moyenne théorique : elle divise 100 % par le nombre de modalité active.
#'
#' @param resultat_acm Le résultat d'une ACM.
#'
#' @returns Un nombre entre 0 et 100 qui indique le pourcentage de contribution
#'  moyenne théorique.
#' @export
#'
#' @examples
#' resultat_acm <- createurs_de_contenus_pol |>
#'   FactoMineR::MCA(quali.sup = c("titre", "personne", "orientation"))
#'
#' contribution_moyenne_theorique(resultat_acm)
#'
#'
contribution_moyenne_theorique <- function(resultat_acm) {

  # Checks
  check_resultat_acm(resultat_acm)

  # on sort les résultats des modalités
  resultat_modalites <- extraire_modalites(resultat_acm = resultat_acm)

  nb_modalites_actives <- resultat_modalites |>
    filter(type == "variable active") |>
    nrow()

  cont_moy_theo <- 100 / nb_modalites_actives

  cli::cli_alert_info("Si toutes les modalités actives contribuaient également à la construction d'un axe, chacune aurait une contribution de {round(cont_moy_theo, digits = 2)} %.", wrap = TRUE)

  cont_moy_theo

}
