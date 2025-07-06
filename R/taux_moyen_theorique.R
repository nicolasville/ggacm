
#' Calculer la contribution moyenne théorique
#'
#' Certains manuels recommandent de calculer la contribution moyenne théorique pour s'en servir comme seuil afin de déterminer quelles sont les variables à prendre en compte dans l'analyse. Cette fonction permet de calculer cette contribution moyenne théorique : elle fait 100 % divisé par le nombre de modalité active.
#'
#' @param resultat_acm Le résultat d'une ACM
#'
#' @returns un nombre entre 100 et 0
#' @export
#'
#' @examples
#' resultat_acm <- createurs_de_contenus_pol |> FactoMineR::MCA(quali.sup = c("titre", "personne", "orientation"))
#'
#' contribution_moyenne_theorique(resultat_acm)
#'
#'
contribution_moyenne_theorique <- function(resultat_acm) {

  # on sort les résultats des modalités
  resultat_modalites <- extraire_modalites(resultat_acm = resultat_acm)

  nb_modalites_actives <- resultat_modalites |>
    filter(type == "variable active") |>
    nrow()

  message(paste0("Si toutes les modalités actives contribuait de manière équivalente à la construction d'un axe, chacune aurait une contribution de ", round(100 / nb_modalites_actives, digits = 1), " %. \n"))

  100 / nb_modalites_actives

}
