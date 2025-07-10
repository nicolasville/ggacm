#' Visualiser rapidement le nuage d'individus
#'
#' On veut souvent visualiser rapidement un nuage d'individus sans trop y passer du temps. Cette fonction est un raccourci à cela, elle permet de faire
#'
#' @param resultat_acm Le résultat d'une acm
#' @param couleur on peut optionnellement coloré le nuage d'individus en fonction d'une variable.
#'
#' @returns un objet ggplot
#' @export
#'
#' @examples #
nuage_individus <- function(resultat_acm, couleur) {
  resultat_individus <- extraire_individus(resultat_acm)

  output <- resultat_individus |>
    ggacm() +
    ggplot2::labs(title = "Nuage d'individus dans le plan 1 et 2")

  if (!missing(couleur)) {
    output <- output +
      ggplot2::geom_point(aes(color = {{ couleur }}))
  } else {
    output <- output +
      ggplot2::geom_point(aes(color = {{ couleur }}, shape = {{ couleur }}))
  }

  output
}
