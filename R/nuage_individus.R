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
#' @examples
#'
#' # on commence par créer un résultat d'ACM :
#' resultat <- createurs_de_contenus_pol |>
#'   ACM(variables_supplementaires = personne)
#'
#' # puis on peut sortir le graphique
#' resultat |>
#'   nuage_individus() +
#'   theme_acm()
#'
nuage_individus <- function(resultat_individus, couleur) {


  if (inherits(resultat_individus, "MCA")) {
  resultat_individus <- extraire_individus(resultat_individus)
  }

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
