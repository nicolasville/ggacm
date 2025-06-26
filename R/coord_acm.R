#' Paramétrer les coordonnées de ggplot2 pour une ACM.
#'
#' Cette fonction permet de "fixer" les coordonnées d'une représentation. Par défaut, ggplot représente les axes sans qu'il soit égaux entre eux : par exemple si monter de 1 cm vers le haut sur la représentation peut vous faire gagner 0.1 en coordonnées sur l'axe des ordonnées, rien ne garantit par défaut qu'en faisant la même chose vers la droite vous augmenterez de 0.1 en coordonnée sur l'axe des abscisse. Cette fonction permet de remédier à cela. Par ailleur, elle trace deux lignes symbolisant les axes sur la représentation.
#'
#' Vous n'avez pas besoin d'utiliser cette fonction si vous avez recours à [ggacm()] qui fait déjà cela sous le capot sans que vous ayez besoin d'y toucher. Cette fonction est cependant là dans le cas où vous souhaitiez créer des graphiques avancés sans utiliser [ggacm()].
#'
#' @param ... une liste optionnelle d'argument à passer à [ggplot2::coord_fixed()]. Cet argument est optionnel. Deux arguments que vous pouvez avoir envie de passer à la fonction sont : xlim et ylim qui fixent les limites des coordonnées. Par défaut les coordonnées de la représentation s'adaptent aux coordonnées des points représentés mais vous pouvez avoir envie de fixer ces coordonnées pour pouvoir comparer par exemple deux graphiques. Si vous voulez que ces coordonnées soient fixés de -1 à 2 pour l'axe des abscisse et de -2 à 1 pour l'axe des ordonnées : remplacez ... par `xlim = c(-1, 2), ylim = c(-2, 1)`.
#'
#'
#' @returns Un objet ggplot dont les coordonnées ont été fixés.
#' @export
#'
#' @examples
#' # une série d'exemple
coord_acm <- function(...) {
  list(
    coord_fixed(
      ratio = 1,
      ...
    ),
    geom_hline(
      yintercept = 0,
      colour = "darkgrey",
      linetype = "longdash"
    ),
    geom_vline(
      xintercept = 0,
      colour = "darkgrey",
      linetype = "longdash"
    )
  )
}
