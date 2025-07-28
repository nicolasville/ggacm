#' Nommer les axes d'une représentation graphique d'ACM
#'
#' Lorsque l'on représente une ACM, il est courant de vouloir donner un nom aux axes, ou au moins les pôles de chacun de ces axes. Cette fonction permet de générer les labels pour chaque axe à utiliser lorsqu'on génère des graphiques axec ggplot
#'
#' @param resultat_acm le résultat d'une ACM sortie de [MCA()]
#' @param en_haut à quoi correspond le haut du graphique ? entre guillemet.
#' @param en_bas idem mais en bas
#' @param a_gauche idem mais à gauche
#' @param a_droite idem mais a droite
#'
#' @returns une couche ggplot à ajouter. Sous le capot cette fonction fait génère des annotations pour graphique ggplot avec [ggplot2::labs()]
#' @export
#'
#' @examples #
nommer_axes <- function(resultat_acm,
                        en_haut = "",
                        en_bas = "",
                        a_gauche = "",
                        a_droite = ""){


  # Checks
  check_resultat_acm(resultat_acm)
  check_chr_scalar(en_haut)

  var1 <- extraire_variance(resultat_acm)[1, 3]
  var2 <- extraire_variance(resultat_acm)[2, 3]

  axes <- labs(
    x = paste0(a_gauche, " ← Axe 1 (", var1, ") → ", a_droite),
    y = paste0(en_bas, " ← Axe 2 (", var2, ") → ", en_haut)
  )

  axes
}
