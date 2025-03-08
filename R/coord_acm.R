#' Title
#'
#' @param ... une liste optionnelle d'argument à passer à [ggplot2::coord_fixed()]. Cet argument est optionel
#'
#' @returns un objet ggplot qui sert de canva pour les ACM
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
    ),
    theme_bw()
  )
}
