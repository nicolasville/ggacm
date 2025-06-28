#' Visualiser les résultats d'une ACM
#'
#'
#' @param data Le résultat d'une ACM après qu'il soit passé par la fonction [get_individus()] ou [get_variables()].
#' @param mapping Par défaut prend les coordonnée de la dimension 1 en abcisse (`x = dim1coord`) et de la dimension 2 en ordonné (`y = dim2coord`). Il est possible d'utiliser d'autres aesthetics. Pour plus d'information voir la vignette et la documentation de ggplot. Cet argument doit toujours être écrit avec la fonction [aes()] comme : `aes(x = ..., y = ..., label = ..., etc.)`
#' @param ... Arguments optionnels passés à [coord_acm()] qui seront passé à [ggplot2::coord_fixed()].
#'
#' @returns un graphique ggplot
#' @export
#' @import ggplot2
#' @examples
#' # un exemple pas piqué des annetons
ggacm <- function(data,
                  mapping = aes(dim1_coord,
                                dim2_coord),
                  ...) {
  ggplot(data, mapping = mapping) +
    coord_acm(...)
}
