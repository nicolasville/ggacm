#' Utiliser un thème adapté à l'analyse de correspondance multiple
#'
#' Cette fonction permet de générer un thème de graphique adapté aux ACM et aux publications : Fond blanc, police Times New Roman, légende en bas du graphique et transforme les axes en flèches.
#'
#' @param ... Si vous voulez customiser le thème plus en profondeur, il est possible de rajouter des éléments qui seront passé à [ggplot2::theme()]
#'
#' @returns Un thème ggplot
#' @export
#'
#' @examples #
theme_acm <- function(...){
  theme_classic(base_family = "serif") %+replace%
    theme(
      plot.title = element_text(size = 20, margin = margin(b = 8)),
      axis.line = element_line(colour = "black",
                               arrow = arrow(length = unit(0.20, "cm"),
                                             type = "closed")),
      plot.caption = element_text(hjust = 0, color = "grey30"),
      plot.caption.position = "plot",
      legend.position = "bottom",
      ...
    )
}

