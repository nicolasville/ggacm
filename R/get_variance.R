
#' Décrire la variance des capturer par chaque axe
#'
#' @param resultat_acm un résultat d'ACM qui sort de factominer::MCA()
#'
#' @returns un tableau (data.frame) qui documente la variance des 5 premiers axes.
#' @export
#' @importFrom tibble rownames_to_column
#' @examples
#' # une liste d'exemple
get_variance <- function(resultat_acm){
  resultat_acm |>
    pluck("eig") |>
    as.data.frame() %>%
    rownames_to_column() %>% # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
    slice(1:10) %>% # conserver que les infos des 10 premiers axes
    mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
    select(-rowname) %>%# on enlève cette colonne dont on n'a plus besoin
    rename(`Valeurs propres` = eigenvalue) %>%
    rename(`% de variance` = `percentage of variance`) %>% # on renomme les autres colonnes
    rename(`% cumulé de variance` = `cumulative percentage of variance`) %>%
    mutate(Axes = fct_relevel(Axes, paste("Axe", 1:10))) |>
    relocate(Axes) # pour que l'ordre de 1 à 10 soit bien respecté dans les graphiques
}
