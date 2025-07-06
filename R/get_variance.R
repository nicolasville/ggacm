
#' Décrire la variance des capturer par chaque axe
#'
#' @param resultat_acm un résultat d'ACM qui sort de factominer::MCA()
#'
#' @returns un tableau (data.frame) qui documente la variance des 5 premiers axes.
#' @export
#' @importFrom tibble rownames_to_column
#' @importFrom forcats fct_relevel
#' @examples
#' # une liste d'exemple
extraire_variance <- function(resultat_acm){
  resultat_acm$eig  |>
    as.data.frame() %>%
    tibble::rownames_to_column() %>% # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
    slice(1:10) %>% # conserver que les infos des 10 premiers axes
    mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
    select(-rowname) %>%# on enlève cette colonne dont on n'a plus besoin
    mutate(
      Axes = forcats::fct_relevel(Axes, paste("Axe", 1:10)),
      `Valeurs propres` = eigenvalue |> round(digits = 2),
      `% de variance` = `percentage of variance` |> en_pourcentage(),
      `% cumulé de variance` = `cumulative percentage of variance` |> en_pourcentage(),
      .keep = "unused"
    ) |>
    relocate(Axes) # replace Axes au début du tableau
}
