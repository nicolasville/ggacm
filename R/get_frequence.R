#' Obtenir la fréquence des modalités
#'
#' @param data_acm le data.frame à qui a été donné à manger à Factominer
#'
#' @returns un tableau qui renvoie pour chaque modalité le nombre d'individu concerné (n) ainsi que leur pourcentage
#' @export
#'
#' @examples
#' # une liste d'exemple
get_frequence <- function(data_acm) {
  data_acm |>
    gather(variables, modalites) %>% # étendre le jeu de données par variable et modalité
    count(variables, modalites) %>% # compter le nombre de couples "variable/modalité" unique (donc le nombre d'individus par modalité du jeu de données)
    group_by(variables) %>%
    mutate(pourcentage = round(100 * n / nrow(data_acm), 1)) %>% # calculer des pourcentages pour chaque groupe de variable
    ungroup() %>%
    select(variables, modalites, n, pourcentage)
}
