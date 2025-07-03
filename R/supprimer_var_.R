#' Supprimer le format variable_modalite
#'
#' FactoMineR renvoie parfois pour certaines colonnes un résultat avec des modalités qui ont la forme de "variable_modalite". Cette fonction permet de nettoyer les tableaux qui se présentent sous cette forme.
#'
#' @param df un tableau à nettoyer
#'
#' @returns Le data.frame nettoyé
#' @importFrom purrr map2
#' @examples #
supprimer_var_ <- function(df) {
  noms_colonnes <- names(df)

  purrr::map2(
    .x = df,
    .y = noms_colonnes,
    function(.x, .y) {
      sub(
        paste0("^", .y, "_"),
        "",
        .x
      )
    }
  ) |>
    as.data.frame()
}
