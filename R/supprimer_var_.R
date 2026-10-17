#' Supprimer le format variable_modalite
#'
#' FactoMineR renvoie parfois pour certaines colonnes un résultat avec des modalités qui ont la forme de "variable_modalite". Cette fonction permet de nettoyer les tableaux qui se présentent sous cette forme.
#'
#' @param df un tableau à nettoyer
#'
#' @returns Le data.frame nettoyé
#' @importFrom purrr map2
#' @keywords internal
#' @examples #

enlever_prefixe <- function(x, prefixe) {
  if (is.factor(x)) {
    levels(x) <- enlever_prefixe(levels(x), prefixe)
    return(x)
  }
  if (is.character(x)) {
    x |>
      str_remove(str_c("^",
                       str_escape(prefixe))
                 )
  } else x  # numérique, logique, etc. : on ne touche pas
}

supprimer_var_ <- function(df) {
  df[] <- purrr::imap(df, \(x, nom) enlever_prefixe(x, paste0(nom, "_")))
  df
}
