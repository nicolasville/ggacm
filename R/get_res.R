#' Title
#'
#' @param resSqqche Résultat de l'ACM avec Factominer
#' @param quoi a quoi correspond chaque ligne de chaque matrice ?
#'
#' @returns extrait les indicateurs d'une liste de matrice avec les mêmes noms
#'  de lignes.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import purrr
#' @import stringr
#'
#' @keywords internal
#'
#' @examples
#' # une liste d'exemple
get_indicateurs <- function(resSqqche, quoi) {

    # extraire les données de ...$var$coord, ..$var$cos2, ..$var$contrib, ...
    output <- resSqqche |>
      imap(\(m, nom) {
        m |>
          as_tibble(rownames = quoi) |>
          rename_with(
            \(x) {
              x |> # chaque nom de colonne... :
                str_to_lower() |> # en minuscule
                str_c(nom, sep = "_") |> # on colle un suffixe en fonction de l'indicateur
                str_remove_all(" |\\.") # on enlève les points et les espaces
            },
            -any_of(quoi) # partout sauf ici
          )
      }
      ) |>
      reduce(left_join, by = quoi) # on left_join tout ensemble.

    output
  }


