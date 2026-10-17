#' Title
#'
#' @param resultat_acm Résultat de l'ACM avec Factominer
#' @param active TRUE ou FALSE. TRUE pour extraire les variables actives. FALSE pour extraire les variables supplémentaires
#'
#' @returns pour chaque modalité les coordonnées, les contributions, les vtest et les cosinus carrés pour les 5 premiers axes.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import purrr
#'
#' @keywords internal
#'
#' @examples
#' # une liste d'exemple
get_res_act <-
  function(resultat_acm) {

    data_acm <- resultat_acm$call$X

    frequences <- get_frequence(data_acm)

    # extraire les données de ...$var$coord, ..$var$cos2, ..$var$contrib, ...
    output <- resultat_acm$var |>
      imap(\(m, nom) {
        m |>
          as_tibble(rownames = "modalites") |>
          rename_with(
            \(x) {
              x |> # chaque nom de colonne... :
                str_to_lower() |> # en minuscule
                str_c(nom, sep = "_") |> # on colle un suffixe en fonction de l'indicateur
                str_remove_all(" |\\.") # on enlève les points et les espaces
            },
            -modalites # partout sauf ici
          )
      }
      ) |>
      reduce(left_join, by = "modalites") # on left_join tout ensemble.


    # on peut ensuite rajouter les fréquences
    output <- output %>%
      left_join(frequences, by = "modalites") |>
      mutate(type = "Variable active") %>%
      # ajout d'une colonne contenant la chaîne de caractères "Variable active"
      # ou "variable supplémentaire en fonction du cas
      select(
        type,
        variables,
        modalites,
        n,
        pourcentage,
        contains("dim1"),
        contains("dim2"),
        contains("dim3"),
        contains("dim4")
      ) # conserver et réorganiser les variables pertinentes axe par axe

    # FactoMineR met parfois des préfixes au modalités en fonction des variables
    # On enlève ça ici. Pour chaque ligne, on supprimer "var1_".
    # str_escape() sert dans les cas où le nom de la variable contient des
    # points par exemple

    output <- output |>
      mutate(modalites = modalites |> str_remove(str_c("^", str_escape(variables), "_")))
  }

get_res_sup <- function(resultat_acm) {

  data_acm <- resultat_acm$call$X

  frequences <- get_frequence(data_acm)

  # extraire les données de ...$var$coord, ..$var$cos2, ..$var$contrib, ...
  output <- resultat_acm$var |>
    imap(\(m, nom) {
      m |>
        as_tibble(rownames = "modalites") |>
        rename_with(
          \(x) {
            x |> # chaque nom de colonne... :
              str_to_lower() |> # en minuscule
              str_c(nom, sep = "_") |> # on colle un suffixe en fonction de l'indicateur
              str_remove_all(" |\\.") # on enlève les points et les espaces
          },
          -modalites # partout sauf ici
        )
    }
    ) |>
    reduce(left_join, by = "modalites") # on left_join tout ensemble.

  # on peut ensuite rajouter les fréquences
  output <- output %>%
    left_join(frequences, by = "modalites") |>
    mutate(type = "Variable supplémentaire") %>%
    # ajout d'une colonne contenant la chaîne de caractères "Variable active"
    # ou "variable supplémentaire en fonction du cas
    select(
      type,
      variables,
      modalites,
      n,
      pourcentage,
      contains("dim1"),
      contains("dim2"),
      contains("dim3"),
      contains("dim4")
    ) # conserver et réorganiser les variables pertinentes axe par axe

  # FactoMineR met parfois des préfixes au modalités en fonction des variables
  # On enlève ça ici. Pour chaque ligne, on supprimer "var1_".
  # str_escape() sert dans les cas où le nom de la variable contient des
  # points par exemple

  output <- output |>
    mutate(modalites = modalites |> str_remove(str_c("^", str_escape(variables), "_")))
}
