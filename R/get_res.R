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

    # une fonction pour faire en masse ce qu'on va faire pour coord, contrib cos2 et vtest :
    matrice_a_df <- function(resultat_acm_varSqqchse, quoi){
      resultat_acm_varSqqchse |> # prend la matrice
        as.data.frame() |> # la transforme en df
        rename_all(tolower) %>% # met tout en minuscule
        rename_all(~ str_replace(., " ", "")) %>% # supprime les espaces des noms de colonnes
        rename_all(~ str_c(., quoi, sep = "_")) %>% # colle un suffixe en fonction de ce que c'est
        mutate(modalites = rownames(.)) # transforme les noms de lignes en variable
    }

    # à appliquer pour les :
    # coordonnées -------
    coordonnees <- resultat_acm$var$coord |> matrice_a_df(quoi = "coord")

    # Cosinus carrés ----
    cos2 <- resultat_acm$var$cos2 |> matrice_a_df(quoi = "cos2")

    # vtest  ----
    vtest <- resultat_acm$var$v.test |> matrice_a_df(quoi = "vtest")

    contributions <- resultat_acm$var$contrib |> matrice_a_df(quoi = "contrib")


    # qu'on combine ensuite dans un tableau
    output <- coordonnees |>
      left_join(cos2, by = "modalites") |>
      left_join(vtest, by = "modalites") |>
      left_join(contributions, by = "modalites")


    # on peut ensuite rajouter les fréquences
    output <- output %>%
      left_join(frequences, by = "modalites") |>
      mutate(type = "variable active") %>%
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

    # parfois FactoMineR met en préfixe le nom des variables. Il faut donc les enlever
    variables.chr <- names(data_acm)

    # on génère une expression regex pour tester cela :
    # sélectionne les début de chaine qui commence par le nom d'une variable suivi d'un _
    patterne <- paste0("^(", paste0(variables.chr, collapse = "|"), ")_")

    output$modalites <- gsub(patterne, "", output$modalites)

    output
  }

get_res_sup <- function(resultat_acm) {

  data_acm <- resultat_acm$call$X

  frequences <- get_frequence(data_acm)

  # une fonction pour faire en masse ce qu'on va faire pour coord, contrib cos2 et vtest :
  matrice_a_df <- function(resultat_acm_varSqqchse, quoi){
    resultat_acm_varSqqchse |> # prend la matrice
      as.data.frame() |> # la transforme en df
      rename_all(tolower) %>% # met tout en minuscule
      rename_all(~ str_replace(., " ", "")) %>% # supprime les espaces des noms de colonnes
      rename_all(~ str_c(., quoi, sep = "_")) %>% # colle un suffixe en fonction de ce que c'est
      mutate(modalites = rownames(.)) # transforme les noms de lignes en variable
  }

  # à appliquer pour les :
  # coordonnées -------
  coordonnees <- resultat_acm$quali$coord |> matrice_a_df(quoi = "coord")

  # Cosinus carrés ----
  cos2 <- resultat_acm$quali$cos2 |> matrice_a_df(quoi = "cos2")

  # vtest  ----
  vtest <- resultat_acm$quali$v.test |> matrice_a_df(quoi = "vtest")


  # qu'on combine ensuite dans un tableau
  output <- coordonnees |>
    left_join(cos2, by = "modalites") |>
    left_join(vtest, by = "modalites")

  # on peut ensuite rajouter les fréquences
  output <- output %>%
    left_join(frequences, by = "modalites") |>
    mutate(type = "variable supplémentaire") %>%
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

  # parfois FactoMineR met en préfixe le nom des variables. Il faut donc les enlever
  variables.chr <- names(data_acm)

  # on génère une expression regex pour tester cela :
  # sélectionne les début de chaine qui commence par le nom d'une variable suivi d'un _
  patterne <- paste0("^(", paste0(variables.chr, collapse = "|"), ")_")

  output$modalites <- gsub(patterne, "", output$modalites)

  output
}
