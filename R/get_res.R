#' Title
#'
#' @param resultat_acm_var Résultat de l'ACM avec Factominer
#' @param data_acm Tableau original
#' @param active TRUE ou FALSE. TRUE pour extraire les variables actives. FALSE pour extraire les variables supplémentaires
#'
#' @returns pour chaque modalité les coordonnées, les contributions, les vtest et les cosinus carrés pour les 5 premiers axes.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import purrr
#'
#' @examples
#' # une liste d'exemple
get_res <-
  function(resultat_acm_var, # soit resultat_acm$var ou resutat_acm$sup
           data_acm,
           active = TRUE) {
    frequences <- get_frequence(data_acm)

    # une fonction pour faire en masse ce qu'on va faire pour coord, contrib cos2 et vtest :
    matrice_a_df <- function(resultat_acm_varSqqchse, quoi){
      resultat_acm_varSqqchse |> # prend la matrice
        round(digits = 2) |> # arrondi tout à 0.01
        as.data.frame() |> # la transforme en df
        rename_all(tolower) %>% # met tout en minuscule
        rename_all(~ str_replace(., " ", "")) %>% # supprime les espaces des noms de colonnes
        rename_all(~ str_c(., quoi, sep = "_")) %>% # colle un suffixe en fonction de ce que c'est
        mutate(modalites = rownames(.)) # transforme les noms de lignes en variable
    }

    # à appliquer pour les :
    # coordonnées -------
    coordonnees <- resultat_acm_var$coord |> matrice_a_df(quoi = "coord")

    # Cosinus carrés ----
    cos2 <- resultat_acm_var$cos2 |> matrice_a_df(quoi = "cos2")

    # vtest  ----
    vtest <- resultat_acm_var$v.test |> matrice_a_df(quoi = "vtest")

    # qu'on combine ensuite dans un tableau
    output <- coordonnees |>
      left_join(cos2, by = "modalites") |>
      left_join(vtest, by = "modalites")

    # on y ajoute les contributions si l'on doit extraire les variables actives
    if (active) {
      contributions <- resultat_acm_var$contrib |> matrice_a_df(quoi = "contrib")
      output <- output %>%
        left_join(contributions, by = "modalites")
    }

    # parfois FactoMineR met en préfixe le nom des variables. Il faut donc les enlever
    variables.chr <- names(data_acm)

    # on génère une expression regex pour tester cela :
    # sélectionne les début de chaine qui commence par le nom d'une variable suivi d'un _
    patterne <- paste0("^(", paste0(variables.chr, collapse = "|"), ")_")

    output$modalites <- gsub(patterne, "", output$modalites)

    # on peut ensuite rajouter les fréquences
    output <- output %>%
      left_join(frequences, by = "modalites") |>
      mutate(type = ifelse(active,
                           "variable active",
                           "variable supplémentaire")) %>%
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
    output
  }


