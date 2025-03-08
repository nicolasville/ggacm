#' Title
#'
#' @param resultat_acm_var Résultat de l'ACM avec Factominer
#' @param data_acm Tableau original
#' @param active TRUE ou FALSE. TRUE pour extraire les variables actives. FALSE pour extraire les variables supplémentaires
#'
#' @returns pour chaque modalité les coordonnées, les contributions, les vtest et les cosinus carrés pour les 5 premiers axes.
#' @export
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

    coordonnees <-
      as.data.frame(round(resultat_acm_var$coord, 2)) %>% # récupérer les coordonnées des modalités actives et arrondir à deux décimales (c'est bien suffisant)
      rename_all(tolower) %>% # tout en minuscules
      rename_all(~ str_replace(., " ", "")) %>% # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1"
      rename_all(~ str_c(., "coord", sep = "_")) %>% # ajouter le suffixe _coord à chaque nom de variable. On obtient ainsi par exemple "dim1_coord"
      mutate(modalites = rownames(.)) # récupérer les noms des modalités, stockées dans le nom des lignes de resultat_acm$var$coord

    # Contributions (modalités actives) ----
    if (active) {
      contributions <-
        as.data.frame(round(resultat_acm_var$contrib, 2))  %>%
        rename_all(tolower) %>%
        rename_all(~ str_replace(., " ", "")) %>%
        rename_all(~ str_c(., "contrib", sep = "_")) %>% # idem sauf qu'ici on obtient "dim1_contrib"
        mutate(modalites = rownames(.))
    }
    # Cosinus carrés (modalités actives) ----
    cos2 <- as.data.frame(round(resultat_acm_var$cos2, 2)) %>%
      rename_all(tolower) %>%
      rename_all(~ str_replace(., " ", "")) %>%
      rename_all(~ str_c(., "cos2", sep = "_")) %>% # idem avec "cos2"
      mutate(modalites = rownames(.))

    # vtest (modalités actives) ----
    vtest <- as.data.frame(round(resultat_acm_var$v.test, 2)) %>%
      rename_all(tolower) %>%
      rename_all(~ str_replace(., " ", "")) %>%
      rename_all(~ str_c(., "vtest", sep = "_")) %>% # idem avec vtest
      mutate(modalites = rownames(.))

    if (active) {
      frequences <- frequences %>%
        right_join(contributions)
    }

    output <- frequences %>%
      right_join(coordonnees)  %>%
      right_join(cos2) %>%
      right_join(vtest) %>% # fusionner les jeux de données ; la clé de fusion (implicite) est la variable "modalites", qui est commune à tous.
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
