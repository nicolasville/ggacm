#' Décrire une classe
#'
#' @param classification une classification
#' @param n_classe Le nombre de personnes présentes dans la classe
#' @param numero_classe Le numéro de la classe
#'
#' @returns Un
#' @export
#'
#' @import gtsummary
#' @import scales
#' @examples # un exemple
#'
tableau_classe <- function(classification,
                           n_classe,
                           numero_classe) {

  # fonction pour transformer la matrice en tibble
  nettoyer <- function(x) {
    as_tibble(x, rownames = "modalites") |>
      mutate(
        modalites = str_remove(modalites, ".+=")
      )
  }

  # transformer le tableau qui nous intéresse en tibble
  lien_classe_axes <- classification$desc.axes$quanti |> map(nettoyer)


  lien_classe_axes |>
    # extraire la classe qui nous intéresse
    purrr::chuck(numero_classe) |>
    # créer les colonnes à partir du résultat
    mutate(
      dans_la_classe = paste0(
        `Mod/Cla` / 100 * n_classe, # = n dans la classe
        " (",
        `Mod/Cla` |> round(digits = 0), # = % dans la classe
        " %)"
      ),
      Ensemble = paste0(
        Global / 100 * nrow(resultat_individus), # = n sur le corpus
        " (",
        Global |> round(digits = 0), # % dans le corpus
        " %)"
      ),
      dans_l_ensemble = paste0(
        `Cla/Mod` / 100 * Global / 100 * nrow(resultat_individus),  # n ayant partageant la modalité
        "/",
        Global / 100 * nrow(resultat_individus), # = n sur le corpus
        " (",
        `Cla/Mod` |> round(digits = 0), # %
        " %)"
      ),
      p.value = scales::label_pvalue(decimal.mark = ",")(p.value),
      .keep = "unused"
    ) |>
    # Les colonnes importantes d'abord__________________________________
    relocate(v.test, p.value, .after = everything()) |>
    # quelques modifications pour la lecture __________________________
    as_gtsummary() |>
    modify_header(
      modalites = "**Modalités**",
      dans_la_classe = paste0("**Composition de la classe**\nN = ", n_classe),
      Ensemble = paste0("**Dans l'ensemble**\nN = ", nrow(acm_df)),
      dans_l_ensemble = "**Part des membres de la classe**",
      v.test = "**V-Test**",
      p.value = "**P-Valeur**"
    ) |>
    # éclaircissement des notes de bas de tableau ______________________
    modify_footnote_header("Nombre et part des membres de la classe partageant la modalité",
                           columns = dans_la_classe
    ) |>
    modify_footnote_header("Nombre et part des membres de l'ensemble des individus partageant la modalité",
                           columns = Ensemble
    ) |>
    modify_footnote_header("Part des membres de la classe au sein des individus partageant la même modalité",
                           columns = dans_l_ensemble
    )
}

