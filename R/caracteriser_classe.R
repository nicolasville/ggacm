#' Caractériser une classification
#'
#' Cette fonction permet de présenter un tableau caractérisant une classification réalisé avec la fonction FactoMineR::HCPC()
#'
#' @param x Un objet qui contient le résultat d'une classification réalisée avec FactoMineR::HCPC().
#' @param n_classe Le numéro de la classe que l'on souhaite caractériser
#' @param data_acm Le tableau de donnée qui sur lequel l'ACM a été réalisée.
#'
#' @returns Un tableau prêt à l'exportation décrivant une classe.
#' @export
#' @import gtsummary
#' @examples
#' # un exemple
#'
tableau_classe <- function(x, n_classe, data_acm) {
  x |>
    mutate(
      dans_la_classe = paste0(
        `Mod/Cla` / 100 * n_classe, # = n dans la classe
        " (",
        `Mod/Cla` |> round(digits = 0), # = % dans la classe
        " %)"
      ),
      Ensemble = paste0(
        Global / 100 * nrow(data_acm), # = n sur le corpus
        " (",
        Global |> round(digits = 0), # % dans le corpus
        " %)"
      ),
      dans_l_ensemble = paste0(
        `Cla/Mod` / 100 * Global / 100 * nrow(data_acm),  # n ayant partageant la modalité
        "/",
        Global / 100 * nrow(data_acm), # = n sur le corpus
        " (",
        `Cla/Mod` |> round(digits = 0), # %
        " %)"
      ),
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
