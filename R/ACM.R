#' Réaliser une ACM
#'
#' Cette fonction permet de réaliser une ACM en se reposant sur le package FactoMineR. Elle sera sûrement plus simple à utiliser que [FactoMineR::MCA()].
#'
#' @param donnees Les données de l'ACM
#' @param variables_supplementaires La liste des variables supplémentaires. Si vous voulez sélectionner plusieurs variables, vous pouvez les combinez par avec `c(variable1, variable2)`. Pour plus de détails sur les manières de sélectionner des colonnes dans cet argument, voir [tidyselect::tidyselect-package].
#' @param graph Afficher les graphes de la
#' @param ... Arguments supplémentaires qui seront passé à [FactoMineR::MCA].
#'
#' @returns Un résultat d'ACM.
#' @export
#'
#' @examples
#' createurs_de_contenus_pol |> ACM(variables_supplementaires = c(titre, personne))
#'
#'
ACM <- function(donnees, variables_supplementaires, graph = FALSE,...) {

  if (missing(variables_supplementaires)) {
    variables_supplementaires.chr <- NULL

    cli::cli_inform(
      "Toutes les variables sont actives",
      "i" = "Vous pouvez préciser des variables supplémentaires avec l'argument {.arg variables supplementaires}.")
  } else{
    quosure <- enquo(variables_supplementaires)

    # Evaluate the tidyselect expression to get the column names
    variables_supplementaires <- tidyselect::eval_select(quosure, donnees)
    variables_supplementaires.chr <- names(variables_supplementaires)

    variables_actives.chr <- donnees[, !names(donnees) %in% variables_supplementaires.chr] |> names()

    cli::cli_h2("Variables actives")
    cli::cli_inform(
      "L'ACM a été réalisé sur ces variables actives : {.var {variables_actives.chr}}."
      )

    cli::cli_h2("Variables supplémentaires")
    cli::cli_inform(
      "Les variables supplémentaires sont les suivantes : {.var {variables_supplementaires.chr}}."
      )
    }

  FactoMineR::MCA(donnees,
                  quali.sup = variables_supplementaires.chr,
                  graph = graph,
                  ...)
}
