#' Réaliser une ACM
#'
#' Cette fonction permet de réaliser une ACM en se reposant sur le package FactoMineR. Elle sera sûrement plus simple à utiliser que [FactoMineR::MCA()].
#'
#' @param donnees Les données de l'ACM
#' @param variables_supplementaires <[`tidy-select`][dplyr_tidy_select]>
#'  La liste des variables supplémentaires. Si vous voulez sélectionner
#'  plusieurs variables, vous pouvez les combinez par avec `c(variable1, variable2)`.
#'  Pour plus de détails sur les manières de sélectionner des colonnes dans
#'  cet argument, voir [dplyr::select()].
#' @param graph Faut-il afficher les graphiques produit par [FactoMineR::MCA()] ?
#'  par défaut sur `FALSE`.
#' @param ... Arguments supplémentaires qui seront passé à [FactoMineR::MCA].
#'
#' @returns Le résultat d'une ACM.
#' @export
#'
#' @examples
#' resultat <- createurs_de_contenus_pol |>
#'   ACM(variables_supplementaires = c(titre, personne))
#'
#'
ACM <- function(donnees, variables_supplementaires, graph = FALSE,...) {

  # prendre les noms
  donnees.nom <- as_label(enquo(donnees))

  check_df(donnees)



  # vérifier qu'aucune colonnes ne contiennent des données quantitatives :
  # Fin test

  if (missing(variables_supplementaires)) {

    variables_supplementaires.chr <- NULL

  } else{

    quosure <- enquo(variables_supplementaires)

    # Evaluate the tidyselect expression to get the column names
    variables_supplementaires <-  tidyselect::eval_select(quosure, donnees)
    variables_supplementaires.chr <- names(variables_supplementaires)

    variables_actives.chr <- donnees[, !names(donnees) %in% variables_supplementaires.chr] |> names()

    }



  cli::cli_progress_step(
    "Réalisation de l'ACM avec {.fn FactoMineR::MCA}",
    msg_failed = "Erreur  dans la réalisation de l'ACM avec {.fn FactoMineR::MCA}",
    msg_done = "L'ACM a été réalisé avec succès avec {.fn FactoMineR::MCA}")


  output <- FactoMineR::MCA(donnees,
                  quali.sup = variables_supplementaires.chr,
                  graph = graph,
                  ...)

  cli::cli_progress_step("")

  cli::cli_bullets(c(
    "*" = "Sur {nrow(donnees)} individus...",
    "*" = "et {length(donnees)} variables."
  ))

  if (missing(variables_supplementaires)) {
    cli::cli_inform(c(
      "*" = "Toutes les variables sont actives",
      "i" = "Vous pouvez préciser des variables supplémentaires avec l'argument {.arg variables supplementaires}."
    ))
  } else {
    cli::cli_h3("Variables actives : {length(variables_actives.chr)}")
    cli::cli_inform(
      "{.var {variables_actives.chr}}."
    )

    cli::cli_h3("Variables supplémentaires : {length(variables_supplementaires.chr)}")
    cli::cli_inform(
      "{.var {variables_supplementaires.chr}}."
    )
  }

  output
}
