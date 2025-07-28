#' Extraire les résultats d'une classification ascendante hiérarchique
#'
#' Lorsque l'on réalise une classification ascendante hiérarchique, et que
#' l'on souhaite utiliser les résultats, on a souvent envie d'avoir ces résultats
#' acollés à notre tableau d'origine.
#'
#' Cette fonction permet d'extraire d'un résultat d'une classification réalisé
#' avec [FactoMineR::HCPC()] le tableau de données d'origines enrichi d'une
#' colonne qui précise à quelle classe appartient chaque individus.
#'
#' Le jeu de données comporte également les colonnes des résultats de l'ACM.
#' Pour plus de détails sur ces colonnes, voir [extraire_individus()].
#'
#' Le principal intérêt de cette fonction est de permettre de générer un tableau
#' prêt à être utilisé par [ggplot2::ggplot()] pour en faire un graphique.
#'
#' @param resultat_classification Un objet qui contient le résultat d'une
#'  classification réalisée par [FactoMineR::HCPC()].
#'
#' @param ... Colonnes placées en début de tableau. Argument inutile.
#'
#' @returns Le tableau de donnée initial utilisé pour l'ACM auquel a été rajouté
#'  une colonne `classe` qui renseigne la classe des individus. Les colonnes
#'  de résultat de l'ACM telle que décrite dans [extraire_individus()] sont
#'  également incluses.
#'
#' @export
#'
#' @examples
#'
#' # 1. On réalise une ACM avec ACM() ou FactoMineR::MCA()
#' resultat_acm <- createurs_de_contenus_pol |> ACM()
#'
#' # 2. On fait la classification sur le résultat de l'ACM
#' resultat_classification <- resultat_acm |> FactoMineR::HCPC(nb.clust = 2)
#'
#' # 3. On peut extraire du résultat de la classification les classes.
#' resultat_classification |> extraire_classes()
extraire_classes <- function(resultat_classification, ...){

  # tests ---------------------------------------

  # prendre le nom de l'objet qui est passé comme argument
  objet <- as_label(enquo(resultat_classification))


  if (inherits(resultat_classification, "data.frame")) {
    cli::cli_abort(c(
      "{.arg resultat_classification} doit être un résultat de {.fun FactoMineR::HCPC}.",
      "x" = "L'argument {.arg resultat_classification} fourni est un tableau de donnée {.cls {class(resultat_classification)}}.",
      "i" = "{.var {objet}} est-il effectivement un résultat de {.fun FactoMineR::HCPC} ?"
    ))
  }

  if (inherits(resultat_classification, "MCA")) {
    cli::cli_abort(c(
      "{.arg resultat_classification} doit être un résultat de {.fun FactoMineR::HCPC}.",
      "x" = "L'argument {.arg resultat_classification} fourni est un resultat de {.fun FactoMineR::MCA} {.cls {class(resultat_classification)}}.",
      "i" = "{.var {objet}} est-il effectivement un résultat de {.fun FactoMineR::HCPC} ?"
    ))
  }

  # fin tests ------------------------------

  classif.df <- resultat_classification$data.clust |>
    supprimer_var_() |>
    tibble::as_tibble(rownames = "id") |>
    select(id, clust)
  # On pourrait s'arrêter là, mais on souhaite également avoir les dimensions
  # et les contributions de l'ACM

  data_acm <- resultat_classification$call$t$res |>
    extraire_individus() |>
    tibble::as_tibble(rownames = "id")

  # on peut maintenant joindre les deux :
  output <- data_acm |> left_join(classif.df)

  # et enfin on renomme la colonne
  output |>
    rename(classe = clust) |>
    select(-id) |>
    relocate(classe, ...)

}
