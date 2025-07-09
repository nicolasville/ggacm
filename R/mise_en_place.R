#' Mettre en place une session R adapté
#'
#' R affiche par défaut beaucoup trop de chiffres après la virgule (qui est d'ailleurs un point en général) et tend à utiliser l'écriture scientifique... ce qui n'arrange pas vraiment les chercheurs en sciences sociales... Cette fonction permet de modifier ces options d'un seul coup. Un nombre qui s'affichait comme 0.456789 s'affichera alors comme 0,457.
#'
#' @param ... Il est possible de faire passer d'autres arguments à [base::options()].
#'
#' @returns Une liste d'option
#' @export
#'
#' @examples #
mise_en_place_ACM <- function(...){
  options(digits = 3, # arrondir deux chiffres après la virgule
        scipen = 999,# pas d'écriture scientifique
        OutDec = ",",
        ...) # marque des décimale : , et pas .


  cli::cli_h3("Les options générales de la session R ont été modifié.")
  cli::cli_inform(c(
    "v" = "Les nombres décimaux s'afficheront avec une virgule.",
    "v" = "Ils n'afficheront que 3 chiffres après la virgule.",
    "v" = "L'écriture scientifique a été désactivé."
    ))

  gtsummary::theme_gtsummary_language(
    language = "fr",
    decimal.mark = ",",
  ) |> suppressMessages()

  cli::cli_h3("Le thème des tableaux produit par {.pkg gtsummary} a été modifié.")
  cli::cli_inform(c(
    "v" = "Ils s'afficheront en français.",
    "v" = "En utilisant une virgule pour les nombres décimaux."
  )
  )
  }
