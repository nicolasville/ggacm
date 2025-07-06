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

  message("Les options générales de la session R ont été modifié. Les nombres décimaux s'afficheront avec une virgule et uniquement 3 chiffres après la virgule. L'écriture scientifque a été désactivée.\n")

  gtsummary::theme_gtsummary_language(
    language = "fr",
    decimal.mark = ",",
  )
  message("Le thème des objets gtsummary a été modifié. Ils seront en français et afficheront les nombres décimaux avec une virgule ")
  }
