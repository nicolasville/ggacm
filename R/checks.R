

#' Vérifer ce que l'utilisateurs donne
#'
#' @param x l'objet à vérifer
#' @param classe le <chr> qui identifie la classe
#' @param message Le message d'erreur à afficher
#' @param complement Un complément d'information pour aguiller l'utilisaeur
#'  (optionnel)
#' @param call Pour que l emessage d'erreur mentionne la fonction que
#'  l'utilisateur a appelé
#' @param arg Pour choper le nom de l'argument dans lequel la fonction a été
#'  utilisé.
#'
#' @returns Rien du tout, mais un message d'erreur si ça foire
#'
#' @examples
#'
#'
#'
#'
check_chr_scalar <- function(
    x,
    complement = NULL,
    call = rlang::caller_env(),
    arg = rlang::caller_arg(x),
    ...
) {

  # si tout se passe bien on sort de la fonction
  if (is.character(x) & length(x) == 1) return(invisible(NULL))

  # 1. Mettre en place l'infrastructure pour générer le message d'erreur :
  ## 1. Capturer l'expression de l'utilisateur qui pose pb ----------------

  # on caputre le call de la fonction user_facing_fn(resultat_acm = blabla)
  appel_fn <- rlang::caller_call() |>
    # on s'assure que le call de la fonction soit bien construit
    rlang::call_match(rlang::caller_fn(), defaults = TRUE)

  # on extrait l'expression utilisé par l'utilisateur pour l'argument resultat_acm
  nom_arg_utilisateur <- appel_fn[[arg]] |> deparse()

  # on crée un environnement combiné pour que cli sache ou trouver
  # les expressions (soit dans cet environnement, soit dans celui plus haut)
  combined_env <- rlang::env_bury(
    rlang::caller_env(),
    x = x,
    arg = arg,
    nom_arg_utilisateur = nom_arg_utilisateur,
    ...
  )

  if (!is.character(x)) {

    message <- c(
      "!" = "L'argument {.arg {arg}} doit être de type {.cls character}.",
      "x" = "{.arg {nom_arg_utilisateur}} est un objet de type {.cls {class(x)}}."
    )

  }

  if (length(x) > 1) {

    message <- c(
      message,
      c(
        "!" = "L'argument {.arg {arg}} doit être de longueur 1.",
        "x" = "{.arg {nom_arg_utilisateur}} contient plusieurs valeurs.",
        "i" = "Avez-vous passez une liste ou un vecteur par erreur ?")
    )
  }

  # on déclenche le message d'erreur
  cli::cli_abort(
    message,
    call = call,
    .envir = combined_env
  )
}



check_inherits <- function(
    x,
    classe,
    complement = NULL,
    message = c("L'argument {.arg {arg}} doit être de type {.cls {classe}}.",
                "x" = "{.arg {nom_arg_utilisateur}} est un objet de type {.cls {class(x)}}."),
    call = rlang::caller_env(),
    arg = rlang::caller_arg(x),
    ...
    ) {


  if (!x |> inherits(classe)) {

    ## 1. Capturer l'expression de l'utilisateur qui pose pb ----------------

    # on caputre le call de la fonction user_facing_fn(resultat_acm = blabla)
    appel_fn <- rlang::caller_call() |>
      # on s'assure que le call de la fonction soit bien construit
      rlang::call_match(rlang::caller_fn(), defaults = TRUE)

    # on extrait l'expression utilisé par l'utilisateur pour l'argument resultat_acm
    nom_arg_utilisateur <- appel_fn[[arg]]



    # on crée un environnement combiné pour que cli sache ou trouver
    # les expressions (soit dans cet environnement, soit dans celui plus haut)
    combined_env <- rlang::env_bury(
      rlang::caller_env(),
      x = x,
      classe = classe,
      arg = arg,
      nom_arg_utilisateur = nom_arg_utilisateur,
      ...
    )

    cli::cli_abort(
      c(message, complement),
      call = call,
      .envir = combined_env
    )
  }
}

check_df <- function(
    x,
    classe = "data.frame",
    complement = NULL,
    message = c("L'argument {.arg {arg}} doit être un tableau de données {.cls {classe}}.",
                "x" = "{.arg {nom_arg_utilisateur}} est un objet de type {.cls {class(x)}}."),
    call = rlang::caller_env(),
    arg = rlang::caller_arg(x),
    ...
) {


  if (!x |> inherits(classe)) {

    ## 1. Capturer l'expression de l'utilisateur qui pose pb ----------------

    # on caputre le call de la fonction user_facing_fn(resultat_acm = blabla)
    appel_fn <- rlang::caller_call() |>
      # on s'assure que le call de la fonction soit bien construit
      rlang::call_match(rlang::caller_fn(), defaults = TRUE)

    # on extrait l'expression utilisé par l'utilisateur pour l'argument resultat_acm
    nom_arg_utilisateur <- appel_fn[[arg]]



    # on crée un environnement combiné pour que cli sache ou trouver
    # les expressions (soit dans cet environnement, soit dans celui plus haut)
    combined_env <- rlang::env_bury(
      rlang::caller_env(),
      x = x,
      classe = classe,
      arg = arg,
      nom_arg_utilisateur = nom_arg_utilisateur,
      ...
    )

    cli::cli_abort(
      c(message, complement),
      call = call,
      .envir = combined_env
    )
  }
}

check_resultat_acm <- function(
    x,
    nom_arg_utilisateur,
    message = c("L'argument {.arg {arg}} doit être de type {.cls MCA}.",
                "x" = "{.var {nom_arg_utilisateur}} est un objet de type {.cls {class(x)}}."),
    complement = c("i" = "Est-ce bien un résultat de {.fn FactoMineR::MCA} ou de {.fn ACM} ?"),
    complement2 = NULL,
    call = rlang::caller_env(),
    arg = rlang::caller_arg(x),
    ...
) {

  if (!x |> inherits("MCA")) {

    ## 1. Capturer l'expression de l'utilisateur qui pose pb ----------------

    # on caputre le call de la fonction user_facing_fn(resultat_acm = blabla)
    appel_fn <- rlang::caller_call() |>
      # on s'assure que le call de la fonction soit bien construit
      rlang::call_match(rlang::caller_fn(), defaults = TRUE)

    # on extrait l'expression utilisé par l'utilisateur pour l'argument resultat_acm
    nom_arg_utilisateur <- appel_fn$resultat_acm

    ## 2. Générer l'erreur -------------------------------------------------
    cli::cli_abort(
      c(
        message,
        complement,
        complement2),
      call = call
    )
  }
}

check_manquant <- function(
    x,
    complement = c("i" = "L'avez-vous oublié ?"),
    message,
    call = rlang::caller_env(),
    arg = rlang::caller_arg(x)) {


  if (rlang::is_missing(x)) {
    cli::cli_abort(
      c("L'argument {.arg {arg}} n'a pas été fourni.", complement),
      call = call
    )
  }

}

