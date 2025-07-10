#' Visualiser les positions des individus en fonction des modalités
#'
#' Cette fonction permet de faire apparaître plusieurs fois le nuage d'individu en colorant à chaque fois les individus qui partagent une même modalité.
#'
#' @param resultat_individus Un tableau de donnée avec les coordonnées des individus
#'   qui a été produit par extraire_individus().
#'  La fonction prend également en charge un résultat d'ACM directement si besoin.
#' @param variables_a_representer La liste des variables à représenter :
#'  on si on souhaite sélectionner le `genre`, la `classe` et le `niveau_de_revenu`
#'  on peut par exemple écire `c(genre, classe, niveau_de_revenu)`
#' @param forme Quelles formes pour les individus qui partagent une même modalité ?
#'  Par défaut un triangle. Doit-être un nombre, voir la documentation de ggplot
#'  sur les "shapes" pour les possibilités.
#' @param couleur Quelle couleur doit-être utilisée ?
#'  doit-être une chaiîne de caractère qui encode la couleur comme "#e41ac" ou "yellow"
#'  par exemple.
#' @param longueur_max_labels Longeur maximum des labels de chaque facette.
#'  Par défaut chaque label ira à la ligne au bout de 20 caractères.
#' @param ... Arguments supplémentaires pour les utilisateurs avancés
#'  qui seront passés à ggplot2::facet_wrap()
#'
#' @returns Un objet ggplot2.
#' @export
#'
#' @examples
#'
#' res.ACM <- createurs_de_contenus_pol |> ACM()
#'
#' res.ind <- res.ACM |> extraire_individus()
#'
#' res.ind |>
#' nuage_individus_facette(variables_a_representer = c(CNC, partenariat_entreprise))
nuage_individus_facette <- function(resultat_individus,
                           variables_a_representer,
                           forme = 17,
                           couleur = "#e41a1c",
                           longueur_max_labels = 20,
                           ...) {

  if(resultat_individus |> inherits("MCA")){
    resultat_individus <- resultat_individus |> extraire_individus()
  }

  if(!all(c("dim1_coord", "dim2_coord") %in% names(resultat_individus))) {
    cli::cli_abort(c(
      "x" = "Les colonnes {.var dim1_coord} et {.var dim2_coord} n'ont pas été trouvé dans le tableau.",
      "i" = "Les colonnes avec les coordonnées des individus doivent porter ces noms.",
      "i" = "Peut-être vous n'avez pas passé un tableau produit par {.fun extraire_individus} ?"
    ))
  }


  # 1. Capturer le nom des variables à représenter -------------------

  quosure <- enquo(variables_a_representer)

  # Evaluate the tidyselect expression to get the column names
  variables <- tidyselect::eval_select(quosure, resultat_individus)
  variables <- names(variables)


  # 2. Modifier le tableau de donnée pour la représentation ----------

  data_representation <- resultat_individus |>
    select(dim1_coord, dim2_coord, any_of(variables)) |>
    pivot_longer(
      cols = !c(dim1_coord, dim2_coord),
      names_to = "variables",
      values_to = "modalites"
    )


  # 3. Produire le graphique -----------------------------------------

  data_representation |>
    ggacm() +
    ggplot2::geom_point(
      data = resultat_individus,
      aes(dim1_coord, dim2_coord),
      color = "darkgrey",
      show.legend = FALSE
    ) +
    geom_point(
      color = couleur,
      shape = forme,
      taille = 2
    ) +
    facet_wrap(
      ~ modalites,
      labeller = labeller(modalites = label_wrap_gen(width = longueur_max_labels)),
      ...
    )
}
