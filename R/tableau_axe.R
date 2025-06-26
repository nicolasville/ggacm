tableau_axe <- function(x, resultats_max = 30, numero_axe){

  dim <- numero_axe

  dim.chr <- paste0("dim", dim)

  x |>
  head(n = nb) |>
  select(modalites,
         n,
         pourcentage,
         starts_with(dim.chr)) |>
  rename(
    `Modalité` = modalites,
    `%` = pourcentage,
    Contribution = contains(paste0("dim", dim, "_contrib")),
    Coordonnée = contains((paste0("dim", dim, "_coord"))),
    `Cos²` = contains((paste0("dim", dim, "_cos2"))),
    `V-test` = contains((paste0("dim", dim, "_vtest")))
  )

}
