% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_variables.R
\name{get_variables}
\alias{get_variables}
\title{Obtenir un résultat tidy d'une ACM}
\usage{
get_variables(resultat_acm, data_acm)
}
\arguments{
\item{resultat_acm}{résultat sortie de factominer::MCA}

\item{data_acm}{le tableau de donnée d'origine}
}
\value{
un tableau avec pour chaque modalité les coordonnées, les contributions, les cos2 et les vtest pour les 5 premières dimensions
}
\description{
Obtenir un résultat tidy d'une ACM
}
\examples{
# une liste d'exemple
}
