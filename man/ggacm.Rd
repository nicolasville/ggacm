% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggacm.R
\name{ggacm}
\alias{ggacm}
\title{Visualiser les résultats d'une ACM}
\usage{
ggacm(data, mapping = aes(dim1_coord, dim2_coord), ...)
}
\arguments{
\item{data}{Le résultat d'une ACM après qu'il soit passé par la fonction \code{\link[=get_individus]{get_individus()}} ou \code{\link[=get_variables]{get_variables()}}.}

\item{mapping}{Par défaut prend les coordonnée de la dimension 1 en abcisse (\code{x = dim1coord}) et de la dimension 2 en ordonné (\code{y = dim2coord}). Il est possible d'utiliser d'autres aesthetics. Pour plus d'information voir la vignette et la documentation de ggplot. Cet argument doit toujours être écrit avec la fonction \code{\link[=aes]{aes()}} comme : \code{aes(x = ..., y = ..., label = ..., etc.)}}

\item{...}{Arguments optionnels passés à \code{\link[=coord_acm]{coord_acm()}} qui seront passé à \code{\link[ggplot2:coord_fixed]{ggplot2::coord_fixed()}}.}
}
\value{
un graphique ggplot
}
\description{
Visualiser les résultats d'une ACM
}
\examples{
# un exemple pas piqué des annetons
}
