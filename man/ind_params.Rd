% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{ind_params}
\alias{ind_params}
\title{Socioeconomic Parameters}
\usage{
ind_params(N_individuals, mu)
}
\arguments{
\item{N_individuals}{Number of individuals}

\item{mu}{A vector of the mean of the constant parameters to be assigned}
}
\description{
This function assigns constant socioeconomic parameters to each individual. Further improvements are needed to enable this function to be able to assign random, correlated or uncorrelated parameters.
}
\examples{
ind_params(N_individuals = 1000, mu = c(2.2, -1.4, 3.5, 4))

}
