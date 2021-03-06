% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dose_grid.R
\name{dose_grid}
\alias{dose_grid}
\title{Create matrix of all possible dose combinations}
\usage{
dose_grid(d_orig, log_transform = FALSE, d_star = NULL)
}
\arguments{
\item{d_orig}{A list of two vectors. The first vector contains the possible dose levels for drug 1. The second vector contains the possible dose levels for drug 2.}

\item{log_transform}{If TRUE, then will output log-transformed dose levels.}

\item{d_star}{When log_transform is true, you must provide refence dose levels -- a vector of length 2.}
}
\value{
A matrix with two columns (dose1, dose2) and number of rows equal to the number of combinations
}
\description{
Provided with the possible levels of each dose, this function gives you all possible combinations.
}

