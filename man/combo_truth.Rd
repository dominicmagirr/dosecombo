% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combo_truth.R
\name{combo_truth}
\alias{combo_truth}
\title{Produce a vector of "true" p(DLT) at each dose combination}
\usage{
combo_truth(doses, ref_doses = c(75, 40), ref_probs = c(0.25, 0.2),
  other_doses = c(25, 10), other_probs = c(0.005, 0.005),
  odds_multiplier = 9)
}
\arguments{
\item{doses}{A list containing doses on the original and log scale.}

\item{ref_doses}{Vector of length 2. Doses of agent 1 and agent 2 used as reference for standardization. Does not have to match the reference doses in the 'doses' object.}

\item{ref_probs}{Vector of length 2. Probability of DLT at "ref_doses".}

\item{other_doses}{Vector of length 2. Doses of agent 1 and agent 2. Any choices valid, apart from the "ref_doses".}

\item{other_probs}{Vector of length 2. Probability of DLT at "other_doses".}

\item{odds_multiplier}{Interaction effect, equivalent to exp(eta). In the model, the odds of DLT at "ref_doses" gets multiplied by "odds_multiplier".}
}
\value{
A vector containing p(DLT) at each of the dose combinations in "doses".
}
\description{
Produce a vector of "true" p(DLT) at each dose combination
}

