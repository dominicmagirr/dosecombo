% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_mono_prior.R
\name{create_mono_prior}
\alias{create_mono_prior}
\title{Create a prior for monotherapy dose response, similar to crmPack approach}
\usage{
create_mono_prior(doses, ref_dose, prior_medians, psuedo_n)
}
\arguments{
\item{doses}{set of doses considered}

\item{ref_dose}{reference dose for standardizing 'doses'.}

\item{prior_medians}{Guesstimates for p(DLT) at 'doses'.}

\item{psuedo_n}{the equivalent number of observations at each dose. The larger this is, the more certainty in prior.}
}
\value{
A crmPack model object.
}
\description{
Create a prior for monotherapy dose response, similar to crmPack approach
}

