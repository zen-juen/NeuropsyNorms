% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cowat_norms.R
\name{cowat_norms}
\alias{cowat_norms}
\title{Computes Controlled Oral Word Association Test (COWAT) norms.}
\usage{
cowat_norms(
  education,
  age,
  male = TRUE,
  source = "Tombaugh1999_Canada",
  fas_score = NA,
  animals_score = NA
)
}
\arguments{
\item{education}{Education in years.}

\item{age}{Age in years.}

\item{male}{TRUE or FALSE.}

\item{source}{Can be `Tombaugh1999_Canada` or ` Whittle2007_US`.}

\item{fas_score}{FAS score.}

\item{animals_score}{Animals score.}
}
\value{
A table of standardised scores and their descriptors.
}
\description{
Computes Controlled Oral Word Association Test (COWAT) norms.
}
\examples{
out1 <- cowat_norms(education=10, age=55, fas_score=NA, animals_score=20, source="Tombaugh1999_Canada";
out2 <- cowat_norms(education=10, age=91, fas_score=50, animals_score=NA, source="Whittle2007_US");
}
