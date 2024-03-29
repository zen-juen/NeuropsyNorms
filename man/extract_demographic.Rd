% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_demographic.R
\name{extract_demographic}
\alias{extract_demographic}
\title{Extract Demographic Variables}
\usage{
extract_demographic(education, age, male, source)
}
\arguments{
\item{education}{Education in years.}

\item{age}{Age in years.}

\item{male}{TRUE or FALSE.}

\item{source}{Can be `Tombaugh1999_Canada` etc.}
}
\value{
A list of tidied demographic variables to be passed into norming functions.
}
\description{
Tidy demographic variables related to age, education and gender according to normative samples used.
}
\examples{
demo <- extract_demographic(education=10, age=50, male=TRUE, source="Tombaugh1999_Canada");
}
