% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bvmtr_norms.R
\name{bvmtr_norms}
\alias{bvmtr_norms}
\title{Computes Brief Visuospatial Memory Test - Revised (BVMT-R) norms.}
\usage{
bvmtr_norms(
  education,
  age,
  male = TRUE,
  source = "Lee2012_Singapore",
  t1 = NA,
  t2 = NA,
  t3 = NA,
  learning = NA,
  total = NA,
  delayed_recall = NA,
  percent_retained = NA,
  recog_correct = NA,
  recog_FA = NA,
  recog_discrimination = NA
)
}
\arguments{
\item{education}{Education in years.}

\item{age}{Age in years.}

\item{male}{TRUE or FALSE.}

\item{source}{Can be `Lee2012_Singapore` or ` Benedict1997_US`.}

\item{t1}{Trial 1 score}

\item{t2}{Trial 2 score}

\item{t3}{Trial 3 score}

\item{learning}{Learning score (higher of trial 2 or 3 - trial 1 score)}

\item{total}{Totals (Trials 1-3) score.}

\item{delayed_recall}{Delayed Free Recall score}

\item{percent_retained}{Amount of info retained after delay. (delayed recall score * 100) / higher of trial 2 or 3}

\item{recog_correct}{Recognition total number of correct items/hits.}

\item{recog_FA}{Recognition False Alarms.}

\item{recog_discrimination}{Recognition discrimination index (Recognition Hits - Recognition False Alarms).}
}
\value{
A table of standardised scores and their descriptors.
}
\description{
Computes Brief Visuospatial Memory Test - Revised (BVMT-R) norms.
}
\examples{
out1 <- bvmt_norms(education=10, age=55, t1=5, t2=5, t3=6, total=13, delayed_recall=10, source="Benedict1997_US");
}
