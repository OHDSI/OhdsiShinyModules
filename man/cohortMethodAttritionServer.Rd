% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohort-method-attrition.R
\name{cohortMethodAttritionServer}
\alias{cohortMethodAttritionServer}
\title{The module server for rendering the PLE attrition results}
\usage{
cohortMethodAttritionServer(
  id,
  selectedRow,
  connectionHandler,
  resultDatabaseSettings
)
}
\arguments{
\item{id}{the unique reference id for the module}

\item{selectedRow}{the selected row from the main results table}

\item{connectionHandler}{the connection to the PLE results database}

\item{resultDatabaseSettings}{a list containing the result schema and prefixes}
}
\value{
the PLE attrition results content server
}
\description{
The module server for rendering the PLE attrition results
}
