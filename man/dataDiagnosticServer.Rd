% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-diagnostic-main.R
\name{dataDiagnosticServer}
\alias{dataDiagnosticServer}
\title{The module server for exploring data-diagnostic}
\usage{
dataDiagnosticServer(
  id = "dataDiag",
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
)
}
\arguments{
\item{id}{the unique reference id for the module}

\item{connectionHandler}{a connection to the database with the results}

\item{resultDatabaseSettings}{a list containing the data-diagnostic result schema}
}
\value{
The server for the data-diagnostic module
}
\description{
The module server for exploring data-diagnostic
}
\details{
The user specifies the id for the module
}
\seealso{
Other DataDiagnostics: 
\code{\link{dataDiagnosticDrillServer}()},
\code{\link{dataDiagnosticDrillViewer}()},
\code{\link{dataDiagnosticHelperFile}()},
\code{\link{dataDiagnosticSummaryServer}()},
\code{\link{dataDiagnosticSummaryViewer}()},
\code{\link{dataDiagnosticViewer}()}
}
\concept{DataDiagnostics}
