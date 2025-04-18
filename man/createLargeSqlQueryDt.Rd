% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/components-largeTableViewer.R
\name{createLargeSqlQueryDt}
\alias{createLargeSqlQueryDt}
\title{Create Large Sql Query Data Table}
\usage{
createLargeSqlQueryDt(
  connectionHandler = NULL,
  connectionDetails = NULL,
  baseQuery,
  countQuery = NULL
)
}
\arguments{
\item{connectionHandler}{ResultModelManager connectionHandler instance}

\item{connectionDetails}{DatabaseConnector connectionDetails instance}

\item{baseQuery}{base sql query}

\item{countQuery}{count query string (should match query). Can be auto generated with sub query
(default) but this will likely result in slow results}
}
\description{
Construct an instance of a LargeDataTable R6 instance for use inside largeTableServer

This should pass a parameterized sql query that can be used to iteratively return data from a table
rather than returning the entire object.
}
\seealso{
Other LargeTables: 
\code{\link{largeTableServer}()},
\code{\link{largeTableView}()}
}
\concept{LargeTables}
