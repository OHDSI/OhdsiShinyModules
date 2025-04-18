% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/components-largeTableViewer.R
\name{largeTableView}
\alias{largeTableView}
\title{Large Table Component Viewer}
\usage{
largeTableView(
  id,
  pageSizeChoices = c(10, 25, 50, 100),
  selectedPageSize = 10,
  fullDownloads = TRUE
)
}
\arguments{
\item{id}{Shiny module id. Must match largeTableServer}

\item{pageSizeChoices}{numeric selection options for pages}

\item{selectedPageSize}{numeric selection options for pages}

\item{fullDownloads}{allow download button of full dataset from query}
}
\description{
Componenet for results sets with many thousands of rows
More limited than other table components in terms of automatic handling of search and
filtering but will allow responsive apps
}
\seealso{
Other LargeTables: 
\code{\link{createLargeSqlQueryDt}()},
\code{\link{largeTableServer}()}
}
\concept{LargeTables}
