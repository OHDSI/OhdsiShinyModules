% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patient-level-prediction-covariateSummary.R
\name{patientLevelPredictionCovariateSummaryServer}
\alias{patientLevelPredictionCovariateSummaryServer}
\title{The module server for exploring prediction covariate summary results}
\usage{
patientLevelPredictionCovariateSummaryServer(
  id,
  modelDesignId,
  developmentDatabaseId,
  performanceId,
  connectionHandler,
  inputSingleView,
  resultDatabaseSettings
)
}
\arguments{
\item{id}{the unique reference id for the module}

\item{modelDesignId}{unique id for the model design}

\item{developmentDatabaseId}{unique id for the development database}

\item{performanceId}{unique id for the performance results}

\item{connectionHandler}{the connection to the prediction result database}

\item{inputSingleView}{the current tab}

\item{resultDatabaseSettings}{a list containing the result schema and prefixes}
}
\value{
The server to the covariate summary module
}
\description{
The module server for exploring prediction covariate summary results
}
\details{
The user specifies the id for the module
}
\seealso{
Other PatientLevelPrediction: 
\code{\link{patientLevelPredictionCalibrationServer}()},
\code{\link{patientLevelPredictionCalibrationViewer}()},
\code{\link{patientLevelPredictionCovariateSummaryViewer}()},
\code{\link{patientLevelPredictionCutoffServer}()},
\code{\link{patientLevelPredictionCutoffViewer}()},
\code{\link{patientLevelPredictionDesignSummaryServer}()},
\code{\link{patientLevelPredictionDesignSummaryViewer}()},
\code{\link{patientLevelPredictionDiagnosticsServer}()},
\code{\link{patientLevelPredictionDiagnosticsViewer}()},
\code{\link{patientLevelPredictionDiscriminationServer}()},
\code{\link{patientLevelPredictionDiscriminationViewer}()},
\code{\link{patientLevelPredictionHelperFile}()},
\code{\link{patientLevelPredictionModelSummaryServer}()},
\code{\link{patientLevelPredictionModelSummaryViewer}()},
\code{\link{patientLevelPredictionNbServer}()},
\code{\link{patientLevelPredictionNbViewer}()},
\code{\link{patientLevelPredictionServer}()},
\code{\link{patientLevelPredictionSettingsServer}()},
\code{\link{patientLevelPredictionSettingsViewer}()},
\code{\link{patientLevelPredictionValidationServer}()},
\code{\link{patientLevelPredictionValidationViewer}()},
\code{\link{patientLevelPredictionViewer}()}
}
\concept{PatientLevelPrediction}
