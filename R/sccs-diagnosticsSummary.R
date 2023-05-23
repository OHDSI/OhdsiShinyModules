# @file sccs-diagnosticsSummary
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


sccsDiagnosticsSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    resultTableViewer(ns("diagnosticsTable"))
  )
}



sccsDiagnosticsSummaryServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
    
        data <- getSccsAllDiagnosticsSummary(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
        
        customColDefs <- list(
          databaseName = reactable::colDef(
            header = withTooltip(
              "Database",
              "The database name"
            )
          ),
          exposure = reactable::colDef(
            header = withTooltip(
              "Exposure",
              "The exposure of interest "
            )
          ),
          outcome = reactable::colDef(
            header = withTooltip(
              "Outcome",
              "The outcome of interest "
            )
          ),
          analysis = reactable::colDef(
            header = withTooltip(
              "Analysis",
              "The analysis name "
            )
          ),
          covariateName = reactable::colDef(
            header = withTooltip(
              "Time Period",
              "The time period of interest"
            )
          ),
          mdrr = reactable::colDef(
            header = withTooltip(
              "mdrr",
              "The minimum detectible relative risk"
            )
          ),
          ease = reactable::colDef(
            header = withTooltip(
              "ease",
              "The ..."
            )
          ),
          timeTrendP = reactable::colDef(
            header = withTooltip(
              "timeTrendP",
              "The ..."
            )
          ),
          preExposureP = reactable::colDef(
            header = withTooltip(
              "preExposureP",
              "The ..."
            )
          ),
          mdrrDiagnostic = reactable::colDef(
            header = withTooltip(
              "mdrrDiagnostic",
              "The ..."
            )
          ),
          easeDiagnostic = reactable::colDef(
            header = withTooltip(
              "easeDiagnostic",
              "The ..."
            )
          ),
          timeTrendDiagnostic = reactable::colDef(
            header = withTooltip(
              "timeTrendDiagnostic",
              "The ..."
            )
          ),
          preExposureDiagnostic = reactable::colDef(
            header = withTooltip(
              "preExposureDiagnostic",
              "The ..."
            )
          ),
          
          unblind = reactable::colDef(
            header = withTooltip(
              "unblind",
              "If the value is 1 then the diagnostics passed and results can be unblinded"
            )
          )
          
        )
        
        resultTableServer(
          id = "diagnosticsTable",
          df = data,
          colDefsInput = customColDefs
        )
      
    }
  )
}
