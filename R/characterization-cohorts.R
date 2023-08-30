# @file characterization-timeToEvent.R
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


#' The module viewer for exploring 1 or more cohorts features
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#'
#' @return
#' The user interface to the description cohorts features
#'
#' @export
characterizationTableViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(

    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("characterization-www", "help-targetViewer.html", package = utils::packageName())
    ),
    
    
    # module that does input selection for a single row DF
    inputSelectionViewer(
      id = ns("input-selection")
    ),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      resultTableViewer(ns("result-table"))
    )
  )
}


#' The module server for exploring 1 or more cohorts features
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param mainPanelTab the current tab
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#'
#' @return
#' The server to the cohorts features server
#'
#' @export
characterizationTableServer <- function(
    id,
    connectionHandler,
    mainPanelTab,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      inputVals <- getDecCohortsInputs(
        connectionHandler,
        resultDatabaseSettings
      )
      
      # input selection component
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 4,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = inputVals$cohortIds,
              selected = inputVals$cohortIds,
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 3,
            varName = 'databaseId',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Database: ',
              choices = inputVals$databaseIds,
              selected = 1,
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
          
      )
      )
      
    
      allData <- shiny::reactive({
        getDesFEData(
              targetIds = inputSelected()$targetIds,
              databaseId = inputSelected()$databaseId,
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings
            )
          })
      
      #cols: covariateId, covariateName, analysisName,
      #averageValue_"target", countValue_"target"
      
      custom_colDefs <- list(
        covariateId = reactable::colDef(
          header = withTooltip("Covariate ID",
                               "Unique identifier of the covariate")
        ),
        covariateName = reactable::colDef(
          header = withTooltip(
            "Covariate Name",
            "The name of the covariate"
          )
        ),
        analysisName = reactable::colDef(
          header = withTooltip(
            "Covariate Class",
            "Class/type of the covariate"
          )
        )
      )
      
      resultTableServer(
        id = "result-table",
        df = allData,
        colDefsInput = custom_colDefs
      )

      return(invisible(NULL))
      
    })
  
}


getDesFEData <- function(
    targetIds,
    databaseId,
    connectionHandler,
    resultDatabaseSettings
) {
  #  shiny::withProgress(message = 'Getting target comparison data', value = 0, {
  
  if(is.null(targetIds)){
    return(NULL)
  }
  if(is.null(databaseId)){
    return(NULL)
  }
  sql <-
    "select distinct ref.covariate_id, ref.covariate_name, an.analysis_name, c.cohort_name, covs.COUNT_VALUE, covs.AVERAGE_VALUE
  from
  (
  select co.RUN_ID, cd.TARGET_COHORT_ID as COHORT_DEFINITION_ID, co.COVARIATE_ID,
  co.SUM_VALUE as COUNT_VALUE,	co.AVERAGE_VALUE*100 as AVERAGE_VALUE from
   @schema.@c_table_prefixCOVARIATES co
   inner join
   (select * from @schema.@c_table_prefixcohort_details
   where DATABASE_ID = '@database_id' and
   TARGET_COHORT_ID in (@cohort_ids) and COHORT_TYPE = 'T'
   ) as cd
   on co.COHORT_DEFINITION_ID = cd.COHORT_DEFINITION_ID
   and co.DATABASE_ID = cd.DATABASE_ID
  union
  select cc.RUN_ID, cds.TARGET_COHORT_ID as COHORT_DEFINITION_ID, cc.COVARIATE_ID,	cc.COUNT_VALUE,	cc.AVERAGE_VALUE from
    @schema.@c_table_prefixCOVARIATES_continuous cc
    inner join
    (select * from @schema.@c_table_prefixcohort_details
   where DATABASE_ID = '@database_id' and
   TARGET_COHORT_ID in (@cohort_ids) and COHORT_TYPE = 'T'
   ) as cds
   on cc.COHORT_DEFINITION_ID = cds.COHORT_DEFINITION_ID
    and cc.DATABASE_ID = cds.DATABASE_ID
  ) covs
  inner join
  @schema.@c_table_prefixcovariate_ref ref
  on covs.RUN_ID = ref.RUN_ID and
  covs.COVARIATE_ID = ref.COVARIATE_ID
  inner join @schema.@c_table_prefixanalysis_ref an
  on an.RUN_ID = ref.RUN_ID and
  an.analysis_id = ref.analysis_id
  inner join @schema.@cg_table_prefixcohort_definition c
  on c.cohort_definition_id = covs.COHORT_DEFINITION_ID
  ;
  "
  
  #  shiny::incProgress(1/3, detail = paste("Created SQL - Extracting..."))
  
  resultTable <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    cohort_ids = paste(as.double(targetIds), collapse = ','),
    database_id = databaseId
  )
  
  #  shiny::incProgress(2/3, detail = paste("Formating"))
  
  #format
  resultTable$averageValue <- round(
    x = resultTable$averageValue, 
    digits = 2
    )
  
  resultTable <- resultTable %>%
    tidyr::pivot_wider(
      names_from = "cohortName",
      #.data$cohortName,
      values_from = c("averageValue", "countValue"),
      #c(.data$averageValue, .data$countValue),
      id_cols = c("covariateId", "covariateName", "analysisName") #c(.data$covariateId, .data$covariateName, .data$analysisName)
    )
  
  resultTable$analysisName <- as.factor(resultTable$analysisName)
  
  #  shiny::incProgress(3/3, detail = paste("Done"))
  
  # })
  
  return(resultTable)
}


getDecCohortsInputs <- function(
    connectionHandler,
    resultDatabaseSettings
) {
  #shiny::withProgress(message = 'Getting target comparison inputs', value = 0, {
  
  
  sql <-
    ' select distinct c.cohort_definition_id, c.cohort_name from
  @schema.@cg_table_prefixcohort_definition c
  inner join
  (select distinct TARGET_COHORT_ID as id
  from @schema.@c_table_prefixcohort_details
  ) ids
  on ids.id = c.cohort_definition_id
  ;'
  
  #shiny::incProgress(1/4, detail = paste("Extracting targetIds"))
  
  idVals <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  ids <- idVals$cohortDefinitionId
  names(ids) <- idVals$cohortName
  
  #shiny::incProgress(2/4, detail = paste("Extracted targetIds"))
  
  
  sql <- 'select d.database_id, d.cdm_source_abbreviation as database_name
  from @schema.@database_table d;'
  
  #shiny::incProgress(3/4, detail = paste("Extracting databaseIds"))
  
  database <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    database_table = resultDatabaseSettings$databaseTable
  )
  databaseIds <- database$databaseId
  names(databaseIds) <- database$databaseName
  
  #shiny::incProgress(4/4, detail = paste("Done"))
  
  #  })
  
  return(
    list(
      cohortIds = ids,
      databaseIds = databaseIds
    )
  )
  
}
