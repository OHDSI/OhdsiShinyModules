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
      
      # add basic table 
      resultTableViewer(id = ns('mainTable'))
    
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
            columnWidth = 6,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = inputVals$cohortIds,
              selected = inputVals$cohortIds,
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
          ),
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'databaseIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Database: ',
              choices = inputVals$databaseIds,
              selected = inputVals$databaseIds[1],
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
            columnWidth = 6,
            varName = 'analysisIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Covariate Type: ',
              choices = inputVals$analysisIds,
              selected = inputVals$analysisIds[1],
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
      
      columns <- shiny::reactive({
        
        result <- list(
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
        
        if(is.null(inputSelected()$targetIds) | is.null(inputSelected()$databaseIds)){
          return(result) 
        } else{
          temp <- expand.grid(inputSelected()$targetIds,inputSelected()$databaseIds)
          temp[,2] <- as.double(as.character(temp[,2]))
          
          for(i in 1:nrow(temp)){
            
            targetName = names(inputVals$cohortIds)[temp[i,1] == inputVals$cohortIds]
            databaseName = names(inputVals$databaseIds)[temp[i,2] == inputVals$databaseIds]
            
            result[[length(result) + 1]] <- reactable::colDef(
              header = withTooltip(
                paste0("Count-", temp[i,1], '-',temp[i,2]),
                paste0("The number of patients in database ", databaseName, ' and target ', targetName, ' who has the covariate')
              )
            )
            
            names(result)[length(result)] <- paste0('countT', temp[i,1], 'D', ifelse(temp[i,2] <0, 'n', ''), abs(temp[i,2]) )
            
            result[[length(result) + 1]] <- reactable::colDef(
              header = withTooltip(
                paste0("Mean-", temp[i,1], '-', temp[i,2]),
                paste0("The mean covariate value for patients in database ", databaseName, ' and target ', targetName)
              ), 
              format = reactable::colFormat(
                digits = 3
              )
            )
            names(result)[length(result)] <- paste0('averageT', temp[i,1], 'D', ifelse(temp[i,2] <0, 'n', ''), abs(temp[i,2]) )
            
          }
          return(result) 
        }
        
      })
      
      #get results
        resultTable <- shiny::reactive({
          getCohortData(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = inputSelected()$targetIds,
            databaseIds = inputSelected()$databaseIds,
            analysisIds = inputSelected()$analysisIds
          )
        })

      resultTableServer(
        id = 'mainTable',
        df = resultTable,
        colDefsInput = columns()
      ) 

      return(invisible(NULL))
      
    })
  
}


getCohortData <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    databaseIds,
    analysisIds
){
  
  if(is.null(targetIds) | is.null(databaseIds)){
   return(NULL)
  }
  
  combinations <- expand.grid(targetIds, databaseIds)
  combinations[,2] <- as.double(as.character(combinations[,2]))
  
sql <- paste0(
"select ref.covariate_id, ref.covariate_name, an.analysis_name,",

paste(
  lapply(1:nrow(combinations), function(i){
    paste0(
"max(case when temp.selection_id = ",i," then temp.sum_value else 0 end) as count_t",combinations[i,1],'_d',ifelse(combinations[i,2] <0, 'n', ''),abs(combinations[i,2]),",",
"max(case when temp.selection_id = ",i," then temp.average_value else 0 end) as average_t",combinations[i,1],'_d',ifelse(combinations[i,2] <0, 'n', ''),abs(combinations[i,2])
)}), collapse = ','),

" from @schema.@c_table_prefixcovariate_ref ref
 inner join @schema.@c_table_prefixanalysis_ref an
 on an.RUN_ID = ref.RUN_ID and
  an.analysis_id = ref.analysis_id and
  ref.analysis_id in (@analysis_ids)
  

 left join

( ",
  
  
paste(
    lapply(1:nrow(combinations), function(i){
      paste0(
        "
  select 
  co",i,".run_id, co",i,".COVARIATE_ID, co",i,".SUM_VALUE,	co",i,".AVERAGE_VALUE,
  ",i," as selection_id
   from
  @schema.@c_table_prefixCOVARIATES co",i," 
  inner join
  (select * from @schema.@c_table_prefixcohort_details
    where DATABASE_ID = '@database",i,"' and
    TARGET_COHORT_ID = @target",i," and COHORT_TYPE = 'T'
  ) as cd",i,"
  on co",i,".COHORT_DEFINITION_ID = cd",i,".COHORT_DEFINITION_ID
  and co",i,".DATABASE_ID = cd",i,".DATABASE_ID"
        )

    }), collapse = ' union '),
 
") temp 
on ref.run_id = temp.run_id and 
ref.covariate_id = temp.covariate_id

group by 
ref.covariate_id, ref.covariate_name, an.analysis_name
"

)


  inputs <- c(
    as.character(combinations$Var2), 
    as.character(combinations$Var1), 
    resultDatabaseSettings$schema, 
    resultDatabaseSettings$cTablePrefix,
    paste0(analysisIds, collapse = ',')
  )
  names(inputs) <- c(
    paste0('database', 1:nrow(combinations)), 
    paste0('target', 1:nrow(combinations)),
    'schema',
    'c_table_prefix',
    'analysis_ids'
  )
  inputs <- as.list(inputs)
  inputs$sql <- sql
  
  result <- do.call(connectionHandler$queryDb, inputs)

return(result)
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
  
  
  sql <- 'select distinct analysis_id, analysis_name 
  from @schema.@c_table_prefixanalysis_ref order by analysis_name desc;'
  
  #shiny::incProgress(3/4, detail = paste("Extracting databaseIds"))
  
  analyses <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix
  )
  analysisIds <- analyses$analysisId
  names(analysisIds) <- analyses$analysisName
  
  
  #shiny::incProgress(4/4, detail = paste("Done"))
  
  #  })
  
  return(
    list(
      cohortIds = ids,
      databaseIds = databaseIds,
      analysisIds = analysisIds
    )
  )
  
}
