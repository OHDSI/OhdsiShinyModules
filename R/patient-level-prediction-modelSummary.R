# @file patient-level-prediction-modelSummary.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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


#' The module viewer for exploring prediction summary results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the summary module
#'
#' @export
patientLevelPredictionModelSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("patient-level-prediction-www", "main-modelSummaryHelp.html", package = utils::packageName())
    ),
    
    # module that does input selection for a single row DF
    inputSelectionDfViewer(
      id = ns("df-output-selection"),
      title = 'Model Design Selected'
      ),
  
    #shinydashboard::box(
    #  width = "100%",
      resultTableViewer(ns('performanceSummaryTable'))
    #)
  )
}

#' The module server for exploring prediction summary results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @param modelDesignId a reactable id specifying the prediction model design identifier
#' @family PatientLevelPrediction
#' @return
#' The server to the summary module
#'
#' @export
patientLevelPredictionModelSummaryServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings,
  modelDesignId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      

      
      selectedModelDesign <- shiny::reactive(
        getModelDesignInfo(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          modelDesignId = modelDesignId
          )
      )
      
      inputSelectionDfServer(
        id = "df-output-selection", 
        dataFrameRow = selectedModelDesign
          )
      
      resultTable <- shiny::reactive(
        getModelDesignPerformanceSummary(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          modelDesignId = modelDesignId
        )
      )
      
      colDefsInput = list(
        Dev = reactable::colDef( 
          name = "Dev Db",
          filterable = TRUE,
          header = withTooltip(
            "Dev Db", 
            "The database used to develop the model"
          )),
        Val = reactable::colDef( 
          name = "Val Db", 
          filterable = TRUE,
          header = withTooltip(
            "Val Db", 
            "The database used to evaluate the model"
          )),
        T = reactable::colDef( 
          name = "Target Pop",
          filterable = TRUE,
          header = withTooltip(
            "Target Pop", 
            "The patients who the risk model is applied to"
          ), 
          minWidth = 300
          ),
        O = reactable::colDef( 
          name = "Outcome", 
          filterable = TRUE,
          header = withTooltip(
            "Outcome", 
            "The outcome being predicted"
          ), 
          minWidth = 300
          ),
        TAR = reactable::colDef( 
          name = "TAR", 
          filterable = TRUE,
          header = withTooltip(
            "TAR", 
            "The time-at-risk when the outcome is being predicted relative to the target pop index"
          ),
          sortable = TRUE
        ),
        type = reactable::colDef( 
          name = "Type", 
          filterable = TRUE,
          header = withTooltip(
            "Type", 
            "Development contains the model and internal validation; Validation contains the external validation"
          ),
          sortable = TRUE
        ),
        modelDevelopment = reactable::colDef( 
          show = FALSE
        ),
        performanceId = reactable::colDef( 
          show = FALSE
        ),
        modelDesignId = reactable::colDef( 
          show = FALSE
        ),
        developmentDatabaseId = reactable::colDef( 
          show = FALSE
        )
      )
      
      modelTableOutputs <- resultTableServer(
        id = "performanceSummaryTable",
        df = resultTable,
        colDefsInput = colDefsInput,
        addActions = c('results','attrition'),
        elementId = session$ns('performanceSummaryTable')
      )
      
      performanceId <- shiny::reactiveVal(value = NULL)
      developmentDatabaseId <- shiny::reactiveVal(value = NULL)
      modelDevelopment <- shiny::reactiveVal(value = NULL)
      shiny::observeEvent(modelTableOutputs$actionCount(), {
          if(modelTableOutputs$actionType() == 'results'){
            performanceId(NULL)
            performanceId(resultTable()$performanceId[modelTableOutputs$actionIndex()$index])
            developmentDatabaseId(resultTable()$developmentDatabaseId[modelTableOutputs$actionIndex()$index])
            modelDevelopment(resultTable()$modelDevelopment[modelTableOutputs$actionIndex()$index])
          }
      })
      
      shiny::observeEvent(modelTableOutputs$actionCount(), {
        if(modelTableOutputs$actionType() == 'attrition'){
          
          attrition <- shiny::reactive({
            getAttrition(
              performanceId = resultTable()$performanceId[modelTableOutputs$actionIndex()$index],
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings
            ) 
          })
          
              shiny::showModal(
                shiny::modalDialog(
                title = "Attrition",
                shiny::div(
                  DT::renderDataTable(
                    attrition() %>% dplyr::select(-c("performanceId", "outcomeId"))
                  )
                ),
                easyClose = TRUE
              )
              )

          }
      })
      
      return(
        list(
          developmentDatabaseId = developmentDatabaseId,
          performanceId = performanceId,
          modelDevelopment = modelDevelopment
        )
      )
      
    }
  )
}

getAttrition <- function(
    performanceId,
    connectionHandler,
    resultDatabaseSettings
){
  
  if(!is.null(performanceId)){
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixattrition WHERE performance_id = @performance_id;"
    
    attrition  <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      performance_id = performanceId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    
    return(attrition)
  }
}

getModelDesignPerformanceSummary <- function(
    connectionHandler, 
    resultDatabaseSettings,
  modelDesignId
){
  
  if(is.null(modelDesignId())){
    modelDesignId(1)
  }
  
  ParallelLogger::logInfo("gettingDb summary")
  
  shiny::withProgress(message = 'Plotting distributions', value = 0, {
    
    shiny::incProgress(1/3, detail = paste("Extracting data"))
    
  sql <- "SELECT distinct 
     results.performance_id, 
     results.model_design_id, 
     results.development_database_id,
     results.validation_database_id,
     d.database_acronym AS Dev, 
     v.database_acronym AS Val,
     targets.cohort_name AS T, 
     outcomes.cohort_name AS O,
     results.execution_date_time as time_stamp,
       tars.tar_start_day, 
       tars.tar_start_anchor, 
       tars.tar_end_day, 
       tars.tar_end_anchor,
       ROUND(aucResult.auc, 3) as auroc,
       ROUND(auprcResult.auprc,4) as auprc,
       nResult.population_size, 
       oResult.outcome_count,
       ROUND(nTest.test_size*100.0/nResult.population_size, 1) as eval_percent,
       ROUND(oResult.outcome_count*100.0/nResult.population_size,4) as outcome_percent,
       results.model_development
       
       FROM (select * from @schema.@plp_table_prefixperformances where model_design_id = @model_design_id) AS results 
  
    inner join @schema.@plp_table_prefixmodel_designs as model_designs
    on model_designs.model_design_id = results.model_design_id

        LEFT JOIN 
        (SELECT c.cohort_id, cd.cohort_name FROM @schema.@plp_table_prefixcohorts c
        inner join @schema.@cg_table_prefixcohort_definition cd
        on c.cohort_definition_id = cd.cohort_definition_id
        ) AS targets ON results.target_id = targets.cohort_id
        LEFT JOIN 
        (SELECT c.cohort_id, cd.cohort_name FROM @schema.@plp_table_prefixcohorts c
        inner join @schema.@cg_table_prefixcohort_definition cd
        on c.cohort_definition_id = cd.cohort_definition_id
        ) AS outcomes ON results.outcome_id = outcomes.cohort_id
        LEFT JOIN (select dd.database_id, md.cdm_source_abbreviation database_acronym 
                   from @schema.@database_table_prefix@database_table md inner join 
                   @schema.@plp_table_prefixdatabase_details dd 
                   on md.database_id = dd.database_meta_data_id) AS d ON results.development_database_id = d.database_id 
                   LEFT JOIN (select dd.database_id, md.cdm_source_abbreviation database_acronym 
                   from @schema.@database_table_prefix@database_table md inner join 
                   @schema.@plp_table_prefixdatabase_details dd 
                   on md.database_id = dd.database_meta_data_id) AS v ON results.validation_database_id = v.database_id 
        LEFT JOIN @schema.@plp_table_prefixtars AS tars ON results.tar_id = tars.tar_id
        LEFT JOIN (SELECT performance_id, value AS auc FROM @schema.@plp_table_prefixevaluation_statistics where metric = 'AUROC' and evaluation in ('Test','Validation') ) AS aucResult ON results.performance_id = aucResult.performance_id
        LEFT JOIN (SELECT performance_id, value AS auprc FROM @schema.@plp_table_prefixevaluation_statistics where metric = 'AUPRC' and evaluation in ('Test','Validation') ) AS auprcResult ON results.performance_id = auprcResult.performance_id
        LEFT JOIN (SELECT performance_id, sum(value) AS population_size FROM @schema.@plp_table_prefixevaluation_statistics where metric = 'populationSize' and evaluation in ('Train','Test','Validation') group by performance_id) AS nResult ON results.performance_id = nResult.performance_id
        LEFT JOIN (SELECT performance_id, sum(value) AS outcome_count FROM @schema.@plp_table_prefixevaluation_statistics where metric = 'outcomeCount' and evaluation in ('Train','Test','Validation') group by performance_id) AS oResult ON results.performance_id = oResult.performance_id
        LEFT JOIN (SELECT performance_id, value AS test_size FROM @schema.@plp_table_prefixevaluation_statistics where metric = 'populationSize' and evaluation in ('Test', 'Validation') ) AS nTest ON results.performance_id = nTest.performance_id;"
  

  summaryTable <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    model_design_id = modelDesignId(),
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  shiny::incProgress(2/3, detail = paste("Data extracted"))
  
  
  summaryTable$t <- trimws(summaryTable$t)
  summaryTable$o <- trimws(summaryTable$o)
  
  summaryTable <- summaryTable %>% 
    dplyr::rename(`T Size` = "populationSize") %>% 
    dplyr::rename(`O Count` = "outcomeCount") %>%
    dplyr::rename(`Val (%)` = "evalPercent") %>%
    dplyr::rename(`O Incidence (%)` = "outcomePercent")
  
  summaryTable <- editTar(summaryTable)
  
  colnames(summaryTable) <- editColnames(cnames = colnames(summaryTable), 
                                         edits = c('AUROC','AUPRC', 'T', 'O', 'Dev','Val', 'TAR', 'Model'))
  
  summaryTable$T <- as.factor(summaryTable$T)
  summaryTable$O <- as.factor(summaryTable$O)
  
  summaryTable$type <- ifelse(summaryTable$modelDevelopment == 1, 'Development', 'Validation')
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  ParallelLogger::logInfo("Got db summary")
  
  })
  
  # adding actions column to left
  ##summaryTable <- cbind(
  ##  actions = rep("", nrow(summaryTable)),
  ##  summaryTable
  ##)
  
  return(summaryTable[,c('Dev', 'Val', 'T','O', 'modelDesignId',
                         'TAR', 'AUROC', 'AUPRC', 
                         'T Size', 'O Count','Val (%)', 'O Incidence (%)', 'timeStamp', 'performanceId', 'developmentDatabaseId', 'modelDevelopment', 'type')])
  
}

# repeated function?
editTar <- function(summaryTable){
  
  summaryTable <- summaryTable %>% dplyr::mutate(TAR = paste0('(',trimws(.data$tarStartAnchor),' + ',.data$tarStartDay, ') - (',trimws(.data$tarEndAnchor),' + ',.data$tarEndDay, ')' )) %>%
    dplyr::select(-c("tarStartAnchor", "tarStartDay", "tarEndAnchor", "tarEndDay"))
  
  return(summaryTable)
}

editColnames <- function(cnames, edits){
  lwcnames <- tolower(cnames)
  
  for(edit in edits){
    if(tolower(edit)%in%lwcnames){
      cnames[tolower(edit)==lwcnames] <- edit
    }
  }
  return(cnames)
  
}



getModelDesignInfo <- function(
  connectionHandler, 
  resultDatabaseSettings,
  modelDesignId
){
  
  if(is.null(modelDesignId())){
    return(NULL)
  }
  
  modelType <- connectionHandler$queryDb(
    'select distinct ms.model_type from 
    @schema.@plp_table_prefixmodel_settings ms
    inner join 
    @schema.@plp_table_prefixmodel_designs md
    on ms.model_setting_id = md.model_setting_id
    where md.model_design_id = @model_design_id;',
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    model_design_id = modelDesignId()
  )
  
  target <- connectionHandler$queryDb(
    'select distinct targets.cohort_name as target from 
    @schema.@plp_table_prefixmodel_designs md 
    inner join 
    @schema.@plp_table_prefixcohorts c
    on md.target_id = c.cohort_id
    inner join
    @schema.@cg_table_prefixcohort_definition AS targets
    on c.cohort_definition_id = targets.cohort_definition_id
    where md.model_design_id = @model_design_id;',
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    model_design_id = modelDesignId()
  )
  
  outcome <- connectionHandler$queryDb(
    'select distinct targets.cohort_name as outcome from 
    @schema.@plp_table_prefixmodel_designs md 
    inner join 
    @schema.@plp_table_prefixcohorts c
    on md.outcome_id = c.cohort_id
    inner join
    @schema.@cg_table_prefixcohort_definition AS targets
    on c.cohort_definition_id = targets.cohort_definition_id
    where md.model_design_id = @model_design_id;',
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    model_design_id = modelDesignId()
  )
  
  tar <- connectionHandler$queryDb(
    'select distinct 
    tars.tar_start_day, 
    tars.tar_start_anchor,
    tars.tar_end_day,
    tars.tar_end_anchor
    from @schema.@plp_table_prefixmodel_designs md 
    inner join 
    @schema.@plp_table_prefixtars AS tars
    on md.tar_id = tars.tar_id
    where md.model_design_id = @model_design_id;',
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    model_design_id = modelDesignId()
  )
   # replace with editTar?
  tar <- paste0(
    '( ', tar$tarStartAnchor, ' + ', tar$tarStartDay, ' ) - ( ',
    tar$tarEndAnchor, ' + ', tar$tarEndDay, ' )'
    )
    
  result <- data.frame(
    target = target,
    outcome = outcome,
    tar = tar,
    modelDesignId = modelDesignId(),
    modelType = modelType
  )
  return(result)

}






