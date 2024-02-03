# @file patient-level-prediction-settings.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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


#' The module viewer for exploring prediction settings 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the settings module
#'
#' @export
patientLevelPredictionSettingsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shinydashboard::box(
      width = 12,
      title = "Settings Dashboard",
      status = "info", solidHeader = TRUE,
      shinydashboard::infoBoxOutput(ns("cohort"), width = 4),
      shinydashboard::infoBoxOutput(ns("outcome"), width = 4),
      shinydashboard::infoBoxOutput(ns("restrictPlpData"), width = 4),
      shinydashboard::infoBoxOutput(ns("population"), width = 4),
      shinydashboard::infoBoxOutput(ns("covariates"), width = 4),
      shinydashboard::infoBoxOutput(ns("featureEngineering"), width = 4),
      shinydashboard::infoBoxOutput(ns("preprocess"), width = 4),
      shinydashboard::infoBoxOutput(ns("split"), width = 4),
      shinydashboard::infoBoxOutput(ns("sample"), width = 4),
      shinydashboard::infoBoxOutput(ns("model"), width = 4)
    )
    
  )
}

#' The module server for exploring prediction settings
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param modelDesignId unique id for the model design
#' @param developmentDatabaseId  unique id for the development database
#' @param performanceId unique id for the performance results
#' @param connectionHandler the connection to the prediction result database
#' @param inputSingleView the current tab 
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' 
#' @return
#' The server to the settings module
#'
#' @export
patientLevelPredictionSettingsServer <- function(
  id,
  modelDesignId, 
  developmentDatabaseId, 
  performanceId,
  connectionHandler,
  inputSingleView,
  resultDatabaseSettings
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # objects
      
      modelDesign <- shiny::reactive({
        getPredictionModelDesign(
        inputSingleView = inputSingleView,
        modelDesignId = modelDesignId,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )})

      # databases
      databases <- shiny::reactive({
        getPlpSettingDatabase(
        inputSingleView = inputSingleView,
        performanceId = performanceId,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      })
      
      
      # cohort settings
      output$cohort <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Cohort',
          shiny::actionButton(session$ns("showCohort"),"View"), 
          icon = shiny::icon("users"),
          color = "light-blue"
        )
      })
      
      shiny::observeEvent(
        input$showCohort, {
          
            shiny::showModal(shiny::modalDialog(
              title = "Cohort description",
              shiny::p(modelDesign()$cohort$cohortJson),
              easyClose = TRUE
            ))
            
          
        }
      )
      
      # outcome settings
      output$outcome <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Outcome',
          shiny::actionButton(session$ns("showOutcome"),"View"), 
          icon = shiny::icon("heart"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showOutcome, {
          shiny::showModal(shiny::modalDialog(
            title = "Cohort description",
            shiny::p(modelDesign()$outcome$cohortJson),
            easyClose = TRUE
          ))
        }
      )
      
      
      # restrictPlpData settings
      output$restrictPlpData <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'RestrictPlpData',
          shiny::actionButton(session$ns("showRestrictPlpData"),"View"), 
          icon = shiny::icon("filter"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showRestrictPlpData, {
          shiny::showModal(shiny::modalDialog(
            title = "Exclusions done during data extraction",
            shiny::p(modelDesign()$RestrictPlpData),
            easyClose = TRUE
          ))
        }
      )
      
      
      # Population settings
      output$population <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Population',
          shiny::actionButton(session$ns("showPopulation"),"View"), 
          icon = shiny::icon("users-slash"),
          color = "light-blue", 
          width = 3,
        )
      })
      shiny::observeEvent(
        input$showPopulation, {
          shiny::showModal(shiny::modalDialog(
            title = "Population Settings - exclusions after data extraction",
            shiny::div(
              shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulation.html", target="_blank"),
              DT::renderDataTable(
                formatPopSettings(modelDesign()$populationSettings)
              )
            ),
            easyClose = TRUE
          ))
        }
      )
      
      # Covariate settings
      output$covariates <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Covariates',
          shiny::actionButton(session$ns("showCovariates"),"View"), 
          icon = shiny::icon("street-view"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showCovariates, {
          shiny::showModal(shiny::modalDialog(
            title = "Covariate Settings",
            shiny::div(
              shiny::a("help", href="http://ohdsi.github.io/FeatureExtraction/reference/createCovariateSettings.html", target="_blank"),
              DT::renderDataTable(
                formatCovSettings(modelDesign()$covariateSettings)
              )
            ),
            easyClose = TRUE
          ))
        }
      )
      
      # Model settings
      output$model <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Model Settings',
          shiny::actionButton(session$ns("showModel"),"View"), 
          icon = shiny::icon("sliders-h"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showModel, {
          shiny::showModal(shiny::modalDialog(
            title = "Model Settings",
            shiny::div(
              shiny::h3('Model Settings: ',
                        shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/index.html", target="_blank")
              ),
              DT::renderDataTable(
                formatModSettings(modelDesign()$modelSettings  )
              )
            ),
            easyClose = TRUE
          ))
        }
      )
      
      # featureEngineering settings
      output$featureEngineering <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Feature Engineering',
          shiny::actionButton(session$ns("showFeatureEngineering"),"View"), 
          icon = shiny::icon("lightbulb"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showFeatureEngineering, {
          shiny::showModal(shiny::modalDialog(
            title = "Feature Engineering Settings",
            shiny::div(
              shiny::p(modelDesign()$featureEngineeringSettings)
            ),
            easyClose = TRUE
          ))
        }
      )
      
      # preprocess settings
      output$preprocess <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Preprocess',
          shiny::actionButton(session$ns("showPreprocess"),"View"), 
          icon = shiny::icon("chalkboard"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showPreprocess, {
          shiny::showModal(shiny::modalDialog(
            title = "Preprocess Settings",
            shiny::div(
              shiny::p(modelDesign()$preprocessSettings)
            ),
            easyClose = TRUE
          ))
        }
      )
      
      # split settings
      output$split <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Split',
          shiny::actionButton(session$ns("showSplit"),"View"), 
          icon = shiny::icon("object-ungroup"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showSplit, {
          shiny::showModal(shiny::modalDialog(
            title = "Split Settings",
            shiny::div(
              shiny::p(modelDesign()$splitSettings)
            ),
            easyClose = TRUE
          ))
        }
      )
      
      # sample settings
      output$sample <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Sample',
          shiny::actionButton(session$ns("showSample"),"View"), 
          icon = shiny::icon("equals"),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showSample, {
          shiny::showModal(shiny::modalDialog(
            title = "Sample Settings",
            shiny::div(
              shiny::p(modelDesign()$sampleSettings)
            ),
            easyClose = TRUE
          ))
        }
      )
      
    }
    
  )
}         



# helpers


# get the databases

getPlpSettingDatabase <- function(
  inputSingleView,
  performanceId,
  connectionHandler,
  resultDatabaseSettings
){
  
  if(!is.null(performanceId()) & inputSingleView() == 'Design Settings'){
    
  sql <- "
  
    SELECT 
    tempD.dev_db, 
    tempV.val_db 
    
    FROM 
    
    (select * from @schema.@plp_table_prefixperformances
    WHERE performance_id = @performance_id) perf
    
    inner join 
    
    (select dd.database_id, dmd.cdm_source_name as dev_db
    from @schema.@plp_table_prefixdatabase_details as dd inner join
    @schema.@database_table_prefixdatabase_meta_data as dmd on 
    dd.database_meta_data_id = dmd.database_id) tempD
    
    on tempD.database_id = perf.development_database_id
    
    inner join 
    
    (select dd.database_id, dmd.cdm_source_name as val_db
    from @schema.@plp_table_prefixdatabase_details as dd inner join
    @schema.@database_table_prefixdatabase_meta_data dmd on 
    dd.database_meta_data_id = dmd.database_id) tempV
    
    on tempV.database_id = perf.validation_database_id
  
  
  ;"
  
  databaseNames <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    performance_id = performanceId(),
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix
  )
  
  return(databaseNames)
  
  }
  
}


# get the data
getPredictionModelDesign <- function(
    inputSingleView,
  modelDesignId,
  connectionHandler,
  resultDatabaseSettings
){
  if(!is.null(modelDesignId()) & inputSingleView() == 'Design Settings'){

    shiny::withProgress(message = 'Extracting model design', value = 0, {
      
    modelDesign <- list()
    
    shiny::incProgress(1/12, detail = paste("Extracting ids"))
    
    sql <- "SELECT * FROM 
    @schema.@plp_table_prefixmodel_designs 
    WHERE model_design_id = @model_design_id;"
    
    ids <- connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      model_design_id = modelDesignId(),
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    
 
    popSetId <- ids$populationSettingId
    modSetId <- ids$modelSettingId
    covSetId <- ids$covariateSettingId
    feSetId <- ids$featureEngineeringSettingId
    sampleSetId <- ids$sampleSettingId
    splitId <- ids$splitSettingId
    tId <- ids$targetId
    oId <- ids$outcomeId
    plpDataSettingId <- ids$plpDataSettingId
    tidyCovariatesSettingId <- ids$tidyCovariatesSettingId
    
    shiny::incProgress(2/12, detail = paste("Extracting model settings"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixmodel_settings WHERE model_setting_id = @model_setting_id;"

    tempModSettings <- connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      model_setting_id = modSetId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    
    modelDesign$modelSettings <- ParallelLogger::convertJsonToSettings(
      tempModSettings$modelSettingsJson
      )
    
    shiny::incProgress(3/12, detail = paste("Extracting  covariate settings"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixcovariate_settings WHERE covariate_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = covSetId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    modelDesign$covariateSettings <- ParallelLogger::convertJsonToSettings(
      tempSettings$covariateSettingsJson
      )
    
    
    shiny::incProgress(4/12, detail = paste("Extracting population settings"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixpopulation_settings WHERE population_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = popSetId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    
    modelDesign$populationSettings <- ParallelLogger::convertJsonToSettings(
      tempSettings$populationSettingsJson
      )

    shiny::incProgress(5/12, detail = paste("Extracting feature engineering settingd"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixfeature_engineering_settings WHERE feature_engineering_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = feSetId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    modelDesign$featureEngineeringSettings <- tempSettings$featureEngineeringSettingsJson
    
    shiny::incProgress(6/12, detail = paste("Extracting tidy covariate settings"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixtidy_covariates_settings WHERE tidy_covariates_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = tidyCovariatesSettingId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    modelDesign$preprocessSettings <- tempSettings$tidyCovariatesSettingsJson
    
    
    shiny::incProgress(7/12, detail = paste("Extracting restrict plp settings"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixplp_data_settings WHERE plp_data_setting_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = plpDataSettingId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    modelDesign$RestrictPlpData <- tempSettings$plpDataSettingsJson
    
    
    shiny::incProgress(8/12, detail = paste("Extracting sample settings"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixsample_settings WHERE sample_setting_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = sampleSetId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    modelDesign$sampleSettings <- tempSettings$sampleSettingsJson
    
    
    shiny::incProgress(9/12, detail = paste("Extracting split settings"))
    
    sql <- "SELECT * FROM @schema.@plp_table_prefixsplit_settings WHERE split_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = splitId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    modelDesign$splitSettings <- tempSettings$splitSettingsJson
    
    
    shiny::incProgress(10/12, detail = paste("Extracting target cohort"))
    
    sql <- "SELECT c.*, cd.json as cohort_json
    FROM @schema.@plp_table_prefixcohorts c inner join
    @schema.@cg_table_prefixcohort_definition cd
    on c.cohort_definition_id = cd.cohort_definition_id
    WHERE c.cohort_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = tId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix
    )
    modelDesign$cohort <- tempSettings
    
    
    shiny::incProgress(11/12, detail = paste("Extracting outcome cohort"))
    
    sql <- "SELECT c.*, cd.json as cohort_json
    FROM @schema.@plp_table_prefixcohorts c inner join
    @schema.@cg_table_prefixcohort_definition cd
    on c.cohort_definition_id = cd.cohort_definition_id
    WHERE c.cohort_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      setting_id = oId,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix
    )
    modelDesign$outcome <- tempSettings
    
    shiny::incProgress(12/12, detail = paste("Finished"))
    
    })
    
    return(modelDesign)
  }
  return(NULL)
}






# formating
formatModSettings <- function(modelSettings){
  
  modelset <- data.frame(
    paramJson = as.character(
      ParallelLogger::convertSettingsToJson(
        modelSettings$param
      )
    )
  )
  
  return(modelset)
}

# format covariateSettings
formatCovSettings <- function(covariateSettings){
  
  if(inherits(covariateSettings, 'covariateSettings')){
    covariateSettings <- list(covariateSettings)
  }
  
  #code for when multiple covariateSettings
  covariates <- c() 
  for(i in 1:length(covariateSettings)){
    covariatesTemp <- data.frame(
      fun = attr(covariateSettings[[i]],'fun'),
      setting = i,
      covariateName = names(covariateSettings[[i]]), 
      SettingValue = unlist(
        lapply(
          covariateSettings[[i]], 
          function(x) paste0(x, collapse='-')
        )
      )
    )
    covariates  <- rbind(covariates,covariatesTemp)
  }
  row.names(covariates) <- NULL
  return(covariates)
}

# format populationSettings
formatPopSettings <- function(populationSettings){
  population <- populationSettings
  population$attrition <- NULL # remove the attrition as result and not setting
  population <- data.frame(Setting = names(population), 
                           Value = unlist(lapply(population, 
                                                 function(x) paste0(x, 
                                                                    collapse='-')))
  ) 
  row.names(population) <- NULL
  return(population)
}

