# @file prediction-settings.R
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
predictionSettingsViewer <- function(id) {
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
      shinydashboard::infoBoxOutput(ns("model"), width = 4),
      shinydashboard::infoBoxOutput(ns("hyperparameters"), width = 4),
      shinydashboard::infoBoxOutput(ns("attrition"), width = 4)
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
#' @param schema the database schema for the model results
#' @param plpTablePrefix a string that appends the tables in the result schema
#' @param cohortTablePrefix a string that appends the cohort_definition table
#' @param databaseTablePrefix a string that appends the database_meta_data table
#' 
#' @return
#' The server to the settings module
#'
#' @export
predictionSettingsServer <- function(
  id,
  modelDesignId, 
  developmentDatabaseId, 
  performanceId,
  connectionHandler,
  inputSingleView,
  schema, 
  plpTablePrefix, 
  cohortTablePrefix = plpTablePrefix,
  databaseTablePrefix = plpTablePrefix
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # objects
      
      modelDesign <- shiny::reactive({
        getModelDesign(
        inputSingleView = inputSingleView,
        modelDesignId = modelDesignId,
        schema = schema, 
        connectionHandler = connectionHandler,
        plpTablePrefix = plpTablePrefix, 
        cohortTablePrefix = cohortTablePrefix
      )})
      
      hyperParamSearch <- shiny::reactive({getHyperParamSearch(
        inputSingleView = inputSingleView,
        modelDesignId = modelDesignId,
        databaseId = developmentDatabaseId,
        schema = schema, 
        connectionHandler = connectionHandler,
        plpTablePrefix  = plpTablePrefix
      ) })
      
      attrition <- shiny::reactive({
        getAttrition(
        inputSingleView = inputSingleView,
        performanceId = performanceId,
        schema = schema, 
        connectionHandler = connectionHandler,
        plpTablePrefix = plpTablePrefix 
      ) 
      })
      
      # databases
      databases <- shiny::reactive({
        getPlpSettingDatabase(
        inputSingleView = inputSingleView,
        performanceId = performanceId,
        schema = schema, 
        connectionHandler = connectionHandler,
        plpTablePrefix = plpTablePrefix, 
        databaseTablePrefix = databaseTablePrefix 
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
              easyClose = TRUE,
              footer = NULL
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
            easyClose = TRUE,
            footer = NULL
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
            easyClose = TRUE,
            footer = NULL
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
            easyClose = TRUE,
            footer = NULL
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
            easyClose = TRUE,
            footer = NULL
          ))
        }
      )
      
      # Model settings
      output$model <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Model',
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
            easyClose = TRUE,
            footer = NULL
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
            easyClose = TRUE,
            footer = NULL
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
            easyClose = TRUE,
            footer = NULL
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
            easyClose = TRUE,
            footer = NULL
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
            easyClose = TRUE,
            footer = NULL
          ))
        }
      )
      
      # extras
      
      # hyper-param
      output$hyperparameters<- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Hyper-parameters',
          shiny::actionButton(session$ns("showHyperparameters"),"View"), 
          icon = shiny::icon('gear'),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showHyperparameters, {
          shiny::showModal(shiny::modalDialog(
            title = "Hyper-parameters",
            shiny::div(
              DT::renderDataTable(
                DT::datatable(
                  as.data.frame(
                    hyperParamSearch()
                  ),
                  options = list(scrollX = TRUE),
                  colnames = 'Fold AUROC'
                )
              )
            ),
            easyClose = TRUE,
            footer = NULL
          ))
        }
      )
      
      # attrition
      output$attrition <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Attrition',
          shiny::actionButton(session$ns("showAttrition"),"View"), 
          icon = shiny::icon('magnet'),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showAttrition, {
          shiny::showModal(shiny::modalDialog(
            title = "Attrition",
            shiny::div(
              DT::renderDataTable(
                attrition() %>% dplyr::select(-c("performanceId", "outcomeId"))
              )
            ),
            easyClose = TRUE,
            footer = NULL
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
  schema, 
  connectionHandler,
  plpTablePrefix,
  databaseTablePrefix = plpTablePrefix
){
  
  if(!is.null(performanceId()) & inputSingleView() == 'Design Settings'){
    
  sql <- "
  
    SELECT 
    tempD.dev_db, 
    tempV.val_db 
    
    FROM 
    
    (select * from @my_schema.@my_table_appendperformances
    WHERE performance_id = @performance_id) perf
    
    inner join 
    
    (select dd.database_id, dmd.cdm_source_name as dev_db
    from @my_schema.@my_table_appenddatabase_details as dd inner join
    @my_schema.@database_table_appenddatabase_meta_data as dmd on 
    dd.database_meta_data_id = dmd.database_id) tempD
    
    on tempD.database_id = perf.development_database_id
    
    inner join 
    
    (select dd.database_id, dmd.cdm_source_name as val_db
    from @my_schema.@my_table_appenddatabase_details as dd inner join
    @my_schema.@database_table_appenddatabase_meta_data dmd on 
    dd.database_meta_data_id = dmd.database_id) tempV
    
    on tempV.database_id = perf.validation_database_id
  
  
  ;"
  
  databaseNames <- connectionHandler$queryDb(
    sql = sql,
    my_schema = schema,
    performance_id = performanceId(),
    my_table_append = plpTablePrefix,
    database_table_append = databaseTablePrefix
  )
  
  return(databaseNames)
  
  }
  
}


# get the data
getModelDesign <- function(
    inputSingleView,
  modelDesignId,
  schema, 
  connectionHandler,
  plpTablePrefix, 
  cohortTablePrefix = plpTablePrefix
){
  if(!is.null(modelDesignId()) & inputSingleView() == 'Design Settings'){

    shiny::withProgress(message = 'Extracting model design', value = 0, {
      
    modelDesign <- list()
    
    shiny::incProgress(1/12, detail = paste("Extracting ids"))
    
    sql <- "SELECT * FROM 
    @my_schema.@my_table_appendmodel_designs 
    WHERE model_design_id = @model_design_id;"
    
    ids <- connectionHandler$queryDb(
      sql = sql,
      my_schema = schema,
      model_design_id = modelDesignId(),
      my_table_append = plpTablePrefix
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
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendmodel_settings WHERE model_setting_id = @model_setting_id;"

    tempModSettings <- connectionHandler$queryDb(
      sql = sql,
      my_schema = schema,
      model_setting_id = modSetId,
      my_table_append = plpTablePrefix
    )
    
    modelDesign$modelSettings <- ParallelLogger::convertJsonToSettings(
      tempModSettings$modelSettingsJson
      )
    
    shiny::incProgress(3/12, detail = paste("Extracting  covariate settings"))
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendcovariate_settings WHERE covariate_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = covSetId,
      my_table_append = plpTablePrefix
    )
    modelDesign$covariateSettings <- ParallelLogger::convertJsonToSettings(
      tempSettings$covariateSettingsJson
      )
    
    
    shiny::incProgress(4/12, detail = paste("Extracting population settings"))
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendpopulation_settings WHERE population_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = popSetId,
      my_table_append = plpTablePrefix
    )
    
    modelDesign$populationSettings <- ParallelLogger::convertJsonToSettings(
      tempSettings$populationSettingsJson
      )

    shiny::incProgress(5/12, detail = paste("Extracting feature engineering settingd"))
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendfeature_engineering_settings WHERE feature_engineering_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = feSetId,
      my_table_append = plpTablePrefix
    )
    modelDesign$featureEngineeringSettings <- tempSettings$featureEngineeringSettingsJson
    
    shiny::incProgress(6/12, detail = paste("Extracting tidy covariate settings"))
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendtidy_covariates_settings WHERE tidy_covariates_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = tidyCovariatesSettingId,
      my_table_append = plpTablePrefix
    )
    modelDesign$preprocessSettings <- tempSettings$tidyCovariatesSettingsJson
    
    
    shiny::incProgress(7/12, detail = paste("Extracting restrict plp settings"))
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendplp_data_settings WHERE plp_data_setting_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = plpDataSettingId,
      my_table_append = plpTablePrefix
    )
    modelDesign$RestrictPlpData <- tempSettings$plpDataSettingsJson
    
    
    shiny::incProgress(8/12, detail = paste("Extracting sample settings"))
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendsample_settings WHERE sample_setting_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = sampleSetId,
      my_table_append = plpTablePrefix
    )
    modelDesign$sampleSettings <- tempSettings$sampleSettingsJson
    
    
    shiny::incProgress(9/12, detail = paste("Extracting split settings"))
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendsplit_settings WHERE split_setting_id = @setting_id;"

    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = splitId,
      my_table_append = plpTablePrefix
    )
    modelDesign$splitSettings <- tempSettings$splitSettingsJson
    
    
    shiny::incProgress(10/12, detail = paste("Extracting target cohort"))
    
    sql <- "SELECT c.*, cd.json as cohort_json
    FROM @my_schema.@my_table_appendcohorts c inner join
    @my_schema.@cohort_table_appendcohort_definition cd
    on c.cohort_definition_id = cd.cohort_definition_id
    WHERE c.cohort_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = tId,
      my_table_append = plpTablePrefix,
      cohort_table_append = cohortTablePrefix
    )
    modelDesign$cohort <- tempSettings
    
    
    shiny::incProgress(11/12, detail = paste("Extracting outcome cohort"))
    
    sql <- "SELECT c.*, cd.json as cohort_json
    FROM @my_schema.@my_table_appendcohorts c inner join
    @my_schema.@cohort_table_appendcohort_definition cd
    on c.cohort_definition_id = cd.cohort_definition_id
    WHERE c.cohort_id = @setting_id;"
    
    tempSettings <- connectionHandler$queryDb(
      sql = sql, 
      my_schema = schema,
      setting_id = oId,
      my_table_append = plpTablePrefix,
      cohort_table_append = cohortTablePrefix
    )
    modelDesign$outcome <- tempSettings
    
    shiny::incProgress(12/12, detail = paste("Finished"))
    
    })
    
    return(modelDesign)
  }
  return(NULL)
}


getHyperParamSearch <- function(
    inputSingleView,
  modelDesignId,
  databaseId,
  schema, 
  connectionHandler,
  plpTablePrefix
){
  
  if(!is.null(modelDesignId()) & inputSingleView() == 'Design Settings'){

  sql <- "SELECT train_details FROM @my_schema.@my_table_appendmodels WHERE database_id = @database_id
       and model_design_id = @model_design_id;"

  models <- connectionHandler$queryDb(
    sql = sql, 
    my_schema = schema,
    database_id = databaseId(),
    model_design_id = modelDesignId(),
    my_table_append = plpTablePrefix
  )
  trainDetails <- ParallelLogger::convertJsonToSettings(models$trainDetails)
  
  return(trainDetails$hyperParamSearch)
  }
}


getAttrition <- function(
    inputSingleView,
  performanceId,
  schema, 
  connectionHandler,
  plpTablePrefix 
){

  if(!is.null(performanceId()) & inputSingleView() == 'Design Settings'){
    
  sql <- "SELECT * FROM @my_schema.@my_table_appendattrition WHERE performance_id = @performance_id;"

  attrition  <- connectionHandler$queryDb(
    sql = sql, 
    my_schema = schema,
    performance_id = performanceId(),
    my_table_append = plpTablePrefix
  )
  
  return(attrition)
  }
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

