# @file patient-level-prediction-settings.R
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


patientLevelPredictionSettingsServer <- function(
  id,
  modelDesign
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
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
              shiny::p(paste0(as.character(modelDesign()$developmentTargetJson), collapse = ' - ')),
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
            shiny::p(paste0(as.character(modelDesign()$developmentOutcomeJson), collapse = ' - ')),
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
            shiny::p(paste0(as.character(modelDesign()$plpDataSettingsJson), collapse = ' - ')),
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
              #DT::renderDataTable(
                #formatPopSettings(modelDesign()$populationSettingsJson)
              shiny::p(as.character(modelDesign()$populationSettingsJson))
              #)
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
              #DT::renderDataTable(
              #  formatCovSettings(modelDesign()$covariateSettings)
              #)
              shiny::p(paste0(as.character(modelDesign()$covariateSettingsJson), collapse = ' - '))
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
              #DT::renderDataTable(
              #  formatModSettings(modelDesign()$modelSettings  )
              #)
              shiny::p(as.character(modelDesign()$modelSettingsJson))
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
              shiny::p(paste0(as.character(modelDesign()$featureEngineeringSettingsJson ), collapse = ' - '))
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
              shiny::p(as.character(modelDesign()$tidyCovariatesSettingsJson))
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
              shiny::p(as.character(modelDesign()$splitSettingsJson))
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
              shiny::p(as.character(modelDesign()$sampleSettingsJson))
            ),
            easyClose = TRUE
          ))
        }
      )
      
    }
    
  )
}         


# formating helpers
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

