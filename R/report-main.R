# @file report-main.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
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


#' The location of the report module helper file
#'
#' @details
#' Returns the location of the report helper file
#' @family {Report}
#' @return
#' string location of the report helper file
#'
#' @export
reportHelperFile <- function(){
  fileLoc <- system.file('report-www', "report.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for the shiny app report module
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family Report
#' @return
#' The user interface to the home page module
#'
#' @export
reportViewer <- function(
    id = 'reportModule'
    ) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', 
    width = 12,
    title =  shiny::span( shiny::icon('book'), "Report Generator"),
    solidHeader = TRUE,
    
    
    shiny::tabsetPanel(
      type = 'hidden',#'pills',
      id = ns('mainTab'),
      
      shiny::tabPanel(
        title = 'Select Target', 
        shiny::uiOutput(ns("targetSelection"))
        ),
      
      shiny::tabPanel(
        title = 'Select Cohort Method Target', 
        shiny::uiOutput(ns("cmTargetSelection"))
      ),
      
      shiny::tabPanel(
        title = 'Select Comparator', 
        shiny::uiOutput(ns("comparatorSelection"))
      ),
      
      shiny::tabPanel(
        title = 'Select Outcome', 
        shiny::uiOutput(ns("outcomeSelection"))
      ),
      
      shiny::tabPanel(
        title = 'Generate', 
        shiny::uiOutput(ns("generateSelection"))
      )
      
      
    )
      
      
  
    
  )
}

#' The module server for the shiny app report module
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' @param server server for the connection to the results for quarto
#' @param username username for the connection to the results for quarto
#' @param password password for the connection to the results for quarto
#' @param dbms dbms for the connection to the results for quarto
#' @family Report
#' @return
#' The server for the shiny app home
#'
#' @export
reportServer <- function(
    id = 'reportModule',
    connectionHandler = NULL,
    resultDatabaseSettings = NULL,
    server = Sys.getenv("RESULTS_SERVER"), 
    username = Sys.getenv("RESULTS_USER"), 
    password = Sys.getenv("RESULTS_PASSWORD"), 
    dbms = Sys.getenv("RESULTS_DBMS")
    ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # get input options
      tnos <- getTandOs(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings, 
        includeCohortIncidence = F, # turning off for speed
        includeSccs = F # turning off for speed
      )
      
      ## update input selectors
      #============================
      
      # Targets
      targets <- lapply(tnos$groupedTs, function(x) x$cohortId)
      targets <- unlist(targets)
      
      
      # show download button
      showDownload <- shiny::reactiveVal(F)
      # outcome and comparator data.frames
      emptyRow <- data.frame(
        id = 0, 
        name = 'Add',
        friendlyName = 'Add'
        )
      outcomeDf <- shiny::reactiveVal(emptyRow)
      comparatorDf <- shiny::reactiveVal(emptyRow)
      
      
      
      output$targetSelection <- shiny::renderUI({
        
        shiny::div(
          
          shiny::fluidRow(
            
            shiny::column(
              width = 12,
              shiny::p('First pick a target cohort')
            )
          ),
          
          shiny::fluidRow(
            
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = session$ns('targetId'),
                label = 'Target: ',
                choices = targets,
                selected = 1,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
              )
              
            ),
            
            shiny::column(
              width = 6,
              shiny::textInput(
                inputId = session$ns('targetName'), 
                label = 'Friendly target name:', 
                value = 'Target'
              )
            )
          ),
          
          
          shiny::fluidRow(
            shiny::column(
              width = 10,
              shiny::p('')
            ),
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('targetNext'), 
                label = 'Next',
                shiny::icon("arrow-right"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                )
            )
            )
        )
      })
      
      
      cmTargets <- shiny::reactiveVal()
      shiny::observeEvent(
        input$targetNext,
        {
          
          # change tab to 'Select cmTarget'
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Select Cohort Method Target'
            )
          
          # get indications for target
          if(is.null(input$targetId)){
            return(NULL)
          }
          subsetTargets <- tnos$groupedTs[[which(unlist(lapply(tnos$groupedTs, function(x) ifelse(is.null(x$cohortId), F, x$cohortId == input$targetId))))]]$subsets
          ind <- !is.na(subsetTargets$subsetId)
          if(sum(ind)>0){
            cts <- subsetTargets$subsetId[ind]
            names(cts) <- subsetTargets$targetName[ind]
          } else{
            cts <- ''
            names(cts) <- 'No indication'
          }
          cmTargets(cts)
        }
      )
      
      
      output$cmTargetSelection <- shiny::renderUI({
        
        shiny::div(
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('Now pick the subset used by cohort method (indication and extra inclusions)')
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = session$ns('cmSubsetId'),
                label = 'Pick cohort method target: ',
                choices = cmTargets(),
                selected = 1,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
              )
            ),
            
            shiny::column(
              width = 6,
              shiny::textInput(
                inputId = session$ns('cmTargetName'),
                label = 'Friendly indication name: ',
                value = 'indication'
              )
            )
          ),
          

          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('cmTargetPrevious'), 
                label = 'Previous',
                shiny::icon("arrow-left"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            shiny::column(
              width = 8,
              shiny::p('')
            ),
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('cmTargetNext'), 
                label = 'Next',
                shiny::icon("arrow-right"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            )
          )
        )
      })
      
      comparators <- shiny::reactiveVal()
      shiny::observeEvent(
        input$cmTargetNext,
        {
          
          # change tab 
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Select Comparator'
          )
          
          if(!is.null(input$cmSubsetId) & !is.null(input$targetId)){
            if(input$cmSubsetId != ''){
              multipler <- ifelse(input$cmSubsetId == 0, 1, 1000)
              if(length(which(names(tnos$cs) == as.double(input$targetId)*multipler + as.double(input$cmSubsetId)))>0){
                temp <- tnos$cs[[which(names(tnos$cs) == as.double(input$targetId)*multipler + as.double(input$cmSubsetId))]]
                comps <- temp$comparatorId
                names(comps) <- temp$comparatorName
                comparators(comps)
              } else{
                comps <- ''
                names(comps) <- 'No Comparator'
                comparators(comps)
              }
            } else{
              shiny::showNotification('No indication available')
            }
          }

        }
      )
      
      
      shiny::observeEvent(
        input$cmTargetPrevious,
        {
          # change tab 
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Select Target'
          )
        }
      )
      
      
      output$comparatorSelection <- shiny::renderUI({
        
        shiny::div(
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('Now pick one or more comparators')
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = session$ns('comparatorId'),
                label = 'Pick comparator: ',
                choices = comparators(),
                selected = 1,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
              )
            ),
            
            shiny::column(
              width = 6,
              shiny::textInput(
                inputId = session$ns('comparatorName'),
                label = 'Friendly comparator name: ',
                value = 'comparator'
              )
            )
          ),
          
          
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('comparatorPrevious'), 
                label = 'Previous',
                shiny::icon("arrow-left"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            shiny::column(
              width = 8,
              shiny::p('')
            ),
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('comparatorNext'), 
                label = 'Next',
                shiny::icon("arrow-right"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            )
          )
        )
      })
      
      outcomes <- shiny::reactiveVal()
      shiny::observeEvent(
        input$comparatorNext,
        {
          
          # change tab to 'Select Outcome'
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Select Outcome'
          )
          
          if(is.null(input$targetId)){
            return(NULL)
          }
          
          multipler <- ifelse(input$cmSubsetId == 0, 1, 1000)
          cmTargetId <- as.double(input$targetId)*multipler + as.double(input$cmSubsetId)
            
          if(length(which(names(tnos$tos) %in% c(input$targetId, cmTargetId) ))>0){
            temp <- tnos$tos[[which(names(tnos$tos) %in% c(input$targetId, cmTargetId))[1] ]]
            os <- temp$outcomeId
            names(os) <- temp$outcomeName
            outcomes(os)
          } else{
            os <- ''
            names(os) <- 'None'
            outcomes(os)
            shiny::showNotification('No Outcomes')
          }
        }
      )
      
      shiny::observeEvent(
        input$comparatorPrevious,
        {
          # change tab 
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Select Cohort Method Target'
          )
        }
      )
      
      
      
      output$outcomeSelection <- shiny::renderUI({
        
        shiny::div(
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('Now pick one or more outcomes')
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = session$ns('outcomeId'),
                label = 'Pick outcome: ',
                choices = outcomes(),
                selected = 1,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
              )
            ),
            
            shiny::column(
              width = 6,
              shiny::textInput(
                inputId = session$ns('outcomeName'),
                label = 'Friendly outcome name: ',
                value = 'outcome'
              )
            )
          ),
          
          #shiny::fluidRow(
            #shiny::column(
            #  width = 2,
            #  shiny::actionButton(
            #    inputId = session$ns('addOutcome'),
            #    label = 'Add Outcome',
            #    icon = shiny::icon('plus')
            #  )
            #),
            #shiny::column(
            #  width = 10,
            #  reactable::reactable(outcomeDf())
            #)
          #),
          
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('outcomePrevious'), 
                label = 'Previous',
                shiny::icon("arrow-left"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            shiny::column(
              width = 8,
              shiny::p('')
            ),
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('outcomeNext'), 
                label = 'Next',
                shiny::icon("arrow-right"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            )
          )
          
        )
      })
      
      shiny::observeEvent(
        input$outcomeNext,
        {
          
          # change tab to 'Select Outcome'
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Generate'
          )
          
          output$inputTable <- shiny::renderTable(
            
           
            data.frame(
              Input = c('Target','Comparator','Outcome','Indication'),
              Id = c(input$targetId, input$comparatorId, input$outcomeId, input$cmSubsetId),
              Name = c(input$targetName, input$comparatorName, input$outcomeName, input$cmTargetName),
              FullName =  c(unlist(lapply(
                c(input$targetId, input$comparatorId, input$outcomeId),
                function(id){tnos$cg$cohortName[tnos$cg$cohortDefinitionId == id]}
              )), 'NA')
            )
          )

        }
      )
      
      shiny::observeEvent(
        input$outcomePrevious,
        {
          # change tab 
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Select Comparator'
          )
        }
      )
      
      
      # GENERATE 
      output$generateSelection <- shiny::renderUI({
        
        shiny::div(
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('First generate the protocol and then download')
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('Selected input review: '),
              shiny::tableOutput(outputId = session$ns('inputTable'))
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('Add study restriction details: ')
            ),
            shiny::column(
              width = 6,
              shiny::dateRangeInput(
                inputId = session$ns('dateRestriction'), 
                label = 'Study date restriction', 
                start = '1990-01-01'
              )
            ),
            shiny::column(
              width = 6,
              shiny::sliderInput(
                inputId = session$ns('ageRange'), 
                label = 'Study age range:', 
                min = 0, 
                max = 120, 
                step = 1, 
                value = c(18,120), 
                round = T
              )
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = session$ns('generatePrevious'), 
                label = 'Previous',
                shiny::icon("arrow-left"), 
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            shiny::column(
              width = 5,
              shiny::actionButton(
                inputId = session$ns("generate"), 
                label = "Generate", 
                shiny::icon('circle-plus')
              )
            ),
            shiny::column(
              width = 5,
              shiny::uiOutput(session$ns('downloadButton'))
            )
          )
          
          ) # end div
      })
      
      shiny::observeEvent(
        input$generatePrevious,
        {
          # change tab 
          shiny::updateTabsetPanel(
            session = session, 
            inputId = 'mainTab', 
            selected = 'Select Outcome'
          )
        }
      )
      
      output$downloadButton <- shiny::renderUI(
        expr = if(showDownload()) {
          shiny::downloadButton(
            outputId = session$ns("download"), 
            label = "Download"
          )
      } else {
        NULL
      })
      
      # Modals for outcome and comparator 
      #  model pops up with id and name inputs
      #  add the outcomeDf() or comparatorDf() 
      #  also add remove button 
      
      # remove outcome/comparator button
      
        
      # Downloadable presentation ----
      shiny::observeEvent(
        eventExpr = input$generate, 
        handlerExpr = {
          
          shiny::withProgress(
            message = 'Cleaning files', value = 0, {
              # remove file is exists
              if(file.exists(file.path(tempdir(), 'presentation.html'))){
                file.remove(file.path(tempdir(), 'presentation.html'))
                showDownload(F)
              };
              
              shiny::incProgress(0.2, detail = "Generating report")
              
              OhdsiReportGenerator::generatePresentationMultiple(
                server = server, 
                username = username, 
                password = password, 
                dbms = dbms,
                targetId = as.double(input$targetId), 
                resultsSchema = resultDatabaseSettings$schema, 
                subsetId = as.double(input$cmSubsetId), 
                outcomeIds = as.double(input$outcomeId), 
                comparatorIds = floor(as.double(input$comparatorId)/1000), # (remove subset), 
                covariateIds = c(  # TODO add this as input?
                  316139,320128210,443454210,
                  4282096210,441542210
                ),
                friendlyNames = list(
                  targetName = input$targetName,
                  comparatorNames = input$comparatorName,
                  indicationName = input$cmTargetName,
                  outcomeNames = input$outcomeName
                ),
                details = list(
                  studyPeriod = paste0(input$dateRestriction,  collapse = '-'),
                  restrictions = paste0("Age - ", paste0(input$ageRange,collapse = '-'))
                ), 
                title = 'Executive Summary Report', # TODO: add title for shiny app here?
                lead = 'Shiny App', 
                date = as.character(Sys.Date()), 
                outputLocation = tempdir(), 
                outputName = 'presentation.html'
              )
              showDownload(T)
              shiny::incProgress(1, detail = "Done")
        })
      })
      
      output$download <- shiny::downloadHandler(
        filename = function() {
          paste("presentation-", Sys.Date(), ".html", sep="")
        }, 
        content = function(file){
          if(file.exists(file.path(tempdir(), 'presentation.html'))){
            file.copy(
              from = file.path(tempdir(), 'presentation.html'), 
              to = file
                )
          }
        }
          )
      
      
      
    }
  )
}



getTandOs <- function(
    connectionHandler,
    resultDatabaseSettings,
    includeCharacterization = T,
    includeCohortIncidence = T,
    includeCohortMethod = T,
    includePrediction = T,
    includeSccs = T
){
  
  # get cohorts
  sql <- 'select distinct * from @schema.@cg_table_prefixcohort_definition order by cohort_name;'
  cg <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  if(includeCharacterization){
    characterization <- tryCatch(
      {nrow(connectionHandler$queryDb(
        'select * from @schema.@c_table_prefixcohort_details limit 1;', 
        schema = resultDatabaseSettings$schema,
        c_table_prefix = resultDatabaseSettings$cTablePrefix
      ))>=0},
      error = function(e){return(F)}
    )
  } else{
    characterization <- F
  }
    
  if(includeCohortIncidence){
    cohortIncidence <- tryCatch(
      {nrow(connectionHandler$queryDb(
        'select * from @schema.@ci_table_prefixincidence_summary limit 1;',
        schema = resultDatabaseSettings$schema,
        ci_table_prefix = resultDatabaseSettings$incidenceTablePrefix
      ))>=0},
      error = function(e){return(F)}
    )
  } else{
    cohortIncidence <- F
  }
  
  if(includeCohortMethod){
    cohortMethod <- tryCatch(
      {nrow(connectionHandler$queryDb(
        'select * from @schema.@cm_table_prefixtarget_comparator_outcome limit 1;',
        schema = resultDatabaseSettings$schema,
        cm_table_prefix = resultDatabaseSettings$cmTablePrefix
      ))>=0},
      error = function(e){return(F)}
    )
  } else{
    cohortMethod <- F
  }
  
  if(includePrediction){
    prediction <- tryCatch(
      {nrow(connectionHandler$queryDb(
        'select * from @schema.@plp_table_prefixmodel_designs limit 1;',
        schema = resultDatabaseSettings$schema,
        plp_table_prefix = resultDatabaseSettings$plpTablePrefix
      ))>=0},
      error = function(e){return(F)}
    )} else{
      prediction <- F
    }
  
  if(includeSccs){
    sccs <- tryCatch(
      {nrow(connectionHandler$queryDb(
        'select * from @schema.@sccs_table_prefixexposures_outcome_set limit 1;',
        schema = resultDatabaseSettings$schema,
        sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix
      ))>=0},
      error = function(e){return(F)}
    )} else{
      sccs <- F
    }
  
  # get T and O pairs
  sql <- "select distinct tid, oid from
  
  (
  
  {@characterization} ? {
    select distinct TARGET_COHORT_ID as tid, OUTCOME_COHORT_ID as oid 
    from @schema.@c_table_prefixcohort_details where 
    TARGET_COHORT_ID != 0 and OUTCOME_COHORT_ID != 0

  }
  
  {@cohort_incidence} ? {
  {@characterization}?{union}
    select distinct TARGET_COHORT_DEFINITION_ID as tid, OUTCOME_COHORT_DEFINITION_ID as oid 
    from @schema.@ci_table_prefixincidence_summary

  }
  
  {@cohort_method} ? {
   {@cohort_incidence | @characterization}?{union}
    select distinct TARGET_ID as tid, OUTCOME_ID as oid 
    from @schema.@cm_table_prefixtarget_comparator_outcome 
    where OUTCOME_OF_INTEREST = 1
  
  }
  
  {@prediction} ? {
   {@cohort_method | @cohort_incidence | @characterization}?{union}
    
    select distinct c1.cohort_definition_id as tid, c2.cohort_definition_id as oid 
    from @schema.@plp_table_prefixmodel_designs md 
    inner join @schema.@plp_table_prefixcohorts c1 
    on c1.cohort_id = md.target_id
    inner join @schema.@plp_table_prefixcohorts c2
    on c2.cohort_id = md.outcome_id
  }
  
  {@sccs} ? {
   {@cohort_method | @cohort_incidence | @characterization | @prediction}?{union}
    
  SELECT distinct 
  cov.era_id as tid,
  eos.outcome_id as oid
  
  FROM @schema.@sccs_table_prefixdiagnostics_summary ds
  
  inner join
  @schema.@sccs_table_prefixexposures_outcome_set eos
  on ds.exposures_outcome_set_id = eos.exposures_outcome_set_id
  
  INNER JOIN
  @schema.@sccs_table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id and
  cov.database_id = ds.database_id
  
  -- adding code to remove the negative controls
  INNER JOIN 
  @schema.@sccs_table_prefixexposure e
  ON e.exposures_outcome_set_id = ds.exposures_outcome_set_id
  AND e.era_id = cov.era_id
  where e.true_effect_size is NULL
   
  }
  
  ) temp_t_o
  
  ;"
  res <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    ci_table_prefix = resultDatabaseSettings$incidenceTablePrefix,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    characterization = characterization,
    cohort_incidence = cohortIncidence,
    cohort_method = cohortMethod,
    prediction = prediction,
    sccs = sccs
  )
  
  # add cohort names
  res <- merge(
    x = res, 
    y = cg[,c('cohortDefinitionId','cohortName')],
    by.x = 'tid',
    by.y = 'cohortDefinitionId'
  ) %>%
    dplyr::rename(
      targetName = "cohortName"
      )

  res <- merge(
    x = res,
    y = cg[,c('cohortDefinitionId','cohortName')], 
    by.x = 'oid', 
    by.y = 'cohortDefinitionId'
  ) %>%
    dplyr::rename(
      outcomeName = "cohortName"
      ) %>%
    dplyr::arrange(
      .data$targetName,
      .data$outcomeName
    )
  
  tos <- lapply(unique(res$tid), function(tid){
    data.frame(
      outcomeId = res$oid[res$tid == tid],
      outcomeName = res$outcomeName[res$tid == tid]
    )
  })
  names(tos) <- unique(res$tid)
  
  # get target heirarchy 
  groupedCohorts <- lapply(unique(res$tid), function(tid){
    list(
      cohortId = tid,
      cohortName = unique(res$targetName[res$tid == tid]),
      subsets = data.frame(
        targetId = tid,
        targetName = unique(res$targetName[res$tid == tid]),
        subsetId = 0
      )
    )
  })
  names(groupedCohorts) <- unique(res$targetName)
  
  # if using subsets then do this using the subsetDefinitionId
  if('subsetDefinitionId' %in% colnames(cg)){
    if(sum(is.na(cg$subsetParent)) > 0){
      cg$subsetParent[is.na(cg$subsetParent)] <- cg$cohortDefinitionId[is.na(cg$subsetParent)] 
    }
    cg$subsetDefinitionId[is.na(cg$subsetDefinitionId)] <- 0
    
    if(sum(cg$subsetParent == cg$cohortDefinitionId) > 0 ){
      # 
      parentChild <- unique(
        merge(
          x = cg[, c('cohortDefinitionId','subsetParent')], 
          y = res, 
          by.x = 'cohortDefinitionId',
          by.y = 'tid'
        )
      ) %>% dplyr::arrange( # adding order to make options orders
        .data$targetName
      )
      parents <- unique(parentChild$subsetParent)
      groupedCohorts <- lapply(1:length(parents), function(i){
        x <- parents[i];
          list(
            cohortId = x,
            cohortName = cg$cohortName[cg$cohortDefinitionId == x],
            subsets = data.frame(
              targetId = cg$cohortDefinitionId[cg$subsetParent == x],
              targetName = cg$cohortName[cg$subsetParent == x],
              subsetId = cg$subsetDefinitionId[cg$subsetParent == x]
            )
          );
      })
      names(groupedCohorts) <- unlist(lapply(groupedCohorts, function(x){x$cohortName}))
    }}
  
  # get comparators
  cs <- NULL
  if(cohortMethod){
    comps <- connectionHandler$queryDb(
      'select distinct target_id, comparator_id from 
       @schema.@cm_table_prefixtarget_comparator_outcome 
       where outcome_of_interest = 1;',
      schema = resultDatabaseSettings$schema,
      cm_table_prefix = resultDatabaseSettings$cmTablePrefix
    )
    
    comps <- merge(
      comps,cg[,c('cohortDefinitionId','cohortName')], 
      by.x = 'comparatorId', 
      by.y = 'cohortDefinitionId'
    ) %>%
      dplyr::rename(comparatorName = "cohortName")
    
    cs <- lapply(unique(comps$targetId), function(tid){
      data.frame(
        comparatorName = unique(comps$comparatorName[comps$targetId == tid]),
        comparatorId = unique(comps$comparatorId[comps$targetId == tid])
      )
    }
    )
    names(cs) <- unique(comps$targetId)
  }
  
  return(
    list(
      cg = cg,
      groupedTs = groupedCohorts,
      tos = tos,
      cs = cs,
      characterization = characterization,
      cohortIncidence = cohortIncidence,
      cohortMethod = cohortMethod,
      prediction = prediction,
      sccs = sccs
    )
  )
  
}
