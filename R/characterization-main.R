# @file characterization-main.R
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


#' The location of the characterization module helper file
#'
#' @details
#' Returns the location of the characterization helper file
#' 
#' @return
#' string location of the characterization helper file
#'
#' @export
characterizationHelperFile <- function(){
  fileLoc <- system.file('characterization-www', "characterization.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring characterization studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the characterization viewer module
#'
#' @export
characterizationViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', width = '100%',
    title =  shiny::span( shiny::icon("table"), "Characterization Viewer"),
    solidHeader = TRUE,
    
    # pick a targetId of interest 
    shinydashboard::box(
      title = 'Target Of Interest', 
      width = '100%',
      status = "primary",
      collapsible = T,
      shiny::uiOutput(ns("targetSelection"))
    ),
    
    shiny::conditionalPanel(
      condition = 'input.targetSelect', 
      ns = ns,
      inputSelectionDfViewer(id = ns('targetSelected'), title = 'Selected Target'),
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('mainPanel')
      )
    )
  )
  
}

#' The module server for exploring characterization studies
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server for the characterization module
#'
#' @export
characterizationServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      # this function checks tables exist for the tabs
      # and returns the tabs that should be displayed
      # as the tables exist
      charTypes <- getCharacterizationTypes(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      #================================================
      # GETTING OPTIONS FOR INPTUS
      #================================================
      #TODO add time-to-event and dechal-rechal options
      options <- characterizationGetOptions(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings, 
        includeAggregate = "Risk Factor" %in% charTypes$subPanel,
        includeIncidence = "Incidence Results" %in% charTypes$subPanel
      )
      
      #================================================
      # PARENT TARGET SELECTION UI
      #================================================
      parents <- characterizationGetParents(options)
      parentIndex <- shiny::reactiveVal(1)
      subTargets <- shiny::reactiveVal()
      
      # add an input for all char that lets you select cohort of interest
      output$targetSelection <- shiny::renderUI({
        shiny::div(
          shinyWidgets::pickerInput(
            inputId = session$ns('targetId'),
            label = 'Target Group: ',
            choices = parents,
            selected = parents[1],
            multiple = FALSE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              dropupAuto = F,
              size = 10,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 500
            )
          ),
          shiny::selectInput(
            inputId = session$ns('subTargetId'),
            label = 'Target: ',
            choices = characterizationGetChildren(options,1),
            selected = 1,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          shiny::actionButton(
            inputId = session$ns('targetSelect'), 
            label = 'Select',
            icon = shiny::icon('redo') 
          )
        )
      })
      
      #================================================
      # UPDATE TARGET BASED ON TARGET GROUP
      #================================================
      shiny::observeEvent(input$targetId,{
        parentIndex(which(parents == input$targetId))
        subTargets(characterizationGetChildren(options,which(parents == input$targetId)))
        shiny::updateSelectInput(
          inputId = 'subTargetId',
          label = 'Target: ',
          choices = subTargets(), 
          selected = subTargets()[1]
          )
      })
      
      #================================================
      # PARENT TARGET SELECTION ACTION
      #================================================
      # reactives updated when parent target is selected
      outcomes <- shiny::reactiveVal()
      targetSelected <- shiny::reactiveVal()
      subTargetId <- shiny::reactiveVal()
      # output the selected target
      shiny::observeEvent(input$targetSelect, {
        
        # First create input dataframe and add to the inputServer to display
        targetSelected(
          data.frame( 
            `Target group` = names(parents)[parents == input$targetId],
            `Target` = names(subTargets())[subTargets() == input$subTargetId]
          )
        )
        inputSelectionDfServer(
          id = 'targetSelected', 
          dataFrameRow = targetSelected,
          ncol = 1
        )
        
        subTargetId(input$subTargetId)
        
        # update the outcomes for the selected parent target id
        outcomes(characterizationGetOutcomes(options, parentIndex()))

        # create the outcome selector for the case exposure tabs
        output$outcomeSelection <- shiny::renderUI({
          shinydashboard::box(
            collapsible = TRUE,
            title = "Options",
            width = "100%",
            
            shinyWidgets::pickerInput(
              inputId = session$ns('outcomeId'),
              label = 'Outcome: ',
              choices = outcomes(),
              selected = 1,
              multiple = FALSE,
              options = shinyWidgets::pickerOptions(
                actionsBox = F,
                dropupAuto = F,
                size = 10,
                liveSearch = TRUE,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search"
              )
            ),
            shiny::actionButton(
              inputId = session$ns('outcomeSelect'), 
              label = 'Select',
              icon = shiny::icon('redo') 
            )
          )
        })
        
      })
      
      #================================================
      # OUTCOME SELECTION ACTION
      #================================================
      # used by the case exposure tabs
      # show the selected outcome
      outcomeSelected <- shiny::reactiveVal()
      outcomeId <- shiny::reactiveVal()
      #subTargetId <- shiny::reactiveVal()
      
      shiny::observeEvent(input$outcomeSelect, {
        outcomeSelected(
          data.frame( 
            #Target = names(subTargets())[subTargets() == input$subTargetId],
            Outcome = names(outcomes())[outcomes() == input$outcomeId]
          )
        )
        
        # store the outcome and subTargetIds for the case exposure tabs
        outcomeId(input$outcomeId)
        #subTargetId(input$subTargetId)
        
        inputSelectionDfServer(
          id = 'outcomeSelected', 
          dataFrameRow = outcomeSelected,
          ncol = 1
        )
      })
 
      
      #================================================
      # CREATE TABS BASED ON RESULTS TABLES
      #================================================

      # MAIN PANELS
      #first populate the mainPanel
      typesMainPanel <- list(
        list(
          title = 'Cohort Summary', 
          shiny::tabsetPanel(
            type = 'pills',
            id = session$ns('cohortSummaryPanel')
          )
        ),
        list(
          title = 'Exposed Cases Summary', 
          shiny::uiOutput(session$ns("outcomeSelection")),
          shiny::conditionalPanel(
            condition = 'input.outcomeSelect', 
            ns = session$ns,
            inputSelectionDfViewer(id = session$ns('outcomeSelected'), title = 'Selected'),
            shiny::tabsetPanel(
              type = 'pills',
              id = session$ns('exposedCasesPanel')
            )
          )
        ),
        list(
          title = 'Cohort Incidence', 
          shiny::tabsetPanel(
            type = 'pills',
            id = session$ns('cohortIncidencePanel')
          )
        )
      )
      
      selectVal <- T
      for( type in typesMainPanel){
        if(type$title %in% charTypes$mainPanel){
          shiny::insertTab(
            inputId = 'mainPanel',
            tab = do.call(
              what = shiny::tabPanel, 
              args = type
            ),
            select = selectVal
          )
          selectVal = F
        }
      }
      
      # SUB PANELS
      # now populate the subpanel
      # add the tabs based on results
      types <- rbind(
        c("Database Comparison","characterizationDatabaseComparisonViewer", "databaseComparisonTab", "cohortSummaryPanel"),
        c("Cohort Comparison", "characterizationCohortComparisonViewer", "cohortComparisonTab", "cohortSummaryPanel"),
        
        c("Risk Factor", "characterizationRiskFactorViewer", "riskFactorTab", "exposedCasesPanel"),
        c("Case Series", "characterizationCaseSeriesViewer", "caseSeriesTab", "exposedCasesPanel"),
        c("Time-to-event", "characterizationTimeToEventViewer", "timeToEventTab", "exposedCasesPanel"),
        c("Dechallenge Rechallenge", 'characterizationDechallengeRechallengeViewer', 'dechallengeRechallengeTab', "exposedCasesPanel"),
        
        c("Incidence Results", "characterizationIncidenceViewer", "cohortIncidenceTab", "cohortIncidencePanel")
      )
      colnames(types) <- c('c1', 'c2', 'c3', 'c4')
      types <- as.data.frame(types)
      
      for(subPanel in c("cohortSummaryPanel", "exposedCasesPanel", "cohortIncidencePanel")){
        typesOfInterest <- types %>% dplyr::filter(.data$c4 == subPanel)
        if(nrow(typesOfInterest)>0){
          selectVal <- T
          for( i in 1:nrow(typesOfInterest)){
            if(typesOfInterest[i,1] %in% charTypes$subPanel){
              shiny::insertTab(
                inputId = typesOfInterest[i,4],
                tab = shiny::tabPanel(
                  typesOfInterest[i,1],  
                  do.call(what = typesOfInterest[i,2], args = list(id = session$ns(typesOfInterest[i,3])))
                ), 
                select = selectVal
              )
              selectVal = F
            }
          }
        }
      }
      
      
      # =============================
      #  TRACK CURRENT TAB 
      # =============================
     # set the current tab
      mainPanel <- shiny::reactiveVal('None')
      shiny::observeEvent(input$mainPanel,{
        mainPanel(input$mainPanel)
      })
      cohortSummaryPanel <- shiny::reactiveVal('None')
      shiny::observeEvent(input$cohortSummaryPanel,{
        cohortSummaryPanel(input$cohortSummaryPanel)
      })
      exposedCasesPanel <- shiny::reactiveVal('None')
      shiny::observeEvent(input$exposedCasesPanel,{
        exposedCasesPanel(input$exposedCasesPanel)
      })
                          
     currentTab <- shiny::reactive({
       if(mainPanel() == "Cohort Summary" & cohortSummaryPanel() == 'Cohort Comparison'){
         return('Cohort Comparison')
       }
       if(mainPanel() == "Cohort Summary" & cohortSummaryPanel() == 'Database Comparison'){
         return('Database Comparison')
       }
       if(mainPanel() == "Exposed Cases Summary" & exposedCasesPanel() ==  'Risk Factor'){
         return('Risk Factor')
       }
       if(mainPanel() == "Exposed Cases Summary" & exposedCasesPanel() ==  'Case Series'){
         return('Case Series')
       }
       if(mainPanel() == "Exposed Cases Summary" & exposedCasesPanel() ==  'Time-to-event'){
         return('Time-to-event')
       }
       if(mainPanel() == "Exposed Cases Summary" & exposedCasesPanel() ==  'Dechallenge Rechallenge'){
         return('Dechallenge Rechallenge')
       }
       if(mainPanel() == "Cohort Incidence"){
         return("Cohort Incidence")
       } 
       
       return('None')
     }) 
      
      # =============================
      #  MODULE SERVERS
      # =============================
      # store what servers have been loaded and only load them the first time
      # when the corresponding tab is loaded
      previouslyLoaded <- shiny::reactiveVal(c())
      
      # only render the tab when selected
      shiny::observeEvent(currentTab(), {
      # =============================
      #   Cohort Comparison
      # =============================
        if(currentTab() == 'Cohort Comparison'){
          if(!"Cohort Comparison" %in% previouslyLoaded()){
            characterizationCohortComparisonServer(
              id = 'cohortComparisonTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              options = options,
              parents = parents,
              parentIndex = parentIndex,
              subTargetId = subTargetId
            )
            previouslyLoaded(c(previouslyLoaded(), "Cohort Comparison"))
          }
        }
        
        # =============================
        #   Database Comparison
        # =============================
        if(currentTab() == "Database Comparison"){
          if(!"Database Comparison" %in% previouslyLoaded()){
            characterizationDatabaseComparisonServer(
              id = 'databaseComparisonTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              options = options,
              parents = parents,
              parentIndex = parentIndex,
              subTargetId = subTargetId
            )
            previouslyLoaded(c(previouslyLoaded(), "Database Comparison"))
          }
        }
        
      # =============================
      #   Risk Factor
      # =============================
        if(currentTab() == "Risk Factor"){
          if(!"Risk Factor" %in% previouslyLoaded()){
            characterizationRiskFactorServer(
              id = 'riskFactorTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = subTargetId,
              outcomeId = outcomeId
            )
            previouslyLoaded(c(previouslyLoaded(), "Risk Factor"))
          }
        }
        
        # =============================
        #   Case Series
        # =============================
        if(currentTab() == 'Case Series'){
          if(!"Case Series" %in% previouslyLoaded()){
            characterizationCaseSeriesServer(
              id = 'caseSeriesTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = subTargetId,
              outcomeId = outcomeId
            )
            previouslyLoaded(c(previouslyLoaded(), "Case Series"))
          }
        }
        
        # =============================
        #   Time-to-event
        # =============================
        if(currentTab() == 'Time-to-event'){
          if(!"Time-to-event" %in% previouslyLoaded()){
            characterizationTimeToEventServer(
              id = 'timeToEventTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = subTargetId,
              outcomeId = outcomeId
            )
            previouslyLoaded(c(previouslyLoaded(), "Time-to-event"))
          }
        }
        
        # =============================
        #   Dechallenge Rechallenge
        # =============================
        if(currentTab() == 'Dechallenge Rechallenge'){
          if(!"Dechallenge Rechallenge" %in% previouslyLoaded()){
            characterizationDechallengeRechallengeServer(
              id = 'dechallengeRechallengeTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = subTargetId,
              outcomeId = outcomeId
            )
            previouslyLoaded(c(previouslyLoaded(), "Dechallenge Rechallenge"))
          }
        }
        

      # =============================
      #   Incidence
      # =============================
        if(currentTab() == "Cohort Incidence"){
          if(!"Incidence Results" %in% previouslyLoaded()){
            characterizationIncidenceServer( 
              id = 'cohortIncidenceTab',
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              options = options,
              parents = parents,
              parentIndex = parentIndex, # reactive
              outcomes = outcomes, # reactive
              targetIds = subTargetId# reactive
            )
            previouslyLoaded(c(previouslyLoaded(), "Incidence Results"))
          }
        }
      })


      
    }
  )
}

getCharacterizationTypes <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  results <- c()
  
  conn <- DatabaseConnector::connect(
    connectionDetails = connectionHandler$connectionDetails
    )
  on.exit(DatabaseConnector::disconnect(conn))
  tbls <- DatabaseConnector::getTableNames(
    connection = conn,
    databaseSchema = resultDatabaseSettings$schema
  )
  
  #"Database Comparison" - TODO check multiple databases?
  
  # check Targets
  if(sum(paste0(
    resultDatabaseSettings$cTablePrefix,
    c('covariates', 'covariate_ref', 'cohort_details', 'settings')
  ) %in% tbls) == 4){
    results <- rbind(
      results, 
      c("Database Comparison",'Cohort Summary', 'cohortSummaryPanel'), 
      c("Cohort Comparison",'Cohort Summary', 'cohortSummaryPanel'), 
      c("Risk Factor",'Exposed Cases Summary', 'exposedCasesPanel'), 
      c("Case Series",'Exposed Cases Summary', 'exposedCasesPanel')
    )
  }
  
  # check dechallenge_rechallenge
  if(paste0(
    resultDatabaseSettings$cTablePrefix,
    'dechallenge_rechallenge'
  ) %in% tbls){
    results <- rbind(
      results, 
      c("Dechallenge Rechallenge",'Exposed Cases Summary', 'exposedCasesPanel')
    )
  }
  
  # check time_to_event
  if(paste0(
    resultDatabaseSettings$cTablePrefix,
    'time_to_event'
  ) %in% tbls){
    results <- rbind(
      results, 
      c("Time-to-event",'Exposed Cases Summary', 'exposedCasesPanel')
    )
  }
  
  # check incidence
  if(paste0(
    resultDatabaseSettings$incidenceTablePrefix,
    'incidence_summary'
  ) %in% tbls){
    results <- rbind(
      results, 
      c("Incidence Results",'Cohort Incidence', 'cohortIncidencePanel')
    )
  }
  
  
  
  return(list(
    mainPanel = unique(results[,2]),
    subPanel = unique(results[,1])
  ))
}

# TODO add tte and dechal as include options
characterizationGetOptions <- function(
    connectionHandler,
    resultDatabaseSettings,
    includeAggregate,
    includeIncidence
    ){
  
  # get cohorts
  cg <- connectionHandler$queryDb(
    sql = 'select * from @schema.@cg_table_prefixcohort_definition
    ORDER BY cohort_name;',
    schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  # check whether lookup table in CI exists
  ciLookup <- !is.null(tryCatch({
    connectionHandler$queryDb(
      sql = 'select * from @schema.@ci_table_prefixcohorts_lookup limit 1;',
      schema = resultDatabaseSettings$schema,
      ci_table_prefix = resultDatabaseSettings$incidenceTablePrefix
    )
  }, error = function(e){return(NULL)}))
  

TnOs <- connectionHandler$queryDb(
  sql = "
select distinct temp.*, c.cohort_name 

from 
(
{@include_aggregate} ? {
select distinct  
target_cohort_id,
outcome_cohort_id
from @schema.@c_table_prefixcohort_details
where cohort_type = 'TnO'

{@include_incidence} ? {
union
}
}

{@include_incidence} ? {
{@ci_lookup} ?
{select * from @schema.@ci_table_prefixcohorts_lookup} : {
select distinct target_cohort_definition_id as target_cohort_id, 
outcome_cohort_definition_id as outcome_cohort_id
from
@schema.@ci_table_prefixincidence_summary
}

}
) temp
inner join 
@schema.@cg_table_prefixcohort_definition c
on temp.outcome_cohort_id = c.cohort_definition_id

;",
  schema = resultDatabaseSettings$schema,
  c_table_prefix = resultDatabaseSettings$cTablePrefix,
  cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
  ci_table_prefix = resultDatabaseSettings$incidenceTablePrefix,
  include_incidence = includeIncidence,
  include_aggregate = includeAggregate,
  ci_lookup = ciLookup
)
# fix backwards compatability
if(!'isSubset' %in% colnames(cg)){
  cg$isSubset <- NA
}
if(!'subsetParent' %in% colnames(cg)){
  cg$subsetParent <- cg$cohortDefinitionId
}
if(!'subsetDefinitionId' %in% colnames(cg)){
  cg$subsetDefinitionId <- cg$cohortDefinitionId
}
cg$subsetParent[is.na(cg$isSubset)] <- cg$cohortDefinitionId
cg$subsetDefinitionId[is.na(cg$isSubset)] <- cg$cohortDefinitionId
cg$isSubset[is.na(cg$isSubset)] <- 0

parents <- unique(cg$cohortDefinitionId[cg$isSubset == 0])
results <- lapply(parents, function(id){
  list(
    cohortName = cg$cohortName[cg$cohortDefinitionId == id],
    cohortId = id,
    children = lapply(cg$cohortDefinitionId[cg$subsetParent == id], function(sid){
      list(
        subsetName = cg$cohortName[cg$cohortDefinitionId == sid],
        subsetId = sid,
        outcomeIds = unique(TnOs$outcomeCohortId[TnOs$targetCohortId == sid]),
        outcomeNames = unique(TnOs$cohortName[TnOs$targetCohortId == sid])
        # add outcomes from case exposures
      )
    }
    )
  )
})

return(results)

}

characterizationGetParents <- function(options){
  parentTs <- unlist(lapply(options, function(x) x$cohortId))
  names(parentTs) <- unlist(lapply(options, function(x) x$cohortName))
  
  return(parentTs)
}

characterizationGetChildren <- function(options, index){
  children <- unlist(lapply(options[[index]]$children, function(x) x$subsetId))
  names(children) <- unlist(lapply(options[[index]]$children, function(x) x$subsetName))
  
  return(children)
}

characterizationGetOutcomes <- function(options, index){
  result <- unique(
    do.call(
      'rbind', 
      lapply(
        X = options[[index]]$children, 
        FUN = function(x) data.frame(ids = x$outcomeIds, names = x$outcomeNames)
        )
      )
    )
  
  outcomes <- result$ids
  names(outcomes) <- result$names
  
  # sort the outcomes alphabetically
  outcomes <- outcomes[order(names(outcomes))]
  return(outcomes)
}

