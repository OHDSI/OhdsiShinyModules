#' The location of the home module helper file
#'
#' @details
#' Returns the location of the home helper file
#' 
#' @return
#' string location of the home helper file
#'
#' @export
homeHelperFile <- function(){
  fileLoc <- system.file('home-www', "home.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring home
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the home viewer module
#'
#' @export
homeViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  screens <- list(
    shinyglide::screen(
        shiny::p("Assure Executive Summary"),
        shiny::p("Study Name"),
        shiny::p("The study question was..."),
        next_label="Next: Characterization Results",
        inputSelectionViewer(
          id = ns("input-selection-results")
        )
    ),
    shinyglide::screen(
      shiny::p("Estimation Results"),
      shiny::p("User Inputs are possible"),
      shiny::numericInput(
        inputId = ns("n"),
        label =  "n", 
        value = 10, 
        min = 10
        ),
      next_label="Prediction Results"
    ),
    shinyglide::screen(
      shiny::p("Cool plot here"),
      shiny::plotOutput(ns("cool_plot"))
    )
  )
  
  shinydashboard::box(
    status = 'info', width = 12,
    title =  shiny::span( shiny::icon("house"), "Executive Summary"),
    solidHeader = TRUE,
    
    shinyglide::glide(
      height = "500px",
      screens
    )
    
  )
  
}


#' The module server for exploring home
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
#' 
#' @return
#' The server for the home module
#'
#' @export
homeServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #get target, outcome, and TAR options
      options <- getExecutiveSummaryOptions(
        connectionHandler,
        resultDatabaseSettings
      )
      
      # Extract the integers from each TAR string
      tarIntegers <- as.integer(gsub("[^0-9]", "", options$tar))
      # Sort the vector based on the extracted integers
      sortedTars <- options$tar[order(tarIntegers)]
      
      
      # input selection component
      inputSelectedResults <- inputSelectionServer(
        id = "input-selection-results", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 12,
            varName = 'firsttext',
            inputReturn = T,
            uiFunction = 'shiny::div',
            uiInputs = list(
              "Select One of Each: Target, Outcome, and Time at Risk (TAR)",
              style = "font-weight: bold; font-size: 20px; text-align: center; margin-bottom: 20px;"
            )
          ),
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 4,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = options$targetIds,
              selected = options$targetIds[1], #default should be just one (the first)
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
            rowNumber = 2,                           
            columnWidth = 4,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = options$outcomeIds,
              selected = options$outcomeIds[1], #default should be just one (the first)
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
            rowNumber = 2,                           
            columnWidth = 4,
            varName = 'tarFilter',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Select Time at Risk (TAR)',
              choices = sortedTars,
              selected = sortedTars[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                dropupAuto = TRUE,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
      
      
      
     #-------Characterization executive summary
      
      
      
      
      
      
      
      
      
      
      
      
      
      getExecutiveSummaryOptions <- function(
    connectionHandler,
    resultDatabaseSettings
      ){
        
        # shiny::withProgress(message = 'Getting incidence inputs', value = 0, {
        
        sql <- 'select distinct target_cohort_definition_id, target_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
        
        #shiny::incProgress(1/3, detail = paste("Created SQL - Extracting targets"))
        
        targets <- connectionHandler$queryDb(
          sql = sql, 
          result_schema = resultDatabaseSettings$schema,
          incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
        )
        targetIds <- targets$targetCohortDefinitionId
        names(targetIds) <- targets$targetName
        
        sql <- 'select distinct outcome_cohort_definition_id, outcome_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
        
        #shiny::incProgress(2/3, detail = paste("Created SQL - Extracting outcomes"))
        
        outcomes <- connectionHandler$queryDb(
          sql = sql, 
          result_schema = resultDatabaseSettings$schema,
          incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
        )
        
        outcomeIds <- outcomes$outcomeCohortDefinitionId
        names(outcomeIds) <- outcomes$outcomeName
        
        sql <- 'select distinct tar_id, tar_start_with, tar_start_offset, tar_end_with, tar_end_offset
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
        
        #shiny::incProgress(1/3, detail = paste("Created SQL - Extracting targets"))
        
        tars <- connectionHandler$queryDb(
          sql = sql, 
          result_schema = resultDatabaseSettings$schema,
          incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
        )
        tar <- paste0('(',tars$tarStartWith, " + ", tars$tarStartOffset, ') - (', tars$tarEndWith, " + ", tars$tarEndOffset, ')')
        #tar <- tars$tarId
        names(tar) <- paste0('(',tars$tarStartWith, " + ", tars$tarStartOffset, ') - (', tars$tarEndWith, " + ", tars$tarEndOffset, ')')
        
        categoricalChoices <- list(
          "targetName",
          "outcomeName",
          "tar"
        )
        names(categoricalChoices) <- c(
          "Target Cohort",
          "Outcome Cohort", 
          "TAR"
        )
        
        return(
          list(
            targetIds = targetIds,
            outcomeIds = outcomeIds,
            tar = tar,
            categoricalChoices = categoricalChoices
          )
        )
        
      }
      
      
      
      
      
      output$cool_plot <- shiny::renderPlot({
        graphics::hist(
          stats::rnorm(input$n), 
          main = paste("n =", input$n), 
          xlab = ""
          )
      })
      
    }
  )
}