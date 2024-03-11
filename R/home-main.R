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
      shiny::p("Study Name"),
      shiny::p("The study question was..."),
      next_label="Execution Information"
    ),
    shinyglide::screen(
      shiny::p("Execution Information"),
      shiny::uiOutput(ns("nDataSources")),
      shiny::uiOutput(ns("datasourceTextSummary")),
      
      next_label="Prediction Results"
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
      height = "350px",
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
    resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$cool_plot <- shiny::renderPlot({
        graphics::hist(
          stats::rnorm(input$n), 
          main = paste("n =", input$n), 
          xlab = ""
          )
      })
      
      # this is the number of analyses in the design
      dfFromJsonSpec <- data.frame(
        module = c(
          'characterization', 
          'characterization', 
          'characterization', 
          'cohort-diagnostics', 
          'cohort-generator',
          'cohort-method',
          'datasources',
          'evidence-synth',
          'patient-level-prediction',
          'phevaluator',
          'sccs'
        ),
        type = c(
          'number of time-to-event analyses',
          'number of risk-factor analyses',
          'number of dechallenge-rechallenge analyses',
          'number of cohorts',
          'number of cohorts',
          'number of estimations',
          'number of databases',
          'number of meta-estimations',
          'number of prediction models',
          'number of phevaluator analyses', # what is the unit for phenvaluator?
          'number of estimations'
        ),
        value = c(
          4,
          40,
          0,
          24,
          24,
          6,
          6,
          3,
          6,
          0,
          6
        )
      )
      
      # this is the number of analyses with results
      dfFromResults <- data.frame(
        module = c(
          'characterization', 
          'characterization', 
          'characterization', 
          'cohort-diagnostics', 
          'cohort-generator',
          'cohort-method',
          'datasources',
          'evidence-synth',
          'patient-level-prediction',
          'phevaluator',
          'sccs'
        ),
        type = c(
          'number of time-to-event analyses',
          'number of risk-factor analyses',
          'number of dechallenge-rechallenge analyses',
          'number of cohorts',
          'number of cohorts',
          'number of estimations',
          'number of databases',
          'number of meta-estimations',
          'number of prediction models',
          'number of phevaluator analyses', # what is the unit for phenvaluator?
          'number of estimations'
        ),
        value = c(
          4,
          40,
          0,
          24,
          24,
          1,
          1,
          1,
          1,
          0,
          1
        )
      ) %>%
        dplyr::rename(resultValue = value)
      
      dfMerged <- inner_join(dfFromJsonSpec, dfFromResults) %>% 
        dplyr::mutate(propRan = round(resultValue/value, 4)) %>%
        dplyr::mutate(propRan = ifelse(is.nan(propRan), 
                                       0,
                                       propRan) 
                      ) %>%
        dplyr::group_by(module) %>%
        dplyr::mutate(moduleValueSum = sum(value),
                      moduleResultValueSum = sum(resultValue)) %>%
        dplyr::ungroup()
      
      dfMerged$type <- gsub("number of", "", dfMerged$type)
      
      #nCharacterizationAnalysesRan <- dplyr::distinct(dfMerged %>% dplyr::filter(module=="characterization") %>% dplyr::select(moduleValueSum))$moduleValueSum
      
      #output$analysesRunText <- shiny::renderText(paste0("In total, there were ", ))
      
      # Group by 'type' and summarize
      summary_df <- dfMerged %>%
        group_by(module, type) %>%
        summarize(
          total_analyses = sum(value),
          total_ran = sum(resultValue),
          results_ran = sum(resultValue)
        )
      
      # Generate text blocks
      text_blocks <- lapply(unique(summary_df$module), function(module_name) {
        module_summary <- summary_df %>%
          dplyr::filter(module == module_name) %>%
          dplyr::summarise(
            text_chunk = paste(
              "In the", module_name, "module,",
              paste("there were", total_analyses, type, "analyses ran, of which", results_ran, "had results", collapse = ", "),
              ". In total", sum(results_ran), module_name, "results ran."
            )
          )
        return(sub(" \\.", ".", module_summary$text_chunk))
      })
      
      # Convert the list to a character vector
      text_blocks <- unlist(text_blocks)
      
      # Print or use the 'text_blocks' as needed
      output$datasourceTextSummary <- shiny::renderText(text_blocks)
      
      
      datasourcesData <- 
        getDatasourcesData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      
      nDataSources <- dplyr::n_distinct(datasourcesData$cdmSourceName)
      namesDataSources <- dplyr::distinct(datasourcesData, dplyr::pick(dplyr::contains("cdmSourceName")))$cdmSourceName
    
      output$nDataSources <- shiny::renderText(paste0("There were ", nDataSources, " distinct datasources ran in this analysis.")
      )
                                               
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #data extractions
      #pull database meta data table
      getDatasourcesData <- function(
    connectionHandler, 
    resultDatabaseSettings
      ) {
        
        sql <- "SELECT * from @schema.@database_meta_data
  ;"
        return(
          connectionHandler$queryDb(
            sql = sql,
            schema = resultDatabaseSettings$schema,
            database_table = resultDatabaseSettings$databaseTable
          )
        )
      }
      
      
      
      
      
      
      
      
      
      
    }
  )
}