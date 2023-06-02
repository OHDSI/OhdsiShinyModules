

#inputs: data, named list of colDef options, where name is name of each column,
#potentially:
#output: download buttons, table, and column selector



#' Result Table Viewer
#'
#' @param id string
#' @param downloadedFileName string, desired name of downloaded data file. can use the name from the module that is being used
#'
#' @return shiny module UI
#' @export
#'
resultTableViewer <- function(id = "result-table",
                              downloadedFileName = NULL) {
  ns <- shiny::NS(id)
  shiny::div(# UI
    shinydashboard::box(
      width = "100%",
      title = shiny::span(shiny::icon("table"), "Results"),
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(width = 7,
                        shiny::uiOutput(ns("columnSelector"))),
          shiny::column(
            width = 2,
            shiny::downloadButton(
              ns('downloadDataFull'),
              label = "Download (Full)",
              icon = shiny::icon("download")
            )
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              ns('downloadDataFiltered'),
              label = "Download (Filtered)",
              icon = shiny::icon("download"),
              onclick = paste0(
                "Reactable.downloadDataCSV('",
                ns('resultData'),
                "', 'result-data-filtered-",
                downloadedFileName,
                Sys.Date(),
                ".csv')"
              )
            )
          )
        ),
        shiny::fluidRow(
          shinycssloaders::withSpinner(reactable::reactableOutput(outputId = ns("resultData"),
                                                                  width = "100%"))
        )
      )
    ))
}




#tooltip function
withTooltip <- function(value, tooltip, ...) {
  shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
             tippy::tippy(value, tooltip, ...))
}

# customColDefs needs to be named list of colDefs
# example usage:
# Define custom colDefs for the Name and Age columns
# custom_colDefs <- list(
#   mpg = reactable::colDef(align = "left",
#                format = reactable::colFormat(digits = 2),
#                header = withTooltip("MPG column name", "MPG tooltip")),
#   disp = reactable::colDef(align = "center",
#                 header = withTooltip("Disp column name", "Disp tooltip"))
# )


create_colDefs_list <- function(df, customColDefs = NULL) {
  # Get the column names of the input data frame
  col_names <- colnames(df)
  
  # Create an empty list to store the colDefs
  colDefs_list <- vector("list", length = length(col_names))
  names(colDefs_list) <- col_names
  
  # Define custom colDefs for each column if provided
  if (!is.null(customColDefs)) {
    for (col in seq_along(col_names)) {
      if (col_names[col] %in% names(customColDefs)) {
        colDefs_list[[col]] <- customColDefs[[col_names[col]]]
      } else {
        colDefs_list[[col]] <- reactable::colDef(name = col_names[col])
      }
      
      if (!is.null(customColDefs[[col_names[col]]]$header)) {
        colDefs_list[[col]]$header <- customColDefs[[col_names[col]]]$header
      }
      
      if (!is.null(customColDefs[[col_names[col]]]$tooltip)) {
        colDefs_list[[col]]$header <-
          withTooltip(colDefs_list[[col]]$header, customColDefs[[col_names[col]]]$tooltip)
      }
    }
  } else {
    # Define default colDefs if customColDefs is not provided
    for (col in seq_along(col_names)) {
      colDefs_list[[col]] <- reactable::colDef(name = col_names[col])
    }
  }
  
  # Return the list of colDefs
  return(colDefs_list)
}


ohdsiReactableTheme <- reactable::reactableTheme(
  color = "white",
  backgroundColor = "#003142",
  stripedColor = "#333333",
  highlightColor = "#f19119",
  style = list(
    fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI,
    Roboto, Helvetica, Arial, sans-serif, Apple Color Emoji, 
    Segoe UI Emoji, Segoe UI Symbol"
  )
  #,
  #headerStyle = list(
  #)
)



#' Result Table Server
#'
#' @param id string, table id must match resultsTableViewer function
#' @param df reactive that returns a data frame
#' @param colDefsInput named list of reactable::colDefs
#' @param downloadedFileName string, desired name of downloaded data file. can use the name from the module that is being used
#'
#' @return shiny module server
#' @export
#'
resultTableServer <- function(id, #string
                              df, #data.frame
                              colDefsInput, #named list
                              downloadedFileName = NULL #string
) #list of colDefs, can use checkmate::assertList, need a check that makes sure names = columns) {
  shiny::moduleServer(id,
                      function(input, output, session) {
                        output$columnSelector <- shiny::renderUI({
                          shinyWidgets::pickerInput(
                            inputId = session$ns('dataCols'),
                            label = 'Select Columns to Display: ',
                            choices = colnames(df()),
                            selected = colnames(df()),
                            choicesOpt = list(style = rep_len("color: black;", 999)),
                            multiple = T,
                            options = shinyWidgets::pickerOptions(
                              actionsBox = TRUE,
                              liveSearch = TRUE,
                              size = 10,
                              liveSearchStyle = "contains",
                              liveSearchPlaceholder = "Type here to search",
                              virtualScroll = 50
                            ),
                            width = "75%"
                          )
                          
                        })
                        
                        #need to try adding browser() to all reactives to see why selected cols isnt working
                    
                        
                        colDefs <-
                          shiny::reactive({
                            create_colDefs_list(df = df()[, input$dataCols],
                                                              customColDefs = colDefsInput)
                          })
                        
                        fullData <- shiny::reactive({
                          df()
                          })
                        
                        
                        output$resultData <-
                          reactable::renderReactable({
                            
                            data = df()[, input$dataCols, drop = F]
                            
                            tryCatch({
                              
                              reactable::reactable(
                                data,
                                columns = colDefs(),
                                #these can be turned on/off and will overwrite colDef args
                                sortable = TRUE,
                                resizable = TRUE,
                                filterable = TRUE,
                                searchable = TRUE,
                                showPageSizeOptions = TRUE,
                                outlined = TRUE,
                                showSortIcon = TRUE,
                                striped = TRUE,
                                highlight = TRUE,
                                defaultColDef = reactable::colDef(align = "left")
                                #, experimental
                                #theme = ohdsiReactableTheme
                              )}
                              ,

                              error = function(e){
                                shiny::showNotification("No columns selected!"); return(NULL)

                              }
                            )
                            
                            
                          })
                        
                        # download full data button
                        output$downloadDataFull <- shiny::downloadHandler(
                          filename = function() {
                            paste('data-full-', downloadedFileName, Sys.Date(), '.csv', sep = '')
                          },
                          content = function(con) {
                            utils::write.csv(fullData(), con,
                                             row.names = F)
                          }
                        )
                      })