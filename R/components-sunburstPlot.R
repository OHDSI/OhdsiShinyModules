library(htmltools)


sunburstPlotViewer <- function(id = "sunburst") {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::fluidRow(
      shiny::uiOutput(ns("sunburstPlot")),
    )
  )
}


sunburstPlotServer <- function(
    id = "sunburst",
    pathwayTable,
    plotWidth,
    plotHeight,
    legendHeight = 30,
    legendWidth = 400,
    filterColumn,
    sunburstList,
    filenamePrefix = "sunburst"
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!inherits(df, "reactive")) {
        pathwayTable <- shiny::reactiveVal(pathwayTable)
      }
      
      widgetList <- reactiveVal(NULL)
      
      legendSpecs <- list(w = legendWidth, h = legendHeight)
      
      #---- Generate sunburst plots ----
      observeEvent(pathwayTable(), {
        widgets <- list()
        
        if (nrow(pathwayTable()) > 0) {
          # 1. cache all the plots
          for (idx in seq_len(length(sunburstList))) {
            sunburst <- sunburstList[[idx]]
            
            filterTable <- pathwayTable() %>%
              dplyr::filter(.data[[filterColumn]] == sunburst) %>%
              dplyr::select(pathway, freq)
            
            if (nrow(filterTable) > 0) {
              widget <- sunburstR::sunburst(
                filterTable,
                sortFunction = htmlwidgets::JS("function (a, b) {return a.value - b.value;}"),
                width = plotWidth,
                height = plotHeight,
                legend = legendSpecs
              )
              
              
              id <- paste0("widget_", idx)
              
              widgets[[id]] <- widget
            }
          }
          
          widgetList(widgets)
        } else {
          output$sunburstPlot <- shiny::renderUI(
            shiny::column(
              width = 12,
              shiny::helpText("No analyses results to show")
            )
          )
        }
      })
      
      #---- render ui ----
      output$sunburstPlot <- shiny::renderUI({
        widgets <- widgetList()
        
        ui_output <- lapply(seq_along(sunburstList), function(idx) {
          id <- paste0("widget_", idx)
          
          sunburst <- sunburstList[[idx]]
          
          exists <- id %in% names(widgets) && !is.null(widgets[[id]])
          
          if (!exists) {
            return(NULL)
          }
          
          shiny::column(
            width = 12,
            shiny::h2(sunburst),
            shiny::downloadButton(
              outputId = session$ns(paste0("download_", id)),
              label = "Download"
            ),
            widgets[[id]],
          )
        })
        
        ui_output <- Filter(Negate(is.null), ui_output)
        
        shiny::tagList(ui_output)
      })
      
      
      #---- render server ----
      observeEvent(widgetList(), {
        widgets <- widgetList()
        if (length(widgets) == 0) {
          return()
        }
        
        for (idx in seq_len(length(sunburstList))) {
          local({
            idx <- idx
            id <- paste0("widget_", idx)
            
            # if this id doesn't exist in widgets, skip
            if (!(id %in% names(widgets)) || is.null(widgets[[id]])) {
              return(NULL)
            }
            
            widget <- widgets[[id]]
            sunburst <- sunburstList[[idx]]
            sunburstName <- gsub("[^A-Za-z0-9_\\-]", "_", sunburst)
            
            downloadId <- paste0("download_", id)
            
            output[[downloadId]] <- shiny::downloadHandler(
              filename = function() {
                paste0(filenamePrefix, "_", sunburstName, ".html")
              },
              content = function(file) {
                htmlwidgets::saveWidget(widget, file, selfcontained = TRUE)
              }
            )
          })
        }
      })
    }
  )
}

