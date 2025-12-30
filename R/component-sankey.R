sankeyPlotViewer <- function(id = "sankey") {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::fluidRow(
      shiny::div(
        style = "width:90%; overflow-x:auto; padding:10px; margin-left:10px;;",
        shiny::uiOutput(ns("sankeyPlot"))
      )
    )
  )
}


# filerColumn sets the column to split by
# sankeyList provides values in the filterColumn to create separate grouped sankey plots
sankeyPlotServer <- function(
  id,
  pathwayTable,
  sankeyList,
  filterColumn,
  filenamePrefix = "sankey",
  ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!inherits(df, "reactive")) {
        pathwayTable <- shiny::reactiveVal(pathwayTable)
      }

      widgetList <- reactiveVal(NULL)

      #---- Generate sankey plots ----
      observeEvent(pathwayTable(), {
        widgets <- list()

        if (nrow(pathwayTable()) > 0) {
          # 1. cache all the plots
          for (idx in seq_len(length(sankeyList))) {
            sankey <- sankeyList[[idx]]

            filterTable <- pathwayTable() %>%
              dplyr::filter(.data[[filterColumn]] == sankey) %>%
              dplyr::select(pathway, freq, sex, age, indexYear) %>%
              dplyr::rename(index_year = indexYear)

            if (nrow(filterTable) > 0) {
              widget <- TreatmentPatterns::createSankeyDiagram(treatmentPathways = filterTable, ...)


              id <- paste0("widgit_", idx)

              widgets[[id]] <- widget
            }
          }

          widgetList(widgets)
        } else {
          output$sankeyPlot <- shiny::renderUI(
            shiny::column(
              width = 12,
              shiny::helpText("No analyses results to show")
            )
          )
        }
      })

      #---- render ui ----
      output$sankeyPlot <- shiny::renderUI({
        widgets <- widgetList()

        ui_output <- lapply(seq_along(sankeyList), function(idx) {
          id <- paste0("widgit_", idx)

          sankey <- sankeyList[[idx]]

          exists <- id %in% names(widgets) && !is.null(widgets[[id]])

          if (!exists) {
            return(NULL)
          }

          shiny::column(
            width = 12,
            shiny::h2(sankey),
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

        for (idx in seq_len(length(sankeyList))) {
          local({
            idx <- idx
            id <- paste0("widgit_", idx)

            # if this id doesn't exist in widgets, skip
            if (!(id %in% names(widgets)) || is.null(widgets[[id]])) {
              return(NULL)
            }

            widget <- widgets[[id]]
            sankey <- sankeyList[[idx]]
            sankeyName <- gsub("[^A-Za-z0-9_\\-]", "_", sankey)

            downloadId <- paste0("download_", id)

            output[[downloadId]] <- shiny::downloadHandler(
              filename = function() {
                paste0(filenamePrefix, "_", sankeyName, ".html")
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
