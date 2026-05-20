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
    filterColumn,
    sunburstList,
    filenamePrefix = "sunburst",
    ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      pathwayTable <- shiny::reactiveVal(pathwayTable)
      
      widgetList <- reactiveVal(NULL)
    
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
                ...
              ) %>% 
              htmlwidgets::onRender(
                "function(el, x){
                  function labels(){
                    const legend = el.querySelector('.sunburst-legend');
                    const svg = legend.querySelector('svg');
                    
                    legend.style.position = 'relative';
                    const lb = legend.getBoundingClientRect();
                    // on rerenders remove old html
                    legend.querySelectorAll('.legend-html-label').forEach(n => n.remove());
                    svg.querySelectorAll('g').forEach(function(g){
                      const text = g.querySelector('text');
                      const label = text.textContent; 
                      text.remove();
                      
                      svg.style.position = 'relative';
                      svg.style.zIndex = '1';          
                       
                      const gb = g.getBoundingClientRect();
                      const div = document.createElement('div');
                      div.className = 'legend-html-label';
                      div.textContent = label;
                      div.style.position = 'absolute';
                      div.style.left = (gb.left - lb.left) + 'px';
                      div.style.top  = (gb.top  - lb.top)  + 'px';
                      div.style.width  = gb.width  + 'px';
                      div.style.height = gb.height + 'px';
                      div.style.lineHeight = gb.height + 'px';
                      div.style.textAlign = 'center';
                      div.style.whiteSpace = 'nowrap';
                      div.style.overflowX = 'auto';
                      div.style.overflowY = 'hidden';
                      div.style.pointerEvents = 'auto';
                      div.style.zIndex = '2'
                      legend.style.position = 'relative';

                      
                      //hide scroller
                      div.style.scrollbarWidth = 'none';     
                      div.style.msOverflowStyle = 'none';
                      div.style.webkitOverflowScrolling = 'touch';
                      div.style.pointerEvents = 'auto';
                      
                      legend.appendChild(div);
                    });
                  }
                  
                  labels();
                  window.addEventListener('resize', labels);
                }"
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

