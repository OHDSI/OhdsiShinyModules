estimationSccsPlotsViewer <- function(id=1) {
  ns <- shiny::NS(id)
  shinyWidgets::addSpinner(
    output = shiny::plotOutput(ns('esSccsPlot')), 
    spin = 'rotating-plane'
  )
}


estimationSccsPlotsServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1),
    sccsData
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      height <- shiny::reactive({
        if(is.null(sccsData()$indication)){
          return(100)
        }
        length(unique(sccsData()$indication))*200 + 200
      })
      
      output$esSccsPlot <- shiny::renderPlot(
        estimationCreateSccsPlot(
          data = sccsData
        ),
        height = height
      )
      
    }
  )
}

estimationCreateSccsPlot <- function(data) {
  data <- data()
  if(nrow(data) == 0){
    shiny::showNotification('No results to plot')
    return(NULL)
  }
  data <- data[!is.na(data$calibratedRr),]
  if(nrow(data) == 0){
    shiny::showNotification('No results to plot')
    return(NULL)
  }
  data$database <- data$databaseName
  data$type <- data$covariateName
  data$indication[is.null(data$indication)] <- 'no indication'
  data$indication[is.na(data$indication)] <- 'no indication'
  
  if(is.null(data)){
    shiny::showNotification('No results to plot')
    return(NULL)
  }
  if(nrow(data) == 0){
    shiny::showNotification('No results to plot')
    return(NULL)
  }
  
  # change the description to add at bottom
  renameDf <- data.frame(
    shortName = paste0(
      1:length(unique(data$description)),
      ') ', 
      substring(sort(unique(data$description)), 1, 15),
      '...'
    ),
    description = sort(unique(data$description))
  )
  
  data <- merge(
    x = data,
    y = renameDf, 
    by = 'description'
  )
  
  # make sure bayesian is at top
  db <- unique(data$database)
  bInd <- grep('bayesian', tolower(db))
  b <- db[bInd]
  if(length(bInd) > 0){
    withoutb <- db[-bInd]
    data$database <- factor(
      x = data$database, 
      levels = c(b, sort(withoutb))
    )
  }
  # this should be empty if no meta
  metadata <- data[data$database == b,]
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8)
  
  # TODO loop over target-indications pairs
  
  ### Add table above the graph
  renameDf$description <- sapply(
    strwrap(renameDf$description, width = 50, simplify = FALSE), 
    paste, 
    collapse = "\n"
  )
  
  tt <- gridExtra::ttheme_default(
    base_size = 8,
    colhead=list(fg_params = list(parse=TRUE))
  )
  tbl <- gridExtra::tableGrob(
    renameDf, 
    rows=NULL, 
    theme=tt
  )
  plotList <- list(tbl) # adding table first
  
  for(indication in unique(data$indication)){ # TODO do indication + target combo?
  plotList[[length(plotList)+1]] <- ggplot2::ggplot(
    data = data %>% dplyr::filter(.data$indication == !!indication),  #restrict to indication
    ggplot2::aes(x = .data$calibratedRr, y = .data$type)
  ) +
    ggplot2::geom_vline(xintercept = 1, size = 0.5) +
    ggplot2::geom_point(color = "#000088", alpha = 0.8) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = .data$calibratedCi95Lb, 
        xmax = .data$calibratedCi95Ub
      ), 
      height = 0.5, 
      color = "#000088", 
      alpha = 0.8
    ) +
    ggplot2::scale_x_log10(
      "Effect size (Incidence Rate Ratio)", 
      breaks = breaks, 
      labels = breaks
    ) +
    
    # shade the bayesian 
    ggplot2::geom_rect(
      data =  metadata  %>% dplyr::filter(.data$indication == !!indication),
      ggplot2::aes(fill = .data$database),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.2
    ) +
    
    ggplot2::coord_cartesian(xlim = c(0.1, 10)) + 
    ggplot2::facet_grid(.data$database ~ .data$shortName)  +
    ggplot2::ggtitle(indication) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_text(angle = 0),
      legend.position = "none"
    )
}
  
  plot <- do.call(
    gridExtra::grid.arrange, 
    list(grobs = plotList, ncol =1)
    )
  
  return(plot)
}
