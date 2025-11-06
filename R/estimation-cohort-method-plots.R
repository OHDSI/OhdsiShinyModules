estimationCmPlotsViewer <- function(id=1) {
  ns <- shiny::NS(id)
  shinyWidgets::addSpinner(
    shiny::plotOutput(ns('esCohortMethodPlot')),
    spin = 'rotating-plane'
  )
}


estimationCmPlotsServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1),
    cmData
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      height <- shiny::reactive({
        if(is.null(cmData()$targetName)){
          return(100)
        }
        length(unique(cmData()$targetName))*250 + 250
      })
      
      output$esCohortMethodPlot <- shiny::renderPlot(
        estimationCreateCmPlot(
          data = cmData
        ), 
        height = height
      )
      
    }
  )
}

estimationCreateCmPlot <- function(data) {
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

  if(is.null(data$comparatorName)){
    shiny::showNotification('No results to plot')
    return(NULL)
  }
  
  
  renameDf <- data.frame(
    shortName = paste0(
      1:length(unique(data$comparatorName)),
      ') ', 
      substring(sort(unique(data$comparatorName)), 1,50),
      '...'
    ),
    comparatorName = sort(unique(data$comparatorName))
  )
  
  
  data <- merge(
    data, 
    renameDf,
    by = "comparatorName"
  )
  
  # make sure bayesian is at top
  db <- unique(data$databaseName)
  bInd <- grep('bayesian', tolower(db))
  b <- db[bInd]
  if(length(bInd) > 0){
    withoutb <- db[-bInd]
    data$databaseName <- factor(
      x = data$databaseName, 
      levels = c(b, sort(withoutb))
    )
  }
  metadata <- data[data$databaseName == b,]
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8)
  
  ### Add table above the graph
  renameDf$comparatorName <- sapply(
    strwrap(renameDf$comparatorName, width = 150, simplify = FALSE), 
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
  
  for(targetName in unique(data$targetName)){ # per targets
    
  title <- sprintf("%s", targetName)
  plotList[[length(plotList) + 1]] <- ggplot2::ggplot(
    data = data %>% dplyr::filter(.data$targetName == !!targetName),
    ggplot2::aes(x = .data$calibratedRr, y = .data$shortName)) +
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
      "Effect size (Hazard Ratio)", 
      breaks = breaks, 
      labels = breaks
    ) +
    
    # shade the bayesian 
    ggplot2::geom_rect(
      data =  metadata %>% dplyr::filter(.data$targetName == !!targetName),
      ggplot2::aes(fill = .data$databaseName),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.2
    ) +
    
    ggplot2::coord_cartesian(xlim = c(0.1, 10)) + 
    ggplot2::facet_grid(.data$databaseName ~ .data$description)  +
    ggplot2::ggtitle(title) +
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
