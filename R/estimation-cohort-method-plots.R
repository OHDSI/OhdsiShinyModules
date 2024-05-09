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
        if(is.null(cmData()$target)){
          return(100)
        }
        length(unique(cmData()$target))*250 + 250
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
  data <- data[!is.na(data$calibratedRr),]
  data$database <- data$cdmSourceAbbreviation

  if(is.null(data$comparator)){
    return(NULL)
  }
  
  
  renameDf <- data.frame(
    shortName = paste0(
      1:length(unique(data$comparator)),
      ') ', 
      substring(sort(unique(data$comparator)), 1,50),
      '...'
    ),
    comparator = sort(unique(data$comparator))
  )
  
  
  data <- merge(
    data, 
    renameDf,
    by = "comparator"
  )
  
  # make sure bayesian is at top
  db <- unique(data$database)
  bInd <- grep('bayesian', tolower(db))
  withoutb <- db[-bInd]
  b <- db[bInd]
  data$database <- factor(
    x = data$database, 
    levels = c(b, sort(withoutb))
  )
  metadata <- data[data$database == b,]
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8)
  
  ### Add table above the graph
  renameDf$comparator <- sapply(
    strwrap(renameDf$comparator, width = 150, simplify = FALSE), 
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
  
  for(target in unique(data$target)){ # per targets
    
  title <- sprintf("%s", target)
  plotList[[length(plotList) + 1]] <- ggplot2::ggplot(
    data = data %>% dplyr::filter(.data$target == !!target),
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
      data =  metadata %>% dplyr::filter(.data$target == !!target),
      ggplot2::aes(fill = .data$database),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.2
    ) +
    
    ggplot2::coord_cartesian(xlim = c(0.1, 10)) + 
    ggplot2::facet_grid(.data$database ~ .data$description)  +
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
