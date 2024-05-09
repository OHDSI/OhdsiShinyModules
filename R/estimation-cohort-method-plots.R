estimationCmPlotsViewer <- function(id=1) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns('esCohortMethodPlot'))
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
      
      output$esCohortMethodPlot <- shiny::renderPlot(
        estimationCreateCmPlot(
          data = cmData
        )
      )
      
    }
  )
}

estimationCreateCmPlot <- function(data) {
  print('PLOT')
  data <- data()
  data <- data[!is.na(data$calibratedRr),]
  data$database <- data$cdmSourceAbbreviation
  
  print(data)
  if(is.null(data$comparator)){
    return(NULL)
  }
  
  
  # TODO create plot for each target
  
  compText <- data.frame(
    comparatorText = paste0('Comp', 1:length(unique(data$comparator))),
    comparator = unique(data$comparator)
  )
  
  data <- merge(
    data, 
    compText,
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
  title <- sprintf("%s", data$target[1])
  plot <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = .data$calibratedRr, y = .data$comparatorText)) +
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
      data =  metadata,
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
    ) + 
    ggplot2::labs(
      caption = paste(
        apply(
          X = compText, 
          MARGIN = 1, 
          FUN = function(x){paste0(paste(substring(x, 1, 50),collapse = ': ', sep=':'), '...')}
        ), 
        collapse = '\n ')
    )
  
  return(plot)
}
