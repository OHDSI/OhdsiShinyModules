# used in estimation-power
prepareEstimationFollowUpDistTable <- function(followUpDist) {
  targetRow <- data.frame(Database = followUpDist$databaseId,
                          Cohort = "Target",
                          Min = followUpDist$targetMinDays,
                          P10 = followUpDist$targetP10Days,
                          P25 = followUpDist$targetP25Days,
                          Median = followUpDist$targetMedianDays,
                          P75 = followUpDist$targetP75Days,
                          P90 = followUpDist$targetP90Days,
                          Max = followUpDist$targetMaxDays)
  comparatorRow <- data.frame(Database = followUpDist$databaseId,
                              Cohort = "Comparator",
                              Min = followUpDist$comparatorMinDays,
                              P10 = followUpDist$comparatorP10Days,
                              P25 = followUpDist$comparatorP25Days,
                              Median = followUpDist$comparatorMedianDays,
                              P75 = followUpDist$comparatorP75Days,
                              P90 = followUpDist$comparatorP90Days,
                              Max = followUpDist$comparatorMaxDays)
  table <- rbind(targetRow, comparatorRow)
  table$Min <- formatC(table$Min, big.mark = ",", format = "d")
  table$P10 <- formatC(table$P10, big.mark = ",", format = "d")
  table$P25 <- formatC(table$P25, big.mark = ",", format = "d")
  table$Median <- formatC(table$Median, big.mark = ",", format = "d")
  table$P75 <- formatC(table$P75, big.mark = ",", format = "d")
  table$P90 <- formatC(table$P90, big.mark = ",", format = "d")
  table$Max <- formatC(table$Max, big.mark = ",", format = "d")
  if (length(unique(followUpDist$databaseId)) == 1)
    table$Database <- NULL
  return(table)
}


# used in estimation-power
prepareEstimationPowerTable <- function(mainResults, connectionHandler , resultsSchema, tablePrefix) {
  analyses <- getCohortMethodAnalyses(connectionHandler , resultsSchema, tablePrefix)
  table <- merge(mainResults, analyses)
  alpha <- 0.05
  power <- 0.8
  z1MinAlpha <- stats::qnorm(1 - alpha/2)
  zBeta <- -stats::qnorm(1 - power)
  pA <- table$targetSubjects/(table$targetSubjects + table$comparatorSubjects)
  pB <- 1 - pA
  totalEvents <- abs(table$targetOutcomes) + abs(table$comparatorOutcomes)
  table$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  table$targetYears <- table$targetDays/365.25
  table$comparatorYears <- table$comparatorDays/365.25
  table$targetIr <- 1000 * table$targetOutcomes/table$targetYears
  table$comparatorIr <- 1000 * table$comparatorOutcomes/table$comparatorYears
  table <- table[, c("description",
                     "databaseId",
                     "targetSubjects",
                     "comparatorSubjects",
                     "targetYears",
                     "comparatorYears",
                     "targetOutcomes",
                     "comparatorOutcomes",
                     "targetIr",
                     "comparatorIr",
                     "mdrr")]
  table$targetSubjects <- formatC(table$targetSubjects, big.mark = ",", format = "d")
  table$comparatorSubjects <- formatC(table$comparatorSubjects, big.mark = ",", format = "d")
  table$targetYears <- formatC(table$targetYears, big.mark = ",", format = "d")
  table$comparatorYears <- formatC(table$comparatorYears, big.mark = ",", format = "d")
  table$targetOutcomes <- formatC(table$targetOutcomes, big.mark = ",", format = "d")
  table$comparatorOutcomes <- formatC(table$comparatorOutcomes, big.mark = ",", format = "d")
  table$targetIr <- sprintf("%.2f", table$targetIr)
  table$comparatorIr <- sprintf("%.2f", table$comparatorIr)
  table$mdrr <- sprintf("%.2f", table$mdrr)
  table$targetSubjects <- gsub("^-", "<", table$targetSubjects)
  table$comparatorSubjects <- gsub("^-", "<", table$comparatorSubjects)
  table$targetOutcomes <- gsub("^-", "<", table$targetOutcomes)
  table$comparatorOutcomes <- gsub("^-", "<", table$comparatorOutcomes)
  table$targetIr <- gsub("^-", "<", table$targetIr)
  table$comparatorIr <- gsub("^-", "<", table$comparatorIr)
  idx <- (table$targetOutcomes < 0 | table$comparatorOutcomes < 0)
  table$mdrr[idx] <- paste0(">", table$mdrr[idx])
  return(table)
}

# estimation-subgroups
prepareEstimationSubgroupTable <- function(subgroupResults, output = "latex") {
  rnd <- function(x) {
    ifelse(x > 10, sprintf("%.1f", x), sprintf("%.2f", x))
  }
  
  subgroupResults$hrr <- paste0(rnd(subgroupResults$rrr),
                                " (",
                                rnd(subgroupResults$ci95Lb),
                                " - ",
                                rnd(subgroupResults$ci95Ub),
                                ")")
  
  subgroupResults$hrr[is.na(subgroupResults$rrr)] <- ""
  subgroupResults$p <- sprintf("%.2f", subgroupResults$p)
  subgroupResults$p[subgroupResults$p == "NA"] <- ""
  subgroupResults$calibratedP <- sprintf("%.2f", subgroupResults$calibratedP)
  subgroupResults$calibratedP[subgroupResults$calibratedP == "NA"] <- ""
  
  if (any(grepl("on-treatment", subgroupResults$analysisDescription)) && 
      any(grepl("intent-to-treat", subgroupResults$analysisDescription))) {
    idx <- grepl("on-treatment", subgroupResults$analysisDescription)
    onTreatment <- subgroupResults[idx, c("interactionCovariateName",
                                          "targetSubjects",
                                          "comparatorSubjects",
                                          "hrr",
                                          "p",
                                          "calibratedP")]
    itt <- subgroupResults[!idx, c("interactionCovariateName", "hrr", "p", "calibratedP")]
    colnames(onTreatment)[4:6] <- paste("onTreatment", colnames(onTreatment)[4:6], sep = "_")
    colnames(itt)[2:4] <- paste("itt", colnames(itt)[2:4], sep = "_")
    table <- merge(onTreatment, itt)
  } else {
    table <- subgroupResults[, c("interactionCovariateName",
                                 "targetSubjects",
                                 "comparatorSubjects",
                                 "hrr",
                                 "p",
                                 "calibratedP")]
  } 
  table$interactionCovariateName <- gsub("Subgroup: ", "", table$interactionCovariateName)
  if (output == "latex") {
    table$interactionCovariateName <- gsub(">=", "$\\\\ge$ ", table$interactionCovariateName)
  }
  table$targetSubjects <- formatC(table$targetSubjects, big.mark = ",", format = "d")
  table$targetSubjects <- gsub("^-", "<", table$targetSubjects)
  table$comparatorSubjects <- formatC(table$comparatorSubjects, big.mark = ",", format = "d")
  table$comparatorSubjects <- gsub("^-", "<", table$comparatorSubjects)
  table$comparatorSubjects <- gsub("^<", "$<$", table$comparatorSubjects)
  return(table)
}






# estiamtion-covariateBal
plotEstimationCovariateBalanceSummary <- function(balanceSummary,
                                                  threshold = 0,
                                                  beforeLabel = "Before matching",
                                                  afterLabel = "After matching") {
  balanceSummary <- balanceSummary[rev(order(balanceSummary$databaseId)), ]
  dbs <- data.frame(databaseId = unique(balanceSummary$databaseId),
                    x = 1:length(unique(balanceSummary$databaseId)))
  vizData <- merge(balanceSummary, dbs)
  
  vizData$type <- factor(vizData$type, levels = c(beforeLabel, afterLabel))
  
  plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = .data$x,
                                                ymin = .data$ymin,
                                                lower = .data$lower,
                                                middle = .data$median,
                                                upper = .data$upper,
                                                ymax = .data$ymax,
                                                group = .data$databaseId)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ymin, ymax = .data$ymin), size = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ymax, ymax = .data$ymax), size = 1) +
    ggplot2::geom_boxplot(stat = "identity", fill = grDevices::rgb(0, 0, 0.8, alpha = 0.25), size = 1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(vizData$x) + 1.75)) +
    ggplot2::scale_y_continuous("Standardized difference of mean") +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(~type) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(color = "#AAAAAA"),
                   panel.background = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 11),
                   axis.title.x = ggplot2::element_text(size = 11),
                   axis.ticks.x = ggplot2::element_line(color = "#AAAAAA"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = 11),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(threshold, -threshold), linetype = "dotted")
  }
  after <- vizData[vizData$type == afterLabel, ]
  after$max <- pmax(abs(after$ymin), abs(after$ymax))
  text <- data.frame(y = rep(c(after$x, nrow(after) + 1.25) , 3),
                     x = rep(c(1,2,3), each = nrow(after) + 1),
                     label = c(c(as.character(after$databaseId),
                                 "Source",
                                 formatC(after$covariateCount, big.mark = ",", format = "d"),
                                 "Covariate\ncount",
                                 formatC(after$max,  digits = 2, format = "f"),
                                 paste(afterLabel, "max(absolute)", sep = "\n"))),
                     dummy = "")
  
  data_table <- ggplot2::ggplot(text, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label)) +
    ggplot2::geom_text(size = 4, hjust=0, vjust=0.5) +
    ggplot2::geom_hline(ggplot2::aes(yintercept=nrow(after) + 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(colour="white"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(colour="white"),
                   strip.background = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines")) +
    ggplot2::labs(x="",y="") +
    ggplot2::facet_grid(~dummy) +
    ggplot2::coord_cartesian(xlim=c(1,4), ylim = c(0.5, max(vizData$x) + 1.75))
  
  plot <- gridExtra::grid.arrange(data_table, plot, ncol = 2)
  return(plot)
}

# estimation-systematicError
plotEstimationScatter <- function(controlResults) {
  
  if(nrow(controlResults)==0){
    return(NULL)
  }
  
  size <- 2
  labelY <- 0.7
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlResults$logRr,
                        seLogRr = controlResults$seLogRr,
                        ci95Lb = controlResults$ci95Lb,
                        ci95Ub = controlResults$ci95Ub,
                        trueRr = controlResults$effectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlResults$calibratedLogRr,
                        seLogRr = controlResults$calibratedSeLogRr,
                        ci95Lb = controlResults$calibratedCi95Lb,
                        ci95Ub = controlResults$calibratedCi95Ub,
                        trueRr = controlResults$effectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
  temp1 <- stats::aggregate(Significant ~ Group + yGroup, data = d, length)
  temp2 <- stats::aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  
  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs include ",
                            temp2$Group)
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(as.character(dd$Group))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  
  d$Group <- paste("True hazard ratio =", d$Group)
  dd$Group <- paste("True hazard ratio =", dd$Group)
  alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$logRr, y = .data$seLogRr), environment = environment()) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$tes))/stats::qnorm(0.025), slope = 1/stats::qnorm(0.025)),
                         colour = grDevices::rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$tes))/stats::qnorm(0.975), slope = 1/stats::qnorm(0.975)),
                         colour = grDevices::rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_point(size = size,
                        color = grDevices::rgb(0, 0, 0, alpha = 0.05),
                        alpha = alpha,
                        shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.15),
                        y = 0.9,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = .data$nLabel),
                        size = 5,
                        data = dd) +
    ggplot2::geom_label(x = log(0.15),
                        y = labelY,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = .data$meanLabel),
                        size = 5,
                        data = dd) +
    ggplot2::scale_x_continuous("Hazard ratio",
                                limits = log(c(0.1, 10)),
                                breaks = log(breaks),
                                labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::facet_grid(yGroup ~ Group) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  
  return(plot)
}

# estimation-attrition
drawEstimationAttritionDiagram <- function(attrition,
                                           targetLabel = "Target",
                                           comparatorLabel = "Comparator") {
  addStep <- function(data, attrition, row) {
    label <- paste(strwrap(as.character(attrition$description[row]), width = 30), collapse = "\n")
    data$leftBoxText[length(data$leftBoxText) + 1] <- label
    data$rightBoxText[length(data$rightBoxText) + 1] <- paste(targetLabel,
                                                              ": n = ",
                                                              data$currentTarget - attrition$targetPersons[row],
                                                              "\n",
                                                              comparatorLabel,
                                                              ": n = ",
                                                              data$currentComparator - attrition$comparatorPersons[row],
                                                              sep = "")
    data$currentTarget <- attrition$targetPersons[row]
    data$currentComparator <- attrition$comparatorPersons[row]
    return(data)
  }
  data <- list(leftBoxText = c(paste("Exposed:\n",
                                     targetLabel,
                                     ": n = ",
                                     attrition$targetPersons[1],
                                     "\n",
                                     comparatorLabel,
                                     ": n = ",
                                     attrition$comparatorPersons[1],
                                     sep = "")), rightBoxText = c(""), currentTarget = attrition$targetPersons[1], currentComparator = attrition$comparatorPersons[1])
  for (i in 2:nrow(attrition)) {
    data <- addStep(data, attrition, i)
  }
  
  
  data$leftBoxText[length(data$leftBoxText) + 1] <- paste("Study population:\n",
                                                          targetLabel,
                                                          ": n = ",
                                                          data$currentTarget,
                                                          "\n",
                                                          comparatorLabel,
                                                          ": n = ",
                                                          data$currentComparator,
                                                          sep = "")
  leftBoxText <- data$leftBoxText
  rightBoxText <- data$rightBoxText
  nSteps <- length(leftBoxText)
  
  boxHeight <- (1/nSteps) - 0.03
  boxWidth <- 0.45
  shadowOffset <- 0.01
  arrowLength <- 0.01
  x <- function(x) {
    return(0.25 + ((x - 1)/2))
  }
  y <- function(y) {
    return(1 - (y - 0.5) * (1/nSteps))
  }
  
  downArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 + arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    return(p)
  }
  rightArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 - arrowLength))
    return(p)
  }
  box <- function(p, x, y) {
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2) + shadowOffset,
                                                    ymin = y - (boxHeight/2) - shadowOffset,
                                                    xmax = x + (boxWidth/2) + shadowOffset,
                                                    ymax = y + (boxHeight/2) - shadowOffset), fill = grDevices::rgb(0,
                                                                                                         0,
                                                                                                         0,
                                                                                                         alpha = 0.2))
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2),
                                                    ymin = y - (boxHeight/2),
                                                    xmax = x + (boxWidth/2),
                                                    ymax = y + (boxHeight/2)), fill = grDevices::rgb(0.94,
                                                                                          0.94,
                                                                                          0.94), color = "black")
    return(p)
  }
  label <- function(p, x, y, text, hjust = 0) {
    p <- p + ggplot2::geom_text(ggplot2::aes_string(x = x, y = y, label = paste("\"", text, "\"",
                                                                                sep = "")),
                                hjust = hjust,
                                size = 3.7)
    return(p)
  }
  
  p <- ggplot2::ggplot()
  for (i in 2:nSteps - 1) {
    p <- downArrow(p, x(1), y(i) - (boxHeight/2), x(1), y(i + 1) + (boxHeight/2))
    p <- label(p, x(1) + 0.02, y(i + 0.5), "Y")
  }
  for (i in 2:(nSteps - 1)) {
    p <- rightArrow(p, x(1) + boxWidth/2, y(i), x(2) - boxWidth/2, y(i))
    p <- label(p, x(1.5), y(i) - 0.02, "N", 0.5)
  }
  for (i in 1:nSteps) {
    p <- box(p, x(1), y(i))
  }
  for (i in 2:(nSteps - 1)) {
    p <- box(p, x(2), y(i))
  }
  for (i in 1:nSteps) {
    p <- label(p, x(1) - boxWidth/2 + 0.02, y(i), text = leftBoxText[i])
  }
  for (i in 2:(nSteps - 1)) {
    p <- label(p, x(2) - boxWidth/2 + 0.02, y(i), text = rightBoxText[i])
  }
  p <- p + ggplot2::theme(legend.position = "none",
                          plot.background = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.title = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_blank())
  
  return(p)
}

# used in helpers-estPandT
nonZeroEstimationHazardRatio <- function(hrLower, hrUpper, terms) {
  if (hrUpper < 1) {
    return(terms[1])
  } else if (hrLower > 1) {
    return(terms[2])
  } else {
    return(terms[3])
  }
}

# estimation-resultsTable
prettyEstimationHr <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  result <- sprintf("%.2f", x)
  result[is.na(x) | x > 100] <- "NA"
  return(result)
}

# used in here
goodEstimationPropensityScore <- function(value) {
  return(value > 1)
}

# used in here
goodEstimationSystematicBias <- function(value) {
  return(value > 1)
}


# estmation-propensity 
prepareEstimationPropensityModelTable <- function(model) {
  rnd <- function(x) {
    ifelse(x > 10, sprintf("%.1f", x), sprintf("%.2f", x))
  }
  table <- model[order(-abs(model$coefficient)), c("coefficient", "covariateName")]
  table$coefficient <- sprintf("%.2f", table$coefficient)
  colnames(table) <- c("Beta", "Covariate")
  return(table)
}

# estimation-forestPlot
plotEstimationForest <- function(results, limits = c(0.1, 10), metaAnalysisDbIds = NULL) {
  
  dbResults <- results[!(results$databaseId %in% metaAnalysisDbIds), ]
  dbResults <- dbResults[!is.na(dbResults$seLogRr), ]
  dbResults <- dbResults[order(dbResults$databaseId), ]
  maResult <- results[results$databaseId %in% metaAnalysisDbIds, ]
  summaryLabel <- sprintf("Summary (I\u00B2 = %.2f)", as.numeric(maResult$i2))
  d1 <- data.frame(x = "Uncalibrated",
                   logRr = -100,
                   logLb95Ci = -100,
                   logUb95Ci = -100,
                   name = "Source",
                   type = "header",
                   stringsAsFactors = FALSE)
  d2 <- data.frame(x = "Uncalibrated",
                   logRr = dbResults$logRr,
                   logLb95Ci = log(dbResults$ci95Lb),
                   logUb95Ci = log(dbResults$ci95Ub),
                   name = dbResults$databaseId,
                   type = "db",
                   stringsAsFactors = FALSE)
  d3 <- data.frame(x = "Uncalibrated",
                   logRr = maResult$logRr,
                   logLb95Ci = log(maResult$ci95Lb),
                   logUb95Ci = log(maResult$ci95Ub),
                   name = summaryLabel,
                   type = "ma",
                   stringsAsFactors = FALSE)
  d4 <- data.frame(x = "Calibrated",
                   logRr = -100,
                   logLb95Ci = -100,
                   logUb95Ci = -100,
                   name = "Source",
                   type = "header",
                   stringsAsFactors = FALSE)
  d5 <- data.frame(x = "Calibrated",
                   logRr = dbResults$calibratedLogRr,
                   logLb95Ci = log(dbResults$calibratedCi95Lb),
                   logUb95Ci = log(dbResults$calibratedCi95Ub),
                   name = dbResults$databaseId,
                   type = "db",
                   stringsAsFactors = FALSE)
  d6 <- data.frame(x = "Calibrated",
                   logRr = maResult$calibratedLogRr,
                   logLb95Ci = log(maResult$calibratedCi95Lb),
                   logUb95Ci = log(maResult$calibratedCi95Ub),
                   name = summaryLabel,
                   type = "ma",
                   stringsAsFactors = FALSE)
  
  d <- rbind(d1, d2, d3, d4, d5, d6)
  d$name <- factor(d$name, levels = c(summaryLabel, rev(as.character(dbResults$databaseId)), "Source"))
  d$x <- factor(d$x, levels = c("Uncalibrated", "Calibrated"))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  plot <- ggplot2::ggplot(d,ggplot2::aes(x = exp(.data$logRr), y = .data$name, xmin = exp(.data$logLb95Ci), xmax = exp(.data$logUb95Ci))) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 1, size = 0.5) +
    ggplot2::geom_errorbarh(height = 0.15) +
    ggplot2::geom_point(size=3, shape = 23, ggplot2::aes(fill=.data$type)) +
    ggplot2::scale_fill_manual(values = c("#000000", "#000000", "#FFFFFF")) +
    ggplot2::scale_x_continuous("Hazard ratio", trans = "log10", breaks = breaks, labels = breaks) +
    ggplot2::coord_cartesian(xlim = limits) +
    ggplot2::facet_grid(~ x) +
    ggplot2::theme(text = ggplot2::element_text(size = 18),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  d$hr <- paste0(formatC(exp(d$logRr),  digits = 2, format = "f"),
                 " (",
                 formatC(exp(d$logLb95Ci), digits = 2, format = "f"),
                 "-",
                 formatC(exp(d$logUb95Ci), digits = 2, format = "f"),
                 ")")
  d <- d[order(d$x), ]
  
  labels <- data.frame(y = factor(c(as.character(d$name[d$x == "Uncalibrated"]), as.character(d$name)), levels = levels(d$name)),
                       x = rep(1:3, each = nrow(d)/2),
                       label = c(as.character(d$name[d$x == "Uncalibrated"]), d$hr),
                       dummy = "dummy",
                       stringsAsFactors = FALSE)
  labels$label[nrow(d)/2 + 1] <-  paste("HR (95% CI)")
  labels$label[nrow(d) + 1] <-  paste("Calibrated HR (95% CI)")
  dataTable <- ggplot2::ggplot(labels, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label)) +
    ggplot2::geom_text(size = 5, hjust = 0, vjust = 0.5) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::facet_grid(~dummy) +
    ggplot2::theme(text = ggplot2::element_text(size = 18),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(colour = "white"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(colour = "white"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(colour = "white"),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines")) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_cartesian(xlim = c(1,4))
  plot <- gridExtra::grid.arrange(dataTable, plot, ncol = 2)
  return(plot)
}
