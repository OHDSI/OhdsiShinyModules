evidenceSynthesisCmViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    inputSelectionViewer(ns("input-selection-cm")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection-cm")),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('esCohortMethodTabs'),
        
        # diagnostic view
        shiny::tabPanel(
          title = 'Diagnostics',
          resultTableViewer(ns("diagnosticsCmSummaryTable"))
        ),
        
        shiny::tabPanel(
          "Plot",
          shiny::plotOutput(ns('esCohortMethodPlot'))
        ),
        shiny::tabPanel(
          "Table",
          resultTableViewer(ns("esCohortMethodTable"))
        )
      )
    )
  )
}


evidenceSynthesisCmServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      targetIds <- getEsCmTargetIds(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      outcomeIds <- getEsOutcomeIds(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      inputSelected <- inputSelectionServer(
        id = "input-selection-cm", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = targetIds,
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = outcomeIds,
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
      
      # plots and tables
      cmdata <- shiny::reactive({
        unique(
          rbind(
            getCMEstimation(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = inputSelected()$targetIds,
              outcomeId = inputSelected()$outcomeIds
            ),
            getMetaEstimation(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = inputSelected()$targetIds,
              outcomeId = inputSelected()$outcomeIds
            )
          )
        )
      })
      
      
      diagSumData <- shiny::reactive({
        getEvidenceSynthCmDiagnostics(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          inputSelected = inputSelected,
          targetIds = inputSelected()$targetIds,
          outcomeIds = inputSelected()$outcomeIds
        )
      })
      
      
      resultTableServer(
        id = "diagnosticsCmSummaryTable",
        df = diagSumData,
        colDefsInput = getColDefsESDiag(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          method = 'cm'
        )
      )
      
      output$esCohortMethodPlot <- shiny::renderPlot(
        createPlotForAnalysis(
          cmdata()
        )
      )
      
      
      resultTableServer(
        id = "esCohortMethodTable",
        df = cmdata,
        colDefsInput = list(
          targetId = reactable::colDef(show = F),
          outcomeId = reactable::colDef(show = F),
          comparatorId = reactable::colDef(show = F),
          analysisId = reactable::colDef(show = F),
          description = reactable::colDef( 
            filterable = TRUE,
            header = withTooltip(
              "Analysis", 
              "Analysis"
            )),
          database = reactable::colDef( 
            filterable = TRUE,
            header = withTooltip(
              "Data source", 
              "Data source"
            )),
          calibratedRr = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.HR", 
              "Hazard ratio (calibrated)"
            )),
          calibratedCi95Lb = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.LB", 
              "Lower bound of the 95 percent confidence interval (calibrated)"
            )),
          calibratedCi95Ub = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.UB", 
              "Upper bound of the 95 percent confidence interval (calibrated)"
            )),
          calibratedP = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.P", 
              "Two-sided p-value (calibrated)"
            )),
          calibratedLogRr = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.Log.HR", 
              "Log of Hazard ratio (calibrated)"
            )),
          calibratedSeLogRr = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.Se.Log.HR", 
              "Log Standard Error of Hazard ratio (calibrated)"
            )),
          target = reactable::colDef( 
            minWidth = 300
          ),
          outcome = reactable::colDef( 
            minWidth = 300
          ),
          comparator = reactable::colDef( 
            minWidth = 300
          )
        )
      )
      
    }
  )
  
}


getEsCmTargetIds <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  sql <- "select distinct
  c1.cohort_name as target,
  r.target_id
  
  from 
   @schema.@cm_table_prefixresult as r
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  ) 
  
  output <- as.list(result$targetId)
  names(output) <- result$target
  
  return(output)
  
}

getCMEstimation <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId,
    outcomeId
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  sql <- "select 
  c1.cohort_name as target,
  c2.cohort_name as comparator,
  c3.cohort_name as outcome,
  r.target_id, r.comparator_id, r.outcome_id, r.analysis_id, 
  a.description,
  db.cdm_source_abbreviation as database, r.calibrated_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_p,
  r.calibrated_log_rr, r.calibrated_se_log_rr
  
  from 
   @schema.@cm_table_prefixresult as r
   inner join 
   @schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @schema.@cm_table_prefixdiagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id and 
   r.database_id = unblind.database_id
   
   inner join
   @schema.@database_table as db
   on db.database_id = r.database_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id = @target_id and
   r.outcome_id = @outcome_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    database_table = resultDatabaseSettings$databaseTable,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
  ) %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )
  
  return(result)
}

getMetaEstimation <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId,
    outcomeId
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  sql <- "select 
  c1.cohort_name as target,
  c2.cohort_name as comparator,
  c3.cohort_name as outcome,
  r.target_id, r.comparator_id, r.outcome_id, r.analysis_id, 
  a.description,
  ev.evidence_synthesis_description as database,
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_p,
  r.calibrated_log_rr, r.calibrated_se_log_rr
  
  from 
   @schema.@es_table_prefixcm_result as r
   inner join 
   @schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @schema.@es_table_prefixcm_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id 
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = r.evidence_synthesis_analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id = @target_id and
   r.outcome_id = @outcome_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    es_table_prefix = resultDatabaseSettings$esTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
  ) %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )
  
  return(unique(result))
}

getEvidenceSynthCmDiagnostics <- function(
    connectionHandler, 
    resultDatabaseSettings,
    inputSelected,
    targetIds,
    outcomeIds
){
  
  if(is.null(targetIds)){
    return(NULL)
  }
  
  cmDiagTemp <- getCmDiagnosticsData(
    connectionHandler = connectionHandler, 
    resultDatabaseSettings = resultDatabaseSettings, 
    inputSelected = inputSelected
  )
  
  if(is.null(cmDiagTemp)){
    return(NULL)
  }
  
  # select columns of interest and rename for consistency
  cmDiagTemp <- diagnosticSummaryFormat(
    data = shiny::reactive({cmDiagTemp}),
    idCols = c('databaseName','target'),
    namesFrom = c('analysis','comparator','outcome')
  )
  
  # return
  return(cmDiagTemp)
}


createPlotForAnalysis <- function(data) {
  
  if(is.null(data$comparator)){
    return(NULL)
  }
  
  compText <- data.frame(
    comparatorText = paste0('Comp', 1:length(unique(data$comparator))),
    comparator = unique(data$comparator)
  )
  
  data <- merge(
    data, 
    compText,
    by = "comparator"
  )
  
  # make sure bayesian is at bottom
  db <- unique(data$database)
  bInd <- grep('bayesian', tolower(db))
  withoutb <- db[-bInd]
  b <- db[bInd]
  data$database <- factor(
    x = data$database, 
    levels = c(sort(withoutb), b)
      )
  metadata <- data[data$database == b,]
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8)
  title <- sprintf("%s", data$outcome[1])
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
