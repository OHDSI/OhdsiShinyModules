evidenceSynthesisSccsViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    inputSelectionViewer(ns("input-selection-sccs")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection-sccs")),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('esSccsTabs'),
        
        # diagnostic view
        shiny::tabPanel(
          title = 'Diagnostics',
          resultTableViewer(ns("diagnosticsSccsSummaryTable"))
        ),
        
        shiny::tabPanel(
          "Plot",
          shiny::plotOutput(ns('esSccsPlot'))
        ),
        shiny::tabPanel(
          "Table",
          resultTableViewer(ns("esSccsTable"))
        )
      )
    )
  )
}

evidenceSynthesisSccsServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      outcomeIds <- getEsOutcomeIds(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      inputSelected <- inputSelectionServer(
        id = "input-selection-sccs", 
        inputSettingList = list(
          .getSccsExposureIndicationSelection(connectionHandler = connectionHandler,
                                              resultDatabaseSettings = resultDatabaseSettings),
          createInputSetting(
            rowNumber = 2,
            columnWidth = 12,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::virtualSelectInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = outcomeIds,
              multiple = F,
              search = TRUE
            )
          )
        )
      )
      
      
      
      diagSumData <- shiny::reactive({
        getEvidenceSynthSccsDiagnostics(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          inputSelected = inputSelected,
          exposure =  inputSelected()$exposure,
          outcomeIds = inputSelected()$outcomeIds
        )
      })
      
      # SCCS plots and tables
      
      resultTableServer(
        id = "diagnosticsSccsSummaryTable",
        df = diagSumData,
        colDefsInput = getColDefsESDiag(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      )
      
      sccsData <- shiny::reactive({
        unique(
          getSccsEstimation(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            exposure = inputSelected()$exposure,
            outcomeId = inputSelected()$outcomeIds
          )
        )
      })
      
      output$esSccsPlot <- shiny::renderPlot({
        sccsRes <- sccsData()
        shiny::validate(shiny::need(hasData(sccsRes), "No valid data for selected target"))
        createPlotForSccsAnalysis(sccsRes)
      })
      
      
      resultTableServer(
        id = "esSccsTable",
        df = sccsData,
        colDefsInput = list(
          targetId = reactable::colDef(show = F),
          outcomeId = reactable::colDef(show = F),
          analysisId = reactable::colDef(show = F),
          description = reactable::colDef( 
            filterable = TRUE,
            header = withTooltip(
              "Analysis", 
              "Analysis"
            ),
            minWidth = 300
            ),
          database = reactable::colDef( 
            filterable = TRUE,
            header = withTooltip(
              "Data source", 
              "Data source"
            )),
          calibratedRr = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.IRR", 
              "Incidence rate ratio (calibrated)"
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
              "Cal.Log.IRR", 
              "Log of Incidence rate ratio (calibrated)"
            )),
          calibratedSeLogRr = reactable::colDef( 
            format = reactable::colFormat(digits = 3),
            header = withTooltip(
              "Cal.Se.Log.IRR", 
              "Log Standard Error of Incidence rate ratio (calibrated)"
            )),
          target = reactable::colDef( 
            minWidth = 300
          ),
          outcome = reactable::colDef( 
            minWidth = 300
          )
        )
      )
      
    }
  )
  
}

getSccsTargets <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  output <- sccsGetExposureIndications(
    connectionHandler,
    resultDatabaseSettings
  )
  return(output)
  
}

getSccsEstimation <- function(
  connectionHandler,
  resultDatabaseSettings,
  exposure,
  outcomeId
){
  if (is.character(exposure)) {
    exposureGroup <- strsplit(exposure, " ")[[1]]
    targetId <- exposureGroup[[1]]
    indicationIds <- exposureGroup[[2]]
  } else {
    targetId <- -1
    indicationIds <- -1
  }

  if (any(indicationIds == -1)) {
    indicationIds <- NULL
  }
  
  sql <- "select 
  c1.cohort_name as target,
  c3.cohort_name as outcome,
  cov.era_id as target_id, eos.outcome_id, r.analysis_id, 
  a.description,
  cov.covariate_name as type,
  db.cdm_source_abbreviation as database, 
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, 
  r.calibrated_ci_95_ub,  
  r.calibrated_p,
  r.calibrated_log_rr, 
  r.calibrated_se_log_rr
  
  from 
   @schema.@sccs_table_prefixresult as r
   inner join 
   @schema.@sccs_table_prefixexposures_outcome_set as eos
   on 
   r.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   inner join
   @schema.@sccs_table_prefixcovariate as cov
   on 
   r.covariate_id = cov.covariate_id and
   r.database_id = cov.database_id and
   r.analysis_id = cov.analysis_id and
   r.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   @schema.@sccs_table_prefixexposure as ex
   on 
   ex.era_id = cov.era_id and
   ex.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   
   @schema.@sccs_table_prefixdiagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.exposures_outcome_set_id = unblind.exposures_outcome_set_id and 
   r.covariate_id = unblind.covariate_id and
   r.database_id = unblind.database_id
   
   inner join
   @schema.@database_table as db
   on db.database_id = r.database_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = cov.era_id

   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = eos.outcome_id
   
   inner join
   @schema.@sccs_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   where 
   r.calibrated_rr != 0 and
   --ex.true_effect_size != 1 and
   cov.covariate_name in ('Main', 'Second dose') and
   unblind.unblind = 1 and
   cov.era_id = @target_id and
   eos.outcome_id = @outcome_id
   {@use_indications} ? {and eos.nesting_cohort_id IN (@indication_ids)} : {and eos.nesting_cohort_id IS NULL}
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    database_table = resultDatabaseSettings$databaseTable,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId,
    indication_ids = indicationIds,
    use_indications = !is.null(indicationIds)
  )
  
  sql <- "select distinct
  c1.cohort_name as target,
  c3.cohort_name as outcome,
  cov.era_id as target_id, eos.outcome_id, r.analysis_id, 
  a.description,
  cov.covariate_name as type,
  ev.evidence_synthesis_description as database, 
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, 
  r.calibrated_ci_95_ub,  
  r.calibrated_p,
  r.calibrated_log_rr, 
  r.calibrated_se_log_rr
  
  from 
   @schema.@es_table_prefixsccs_result as r
   inner join 
   @schema.@sccs_table_prefixexposures_outcome_set as eos
   on 
   r.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   inner join
   @schema.@sccs_table_prefixcovariate as cov
   on 
   r.covariate_id = cov.covariate_id and
   r.analysis_id = cov.analysis_id and
   r.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   @schema.@sccs_table_prefixexposure as ex
   on 
   ex.era_id = cov.era_id and
   ex.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   
   @schema.@es_table_prefixsccs_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.exposures_outcome_set_id = unblind.exposures_outcome_set_id and 
   r.covariate_id = unblind.covariate_id and
   r.evidence_synthesis_analysis_id = unblind.evidence_synthesis_analysis_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = cov.era_id

   inner join
   @schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = eos.outcome_id
   
   inner join
   @schema.@sccs_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = r.evidence_synthesis_analysis_id
   
   where 
   r.calibrated_rr != 0 and
   --ex.true_effect_size != 1 and
   cov.covariate_name in ('Main', 'Second dose') and
   unblind.unblind = 1 and
   cov.era_id = @target_id and
   eos.outcome_id = @outcome_id
   {@use_indications} ? {and eos.nesting_cohort_id IN (@indication_ids)} : {and eos.nesting_cohort_id IS NULL}
  ;"
  
  result2 <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    es_table_prefix = resultDatabaseSettings$esTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId,
    indication_ids = indicationIds,
    use_indications = !is.null(indicationIds)
  )
  
  return(rbind(result,result2))
  
}

createPlotForSccsAnalysis <- function(
    data
){
  
  if(is.null(data)){
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
  plot <- ggplot2::ggplot(
    data = data, 
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
      data =  metadata,
      ggplot2::aes(fill = .data$database),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.2
    ) +
    
    ggplot2::coord_cartesian(xlim = c(0.1, 10)) + 
    ggplot2::facet_grid(.data$database ~ .data$shortName)  +
    ggplot2::ggtitle(data$outcome[1]) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_text(angle = 0),
      legend.position = "none"
    )
  
  ### Add table below the graph
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
  plot <- gridExtra::grid.arrange(
    plot, 
    tbl,
    nrow = 2,
    as.table = TRUE
  )
  
  return(plot)
}

getEvidenceSynthSccsDiagnostics <- function(
  connectionHandler,
  resultDatabaseSettings,
  inputSelected,
  exposure,
  outcomeIds
){
  
  if(is.null(exposure)){
    return(NULL)
  }

  if (is.character(exposure)) {
    exposureGroup <- strsplit(exposure, " ")[[1]]
    targetId <- exposureGroup[[1]]
    indicationIds <- exposureGroup[[2]]
  } else {
    targetId <- -1
    indicationIds <- -1
  }

  if (any(indicationIds == -1)) {
    indicationIds <- NULL
  }
  
  sccsDiagTemp <- getSccsAllDiagnosticsSummary(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetIds = targetId,
    indicationIds = indicationIds,
    outcomeIds = outcomeIds
  ) 
  
  if(is.null(sccsDiagTemp)){
    return(NULL)
  }
  
  # select columns of interest and rename for consistency
  sccsDiagTemp <- diagnosticSummaryFormat(
    data = shiny::reactive({sccsDiagTemp}),
    idCols = c('databaseName','target'),
    namesFrom = c('analysis','covariateName','outcome')
  )
  
  # return
  return(sccsDiagTemp)
}
