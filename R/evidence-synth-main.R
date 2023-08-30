#' The location of the evidence synthesis module helper file
#'
#' @details
#' Returns the location of the evidence synthesis helper file
#' 
#' @return
#' string location of the evidence synthesis helper file
#'
#' @export
evidenceSynthesisHelperFile <- function(){
  fileLoc <- system.file('evidence-synthesis-www', "evidence-synthesis.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring evidence-synthesis
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the evidence-synthesis viewer module
#'
#' @export
evidenceSynthesisViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', 
    width = 12,
    title = shiny::span( shiny::icon("sliders"), 'Evidence Synthesis'),
    solidHeader = TRUE,
    
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("evidence-synthesis-www", "evidence-synthesis.html", package = utils::packageName())
    ),
    
    inputSelectionViewer(ns("input-selection")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('esCohortTabs'),
        
        # diagnostic view
        shiny::tabPanel(
          title = 'Diagnostics',
          resultTableViewer(ns("diagnosticsSummaryTable"))
        ),
        
        shiny::tabPanel(
          "Cohort Method Plot",
          shiny::plotOutput(ns('esCohortMethodPlot'))
        ),
        shiny::tabPanel(
          "Cohort Method Table",
          resultTableViewer(ns("esCohortMethodTable"))
        ),
        shiny::tabPanel("SCCS Plot",
                        shiny::plotOutput(ns('esSccsPlot'))
        ),
        shiny::tabPanel("SCCS Table",
                        resultTableViewer(ns("esSccsTable"))
        )
      )
    )
    
  )
  
}


#' The module server for exploring PatientLevelPrediction
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' 
#' @return
#' The server for the PatientLevelPrediction module
#'
#' @export
evidenceSynthesisServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      targetIds <- getESTargetIds(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      outcomeIds <- getESOutcomeIds(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
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
        getEvidenceSynthDiagnostics(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          inputSelected = inputSelected,
          targetIds = inputSelected()$targetIds,
          outcomeIds = inputSelected()$outcomeIds
        )
      })
      
      
      resultTableServer(
        id = "diagnosticsSummaryTable",
        df = diagSumData,
        colDefsInput = getColDefsESDiag(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
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
      
      
      # SCCS plots and tables
      
      sccsData <- shiny::reactive({
        unique(
          getSccsEstimation(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetId = inputSelected()$targetIds,
          outcomeId = inputSelected()$outcomeIds
        )
        )
      })
      
      output$esSccsPlot <- shiny::renderPlot(
        createPlotForSccsAnalysis(
          sccsData()
        )
      )
      
      
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
            ))
        )
      )
    
    }
  )
  
}


getESTargetIds <- function(
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

getESOutcomeIds <- function(
    connectionHandler,
    resultDatabaseSettings
) {
  sql <- "select distinct
  c1.cohort_name as outcome,
  r.outcome_id
  
  from 
   @schema.@cm_table_prefixresult as r
   inner join 
   @schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   @schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.outcome_id
   
  where 
  tco.outcome_of_interest = 1
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  ) 
  
  output <- as.list(result$outcomeId)
  names(output) <- result$outcome
  
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
    ggplot2::coord_cartesian(xlim = c(0.1, 10)) + 
    ggplot2::facet_grid(.data$database ~ .data$description)  +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_text(angle = 0)
    ) + 
    ggplot2::labs(
      caption = paste(
        apply(
          X = compText, 
          MARGIN = 1, 
          FUN = function(x){paste(x,collapse = ': ', sep=':')}
        ), 
        collapse = '; ')
    )
  
  return(plot)
}


computeTraditionalP <- function(
    logRr, 
    seLogRr, 
    twoSided = TRUE, 
    upper = TRUE
) 
{
  z <- logRr/seLogRr
  
  pUpperBound <- 1 - stats::pnorm(z)
  pLowerBound <- stats::pnorm(z)
  
  if (twoSided) {
    return(2 * pmin(pUpperBound, pLowerBound))
  }
  else if (upper) {
    return(pUpperBound)
  }
  else {
    return(pLowerBound)
  }
}




getSccsEstimation <- function(
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
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    database_table = resultDatabaseSettings$databaseTable,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
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
  ;"
  
  result2 <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    es_table_prefix = resultDatabaseSettings$esTablePrefix,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
  )
  
  return(rbind(result,result2))
  
}


createPlotForSccsAnalysis <- function(
    data
){
  
  if(is.null(data)){
    return(NULL)
  }
  
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
    ggplot2::coord_cartesian(xlim = c(0.1, 10)) + 
    ggplot2::facet_grid(.data$database ~ .data$description)  +
    ggplot2::ggtitle(data$outcome[1]) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.y.right = ggplot2::element_text(angle = 0)
    )
  return(plot)
}


getOACcombinations <- function(
    connectionHandler, 
    resultDatabaseSettings
){
  
  sql <- "SELECT DISTINCT
      CONCAT(cma.description, '_', cgcd2.cohort_name) as col_names
    FROM
      @schema.@cm_table_prefixdiagnostics_summary cmds
      INNER JOIN @schema.@cm_table_prefixanalysis cma 
      ON cmds.analysis_id = cma.analysis_id
      INNER JOIN @schema.@cg_table_prefixcohort_definition cgcd2 
      ON cmds.comparator_id = cgcd2.cohort_definition_id
      
      union
      
      SELECT 
  CONCAT(a.description, '_', cov.covariate_name) as col_names

  FROM @schema.@sccs_table_prefixdiagnostics_summary ds

  INNER JOIN
  @schema.@sccs_table_prefixanalysis a
  on a.analysis_id = ds.analysis_id
  
  INNER JOIN
  @schema.@sccs_table_prefixcovariate cov
  on cov.covariate_id = ds.covariate_id and 
  cov.exposures_outcome_set_id = ds.exposures_outcome_set_id and
  cov.analysis_id = ds.analysis_id
      ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    database_table = resultDatabaseSettings$databaseTable
  )
  
  res <- result$colNames
  names(res) <- result$colNames
  
  return(res)
}

getEvidenceSynthDiagnostics <- function(
    connectionHandler, 
    resultDatabaseSettings,
    inputSelected,
    targetIds,
    outcomeIds
){
  
  if(is.null(targetIds)){
    return(NULL)
  }
  
  sccsDiagTemp <- getSccsAllDiagnosticsSummary(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetIds = targetIds,
    outcomeIds = outcomeIds
  ) 
  
  cmDiagTemp <- getCmDiagnosticsData(
    connectionHandler = connectionHandler, 
    resultDatabaseSettings = resultDatabaseSettings, 
    inputSelected = inputSelected
  )
  
  if(is.null(cmDiagTemp) | is.null(sccsDiagTemp)){
    return(NULL)
  }
  
  # select columns of interest and rename for consistency
  sccsDiagTemp <- diagnosticSummaryFormat(
    data = shiny::reactive({sccsDiagTemp}),
    idCols = c('databaseName','target'),
    namesFrom = c('analysis','covariateName','outcome')
  )
  
  cmDiagTemp <- diagnosticSummaryFormat(
    data = shiny::reactive({cmDiagTemp}),
    idCols = c('databaseName','target'),
    namesFrom = c('analysis','comparator','outcome')
  )
  
  allResult <- merge(
    x = sccsDiagTemp, 
    y = cmDiagTemp, 
    by = c('databaseName','target'),
    all = T
  )
  
  # return
  return(allResult)
}



getColDefsESDiag <- function(
    connectionHandler,
    resultDatabaseSettings
){      
  
  fixedColumns =  list(
    databaseName = reactable::colDef(
      header = withTooltip(
        "Database",
        "The database name"
      ), 
      sticky = "left"
    ),
    target = reactable::colDef(
      header = withTooltip(
        "Target",
        "The target cohort of interest "
      ), 
      sticky = "left"
    )
  )
  
  outcomes <- getESOutcomeIds(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings
  )
  
  analyses <- getOACcombinations(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings
  )
  colnameFormat <- merge(unique(names(analyses)), unique(names(outcomes)))
  colnameFormat <- apply(colnameFormat, 1, function(x){paste(x, collapse = '_', sep = '_')})
  
  styleList <- lapply(
    colnameFormat, 
    FUN = function(x){
      reactable::colDef(
        header = withTooltip(
          substring(x,1,40),
          x
        ),
        style = function(value) {
          color <- 'orange'
          if(is.na(value)){
            color <- 'black'
          }else if(value == 'Pass'){
            color <- '#AFE1AF'
          }else if(value == 'Fail'){
            color <- '#E97451'
          }
          list(background = color)
        }
      )
    }
  )
  names(styleList) <- colnameFormat
  result <- append(fixedColumns, styleList)
  
  return(result)
}