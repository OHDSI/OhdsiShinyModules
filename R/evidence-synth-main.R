
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
      
      shiny::uiOutput(ns('esCohortMethodSelect')),

    
    #shiny::fluidRow(
      #shinydashboard::tabBox(
        shiny::tabsetPanel(
          type = 'pills',
        #width = 12,
        #title = shiny::tagList(shiny::icon("gear"), "Plot and Table"),
        id = ns('esCohortTabs'),
        
        shiny::tabPanel(
          "Cohort Method Plot",
          shiny::plotOutput(ns('esCohortMethodPlot'))
        ),
        shiny::tabPanel(
          "Cohort Method Table",
          reactable::reactableOutput(ns('esCohortMethodTable')),
          shiny::downloadButton(
            ns('downloadCohortMethodTable'), 
            label = "Download"
          )
        ),
        shiny::tabPanel("SCCS Plot",
                        shiny::plotOutput(ns('esSccsPlot'))
        ),
        shiny::tabPanel("SCCS Table",
                        reactable::reactableOutput(ns('esSccsTable'))
        ),
        shiny::tabPanel("Diagnostics Dashboard",
                        reactable::reactableOutput(ns('diagnosticsTable'))
        )
      )
    #)
    
  )
  
}


#' The module server for exploring PatientLevelPrediction
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
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
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      
      targetIds <- getESTargetIds(
        connectionHandler = connectionHandler,
        mySchema = resultDatabaseSettings$schema, 
        cmTablePrefix = resultDatabaseSettings$cmTablePrefix,
        cgTablePrefix = resultDatabaseSettings$cgTablePrefix
      )
      outcomeIds <- getESOutcomeIds(
        connectionHandler = connectionHandler,
        mySchema = resultDatabaseSettings$schema, 
        cmTablePrefix = resultDatabaseSettings$cmTablePrefix,
        cgTablePrefix = resultDatabaseSettings$cgTablePrefix
      )
      
      targetId <- shiny::reactiveVal(targetIds[1])
      outcomeId <- shiny::reactiveVal(outcomeIds[1])
      
      output$esCohortMethodSelect <- shiny::renderUI({
        
        shiny::tagList(
          shiny::selectInput(
            inputId = session$ns('selectedTargetId'), 
            label = 'Target:', 
            choices = targetIds, 
            selected = 1,
            multiple = F, 
            selectize=FALSE
          ),
          shiny::selectInput(
            inputId = session$ns('selectedOutcomeId'), 
            label = 'Outcome:', 
            choices = outcomeIds, 
            selected = 1,
            multiple = F, 
            selectize=FALSE
          )
        )
      })
      
      shiny::observeEvent(input$selectedTargetId,{
        targetId(input$selectedTargetId)
      })
      shiny::observeEvent(input$selectedOutcomeId,{
        outcomeId(input$selectedOutcomeId)
      })
      
      # Cohort Method plots and tables
      
      data <- shiny::reactive({
        getCMEstimation(
          connectionHandler = connectionHandler,
          mySchema = resultDatabaseSettings$schema, 
          cmTablePrefix = resultDatabaseSettings$cmTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          databaseMetaData = resultDatabaseSettings$databaseMetaData,
          targetId = targetId(),
          outcomeId = outcomeId()
        )
      })
      
      data2 <- shiny::reactive({
        getMetaEstimation(
          connectionHandler = connectionHandler,
          mySchema = resultDatabaseSettings$schema, 
          cmTablePrefix = resultDatabaseSettings$cmTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          esTablePrefix = resultDatabaseSettings$tablePrefix,
          targetId = targetId(),
          outcomeId = outcomeId()
        )
      })
      
      output$esCohortMethodPlot <- shiny::renderPlot(
        createPlotForAnalysis(
          unique(rbind(data(),data2()))
        )
      )
      

      output$esCohortMethodTable <- reactable::renderReactable(
        reactable::reactable(
          data =  unique(rbind(data(),data2())) %>%
          dplyr::select(
            -c("targetId","outcomeId", "comparatorId", "analysisId")
          ),
          
          rownames = FALSE, 
          defaultPageSize = 5,
          showPageSizeOptions = T, 
          striped = T,
          columns = list(
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
              ))
          )
        )
      )
      
      # download button
      output$downloadCohortMethodTable <- shiny::downloadHandler(
        filename = function() {
          paste('cohort-method-data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(unique(rbind(data(),data2())) %>%
                             dplyr::select(
                               -c("targetId","outcomeId", "comparatorId", "analysisId")
                             )
                           , con)
        }
      )
      
      # SCCS plots and tables
      
      sccsData <- shiny::reactive({
        getSccsEstimation(
          connectionHandler = connectionHandler,
          mySchema = resultDatabaseSettings$schema, 
          sccsTablePrefix = resultDatabaseSettings$sccsTablePrefix,
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
          esTablePrefix = resultDatabaseSettings$tablePrefix,
          databaseMetaData = resultDatabaseSettings$databaseMetaData,
          targetId = targetId(),
          outcomeId = outcomeId()
        )
      })
      
      output$esSccsPlot <- shiny::renderPlot(
        createPlotForSccsAnalysis(
          unique(sccsData())
        )
      )
      
      output$esSccsTable <- reactable::renderReactable(
        reactable::reactable(
          data =  unique(sccsData()) %>%
            dplyr::select(
              -c("targetId","outcomeId", "analysisId")
            ),
          
          rownames = FALSE, 
          defaultPageSize = 5,
          showPageSizeOptions = T, 
          striped = T,
          columns = list(
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
      )
    }
    )
  
}

    
getESTargetIds <- function(
  connectionHandler,
  mySchema, 
  cmTablePrefix,
  cgTablePrefix
){
  
  sql <- "select distinct
  c1.cohort_name as target,
  r.target_id
  
  from 
   @my_schema.@cm_table_prefixresult as r
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    my_schema = mySchema,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix
  ) 
  
  output <- as.list(result$targetId)
  names(output) <- result$target
  
  return(output)
  
}

getESOutcomeIds <- function(
  connectionHandler,
  mySchema, 
  cmTablePrefix,
  cgTablePrefix
) {
  sql <- "select distinct
  c1.cohort_name as outcome,
  r.outcome_id
  
  from 
   @my_schema.@cm_table_prefixresult as r
   inner join 
   @my_schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.outcome_id
   
  where 
  tco.outcome_of_interest = 1
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    my_schema = mySchema,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix
  ) 
  
  output <- as.list(result$outcomeId)
  names(output) <- result$outcome
  
  return(output)
  
} 


getCMEstimation <- function(
    connectionHandler,
    mySchema,
    cmTablePrefix = 'cm_',
    cgTablePrefix = 'cg_',
    databaseMetaData = 'database_meta_data',
    targetId,
    outcomeId
    ){
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
   @my_schema.@cm_table_prefixresult as r
   inner join 
   @my_schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @my_schema.@cm_table_prefixdiagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id and 
   r.database_id = unblind.database_id
   
   inner join
   @my_schema.@database_meta_data as db
   on db.database_id = r.database_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @my_schema.@cm_table_prefixanalysis as a
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
    my_schema = mySchema,
    database_meta_data = databaseMetaData,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix,
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
    mySchema,
    cmTablePrefix = 'cm_',
    cgTablePrefix = 'cg_',
    esTablePrefix = 'es_',
    targetId,
    outcomeId
){

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
   @my_schema.@es_table_prefixcm_result as r
   inner join 
   @my_schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @my_schema.@es_table_prefixcm_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id 
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @my_schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @my_schema.@es_table_prefixanalysis as ev
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
  my_schema = mySchema,
  cm_table_prefix = cmTablePrefix,
  cg_table_prefix = cgTablePrefix,
  es_table_prefix = esTablePrefix,
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
  mySchema, 
  sccsTablePrefix,
  cgTablePrefix,
  esTablePrefix,
  databaseMetaData,
  targetId,
  outcomeId
){
  
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
   @my_schema.@sccs_table_prefixresult as r
   inner join 
   @my_schema.@sccs_table_prefixexposures_outcome_set as eos
   on 
   r.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   inner join
   @my_schema.@sccs_table_prefixcovariate as cov
   on 
   r.covariate_id = cov.covariate_id and
   r.database_id = cov.database_id and
   r.analysis_id = cov.analysis_id and
   r.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   @my_schema.@sccs_table_prefixexposure as ex
   on 
   ex.era_id = cov.era_id and
   ex.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   
   @my_schema.@sccs_table_prefixdiagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.exposures_outcome_set_id = unblind.exposures_outcome_set_id and 
   r.covariate_id = unblind.covariate_id and
   r.database_id = unblind.database_id
   
   inner join
   @my_schema.@database_meta_data as db
   on db.database_id = r.database_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = cov.era_id

   inner join
   @my_schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = eos.outcome_id
   
   inner join
   @my_schema.@sccs_table_prefixanalysis as a
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
    my_schema = mySchema,
    database_meta_data = databaseMetaData,
    sccs_table_prefix = sccsTablePrefix,
    cg_table_prefix = cgTablePrefix,
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
   @my_schema.@es_table_prefixsccs_result as r
   inner join 
   @my_schema.@sccs_table_prefixexposures_outcome_set as eos
   on 
   r.exposures_outcome_set_id = eos.exposures_outcome_set_id
   
   inner join
   @my_schema.@sccs_table_prefixcovariate as cov
   on 
   r.covariate_id = cov.covariate_id and
   r.analysis_id = cov.analysis_id and
   r.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   @my_schema.@sccs_table_prefixexposure as ex
   on 
   ex.era_id = cov.era_id and
   ex.exposures_outcome_set_id = cov.exposures_outcome_set_id
   
   inner join
   
   @my_schema.@es_table_prefixsccs_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.exposures_outcome_set_id = unblind.exposures_outcome_set_id and 
   r.covariate_id = unblind.covariate_id and
   r.evidence_synthesis_analysis_id = unblind.evidence_synthesis_analysis_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = cov.era_id

   inner join
   @my_schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = eos.outcome_id
   
   inner join
   @my_schema.@sccs_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @my_schema.@es_table_prefixanalysis as ev
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
    my_schema = mySchema,
    es_table_prefix = esTablePrefix,
    sccs_table_prefix = sccsTablePrefix,
    cg_table_prefix = cgTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
  )

  return(rbind(result,result2))
  
}
  

createPlotForSccsAnalysis <- function(
  data
){
  
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

