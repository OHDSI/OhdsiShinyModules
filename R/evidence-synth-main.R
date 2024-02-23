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
    
    # add two buttons - CM or SCCs
    shiny::tabsetPanel(
      id = ns('typeTab'),
      type = 'pills'
    )

  )

}

checkSccsTablesPresent <- function(connectionHandler, resultDatabaseSettings) {
  sql <- "
  SELECT 1 as present FROM @schema.@sccs_table_prefixdiagnostics_summary;
  "
  present <- TRUE
  tryCatch({
    connectionHandler$queryDb(sql = sql,
                              schema = resultDatabaseSettings$schema,
                              sccs_table_prefix = resultDatabaseSettings$sccsTablePrefix)
  }, error = function(...) {
    present <<- FALSE
  })

  return(present)
}

checkCmTablesPresent <- function(connectionHandler, resultDatabaseSettings) {
  sql <- "
  SELECT 1 as present FROM @schema.@cm_table_prefixdiagnostics_summary;
  "
  present <- TRUE
  tryCatch({
    connectionHandler$queryDb(sql = sql,
                              schema = resultDatabaseSettings$schema,
                              cm_table_prefix = resultDatabaseSettings$cmTablePrefix)
  }, error = function(...) {
    present <<- FALSE
  })

  return(present)
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

      showSccsResults <- checkSccsTablesPresent(connectionHandler = connectionHandler,
                                                resultDatabaseSettings = resultDatabaseSettings)

      showCmResults <- checkCmTablesPresent(connectionHandler = connectionHandler,
                                            resultDatabaseSettings = resultDatabaseSettings)

      if (showCmResults) {
        shiny::insertTab(
          inputId = "typeTab",
          tab =
            shiny::tabPanel(
              title = 'Cohort Method',
              evidenceSynthesisCmViewer(id = session$ns('cohortMethodTab')),
            ),
          select = TRUE
        )

        evidenceSynthesisCmServer(
          id = 'cohortMethodTab',
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )

      }

      if (showSccsResults) {
        shiny::insertTab(
          inputId = "typeTab",
          tab = shiny::tabPanel(
            title = "Self Controlled Case Series",
            evidenceSynthesisSccsViewer(id = session$ns('sccsTab')),
          ),
          select = !showCmResults
        )

        evidenceSynthesisSccsServer(
          id = 'sccsTab',
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      }
    }
  )
}

# Function to get outcome ids
# used by both cm and sccs
getEsOutcomeIds <- function(
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

# Function to format results
# used by both cm and sccs
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



# Functions to get column formatting and names
# used by both cm and sccs
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
  
  outcomes <- getEsOutcomeIds(
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
