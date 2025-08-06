# @file characterization-aggregateFeatures.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



characterizationRiskFactorViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shiny::helpText('View features that are associated with having or not having the outcome during the time-at-risk.'),
    
    # module that does input selection for a single row DF
    shinydashboard::box(
      collapsible = TRUE,
      title = "Options",
      width = "100%",
      shiny::uiOutput(ns("inputs"))
    ),
    
    
    shiny::conditionalPanel(
      condition = 'output.showRiskFactors != 0',
      ns = ns,
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
      shinydashboard::tabBox(
        width = "100%",
        # Title can include an icon
        title = shiny::tagList(shiny::icon("gear"), "Risk Factors"),
        
        shiny::tabPanel("Binary Feature Table", 
                        shiny::uiOutput(outputId = ns('helpTextBinary')),
                        resultTableViewer(ns('binaryTable'))
        ),
        shiny::tabPanel("Continuous Feature Table", 
                        shiny::uiOutput(outputId = ns('helpTextContinuous')),
                        resultTableViewer(ns('continuousTable'))
        )
      )
    )
  )

}



characterizationRiskFactorServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings,
    reactiveTargetRow,
    outcomeTable,
    reactiveOutcomeRowId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$showRiskFactors <- shiny::reactive(0)
      shiny::outputOptions(output, "showRiskFactors", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showRiskFactors <- shiny::reactive(0)
      })
      
      reactiveOutcomeTar <- shiny::reactiveVal(NULL)
      reactiveOutcomeTarValues <- shiny::reactiveVal(NULL)
      reactiveOutcomeWashout <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(reactiveOutcomeRowId(), {
        output$showRiskFactors <- shiny::reactive(0)
        
        if(reactiveOutcomeRowId() != 0){
          
          if(inherits(outcomeTable()[reactiveOutcomeRowId(),]$tarNames, 'character')){
            tarNames <- strsplit(
              x = outcomeTable()[reactiveOutcomeRowId(),]$tarNames, 
              split = ':'
            )[[1]]
          } else{
            tarNames <- NULL
          }
          reactiveOutcomeTar(tarNames)
          
          if(inherits(outcomeTable()[reactiveOutcomeRowId(),]$tarStrings, 'character')){
            tarStrings <- lapply(strsplit(
              x = outcomeTable()[reactiveOutcomeRowId(),]$tarStrings, 
              split = ':'
            )[[1]], function(x){
              vals <- strsplit(x = x, split = '/')[[1]]
              list(
                riskWindowStart = vals[1],
                startAnchor = vals[2],
                riskWindowEnd = vals[3],
                endAnchor = vals[4]
              )
            })
          } else{
            tarStrings <- NULL
          }
          reactiveOutcomeTarValues(tarStrings)
          
          if(inherits(outcomeTable()[reactiveOutcomeRowId(),]$outcomeWashoutDays, 'character')){
            outcomeWashoutDays <- strsplit(
              x = outcomeTable()[reactiveOutcomeRowId(),]$outcomeWashoutDays, 
              split = ':'
            )[[1]]
            } else{
              outcomeWashoutDays <- NULL
            }
          reactiveOutcomeWashout(outcomeWashoutDays)
          
        } else{
          reactiveOutcomeTar(NULL)
          reactiveOutcomeWashout(NULL)
        }
        
      })
      
      # get databases
      databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseString, split = ', ')))
      databaseIds <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseIdString, split = ', ')))
        
      output$inputs <- shiny::renderUI({ # need to make reactive?
        
        shiny::div( # TODO make this an options box that can be collapsed
          
          tableSelectionViewer(id = session$ns('outcome-table-select-risk')),
            
          shiny::selectInput(
            inputId = session$ns('databaseName'),
            label = 'Database: ',
            choices = databaseNames(),
            selected = databaseNames()[1],
            multiple = F
          ),
          
          shiny::selectInput(
            inputId = session$ns('tarInd'),
            label = 'Time-at-risk: ',
            choices = reactiveOutcomeTar(),
            selected = reactiveOutcomeTar()[1],
            multiple = F
          ),
          
          shiny::selectInput(
            inputId = session$ns('outcomeWashout'),
            label = 'Outcome washout: ',
            choices = reactiveOutcomeWashout(),
            selected = reactiveOutcomeWashout()[1],
            multiple = F
          ),
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate'
          )
        )
        
      })
      
      # server for outcome seleciton table
      tableSelectionServer(
        id = 'outcome-table-select-risk',
        table = shiny::reactive(outcomeTable() %>%
                                  dplyr::select("parentName", "cohortName", "cohortId") %>%
                                  dplyr::relocate("parentName", .before = "cohortName") %>%
                                  dplyr::relocate("cohortId", .after = "cohortName")
        ), 
        selectedRowId = reactiveOutcomeRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector'),
        inputColumns = characterizationOutcomeDisplayColumns(),
        displayColumns = characterizationOutcomeDisplayColumns(), 
        selectButtonText = 'Select Outcome'
      )
      
      # save the selections
      selected <- shiny::reactiveVal(value = NULL)
      
      shiny::observeEvent(input$generate, {
        # add target, outcome, database and tar check
        reactiveOutcomeRow <- outcomeTable()[reactiveOutcomeRowId(),]
        
        if(is.null(reactiveTargetRow()) | is.null(reactiveOutcomeRow) |
           input$tarInd == "" | is.null(input$databaseName) |
           input$outcomeWashout == ""){
          
          output$showRiskFactors <- shiny::reactive(0)
          shiny::showNotification('Need to set all inputs')
        } else{
          
          if(nrow(reactiveTargetRow()) == 0 | nrow(reactiveOutcomeRow) == 0){
            output$showRiskFactors <- shiny::reactive(0)
            shiny::showNotification('Need to pick a target and outcome')
          } else{
            output$showRiskFactors <- shiny::reactive(1)
            
            selected(
              data.frame(
                Target = reactiveTargetRow()$cohortName,
                Outcome = reactiveOutcomeRow$cohortName,
                Database = input$databaseName,
                `Time-at-risk` = input$tarInd,
                OutcomeWashoutDays = input$outcomeWashout
              )
            )
            
            inputSelectionDfServer(
              id = 'inputSelected', 
              dataFrameRow = selected,
              ncol = 1
            )
            
            
            caseCount <- OhdsiReportGenerator::getCaseCounts(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              cTablePrefix = resultDatabaseSettings$cTablePrefix,
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
              databaseTable = resultDatabaseSettings$databaseTable,
              targetIds = reactiveTargetRow()$cohortId,
              outcomeIds = reactiveOutcomeRow$cohortId,
              databaseIds = databaseIds()[input$databaseName == databaseNames()], 
              riskWindowStart = reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$riskWindowStart,
              riskWindowEnd = reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$riskWindowEnd,
              startAnchor = reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$startAnchor,
              endAnchor = reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]$endAnchor
            )
            
            targetCount <- OhdsiReportGenerator::getCaseTargetCounts(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema,
              cTablePrefix = resultDatabaseSettings$cTablePrefix,
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
              databaseTable = resultDatabaseSettings$databaseTable,
              targetIds = reactiveTargetRow()$cohortId,
              outcomeIds = reactiveOutcomeRow$cohortId,
              databaseIds = databaseIds()[input$databaseName == databaseNames()]
            )
            
            output$helpTextBinary <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts (restricted to first index date and requiring ",
                                     caseCount$minPriorObservation[1]," days observation prior to index) stratified by whether they had the outcome during the time-at-risk with a history of each binary features across databases."))
            )
            output$helpTextContinuous <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts (restricted to first index date and requiring ",
                                     caseCount$minPriorObservation[1]," days observation prior to index) stratified by whether they had the outcome during the time-at-risk with a history of each continuous features across databases."))
            )
            
            caseCount <- caseCount %>% 
              dplyr::filter(
                .data$outcomeWashoutDays == !!input$outcomeWashout &
                 .data$minPriorObservation == !!caseCount$minPriorObservation[1]
                )
            caseN <- caseCount$personCount[1]
            
            targetCount <- targetCount %>% 
              dplyr::filter(
                .data$outcomeWashoutDays == !!input$outcomeWashout &
                  .data$minPriorObservation == !!caseCount$minPriorObservation[1]
              )
            nonCaseN <- targetCount$personCount[1]
            targetN <- targetCount$withoutExcludedPersonCount[1]

            groupColumns <- list(
              reactable::colGroup(
                name = paste0('Case ', ' (N = ',caseN,')'), 
                columns = c(
                  paste0('caseCount'), 
                  paste0('caseAverage'))
              ),
              reactable::colGroup(
                name = paste0('Non Case ', ' (N = ',nonCaseN,')'), 
                columns = c(
                  paste0('nonCaseCount'), 
                  paste0('nonCaseAverage'))
              )
            )
            
             allData <- characterizationGetRiskFactorData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = reactiveTargetRow()$cohortId,
              outcomeId = reactiveOutcomeRow$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              tar = reactiveOutcomeTarValues()[[which(input$tarInd == reactiveOutcomeTar())]]
            )
            
            binTableOutputs <- resultTableServer(
              id = "binaryTable", 
              df = tryCatch({allData$binary %>%
                dplyr::filter(.data$outcomeWashoutDays == !!input$outcomeWashout)},
              error = function(e){return(NULL)}),
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = reactiveOutcomeRow$cohortName,
                caseN = caseN,
                nonCaseN = nonCaseN,
                Database = input$databaseName,
                TimeAtRisk = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]],
                Analysis = 'Exposed Cases Summary - Risk Factor'
              ),
              downloadedFileName = 'risk_factor_binary',
              colDefsInput = characteriationRiskFactorColDefs(
                elementId = session$ns('binary-table-filter')
              ), # function below
              addActions = NULL,
              columnGroups = groupColumns,
              elementId = session$ns('binary-table-filter'), 
            )
            
            groupColumnsContinuous <- list(
              reactable::colGroup(
                name = paste0('Case ', ' (N = ',caseN ,')'), 
                columns = c(
                  paste0('caseCountValue'), 
                  paste0('caseMinValue'), 
                  paste0('caseMaxValue'), 
                  paste0('caseAverageValue'),
                  paste0('caseStandardDeviation'),
                  paste0('caseMedianValue')
                  )
              ),
              reactable::colGroup(
                name = paste0('Target ', ' (N = ',targetN,')'), 
                columns = c(
                  paste0('targetCountValue'), 
                  paste0('targetMinValue'), 
                  paste0('targetMaxValue'), 
                  paste0('targetAverageValue'),
                  paste0('targetStandardDeviation'),
                  paste0('targetMedianValue')
                )
            )
            )
            
            conTableOutputs <- resultTableServer(
              id = "continuousTable", 
              df = tryCatch({allData$continuous %>%
                dplyr::filter(.data$outcomeWashoutDays == !!input$outcomeWashout)},
                error = function(e){return(NULL)}),
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = reactiveOutcomeRow$cohortName,
                caseN = caseN,
                targetN = targetN,
                Database = input$databaseName,
                TimeAtRisk = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]],
                Analysis = 'Exposed Cases Summary - Risk Factor continuous'
              ),
              downloadedFileName = 'risk_factor_continuous',
              colDefsInput = characteriationRiskFactorContColDefs(),
              addActions = NULL,
              columnGroups = groupColumnsContinuous, 
              elementId = session$ns('risk_factor_continuous')
            )
            
          }
        }
        
      })
   
  return(invisible(NULL))
    }
  )
}


characterizationGetRiskFactorData <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetId,
  outcomeId,
  databaseId,
  tar
){
  
  shiny::withProgress(message = 'Getting risk factor data', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Extracting binary"))
    
    binary <- OhdsiReportGenerator::getBinaryRiskFactors(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      cTablePrefix = resultDatabaseSettings$cTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      targetId =  targetId, 
      outcomeId = outcomeId, 
      databaseId = databaseId, 
      analysisIds = NULL,
      riskWindowStart = tar$riskWindowStart,
      riskWindowEnd = tar$riskWindowEnd,
      startAnchor = tar$startAnchor,
      endAnchor = tar$endAnchor
    )
    
  message(paste0('Extracted ',nrow(binary),' binary RF rows'))
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  continuous <- OhdsiReportGenerator::getContinuousRiskFactors(
    connectionHandler = connectionHandler, 
    schema = resultDatabaseSettings$schema, 
    cTablePrefix = resultDatabaseSettings$cTablePrefix, 
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
    databaseTable = resultDatabaseSettings$databaseTable, 
    targetId =  targetId, 
    outcomeId = outcomeId, 
    databaseId = databaseId
  ) 
  
  message(paste0('Extracted ',nrow(continuous),' continuous RF rows'))
  
  shiny::incProgress(4/4, detail = paste("Done"))
  
  })
  
  return(
    list(
      binary = binary,
      continuous = continuous
    )
  )
}


characteriationRiskFactorColDefs <- function(
    elementId
    ){
  result <- list(
    
    databaseName = reactable::colDef(
      show = FALSE
    ),
    databaseId = reactable::colDef(
      show = FALSE
    ),
    targetName = reactable::colDef(
      show = FALSE
    ),
    targetCohortId = reactable::colDef(
      show = FALSE
    ),
    outcomeName = reactable::colDef(
      show = FALSE
    ),
    outcomeCohortId = reactable::colDef(
      show = FALSE
    ),
    riskWindowStart = reactable::colDef(
      show = FALSE
    ),
    riskWindowEnd = reactable::colDef(
      show = FALSE
    ),
    startAnchor = reactable::colDef(
      show = FALSE
    ),
    endAnchor = reactable::colDef(
      show = FALSE
    ),
    
    covariateId = reactable::colDef(
      show = FALSE
    ),
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE
      ), 
    outcomeWashoutDays = reactable::colDef(
      show = FALSE
    ),
    nonCaseCount = reactable::colDef(
      name = "# Non-cases with Feature Before Exposure",
      header = withTooltip("# Non-cases with Feature Before Exposure",
                           "Number of non-cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(
        percent = FALSE,
        separators = TRUE
        ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('< ', abs(value))
      }
    ), 
    caseCount = reactable::colDef(
      name = "# Cases with Feature Before Exposure",
      header = withTooltip("# Cases with Feature Before Exposure",
                           "Number of cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(
        separators = TRUE, 
        percent = FALSE
        ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('< ', abs(value))
      }
    ), 
    nonCaseAverage = reactable::colDef(
      name = "% Non-cases with Feature Before Exposure",
      header = withTooltip("% Non-cases with Feature Before Exposure",
                           "Percent of non-cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    caseAverage = reactable::colDef(
      name = "% Cases with Feature Before Exposure",
      header = withTooltip("% Cases with Feature Before Exposure",
                           "Percent of Cases for the outcome with the feature before exposure"),
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ), 
    
    SMD = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference"),
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    absSMD = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD",
                           "Absolute value of standardized mean difference"),
      format = reactable::colFormat(digits = 2, percent = FALSE),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }"),
      filterInput = function(values, name) {
        oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", elementId, name)
        shiny::tags$input(
          type = "range",
          min = floor(min(values, na.rm = TRUE)),
          max = ceiling(max(values, na.rm = TRUE)),
          value = floor(min(values, na.rm = TRUE)),
          oninput = oninput,
          onchange = oninput, # For IE11 support
          "aria-label" = sprintf("Filter by minimum %s", name)
        )
      }
    )
  )
  return(result)
}



characteriationRiskFactorContColDefs <- function(
    ){
  result <- list(
    databaseName = reactable::colDef(
      show = FALSE
    ),
    databaseId = reactable::colDef(
      show = FALSE
    ),
    targetName = reactable::colDef(
      show = FALSE
    ),
    targetCohortId = reactable::colDef(
      show = FALSE
    ),
    outcomeName = reactable::colDef(
      show = FALSE
    ),
    outcomeCohortId = reactable::colDef(
      show = FALSE
    ),
    riskWindowStart = reactable::colDef(
      show = FALSE
    ),
    riskWindowEnd = reactable::colDef(
      show = FALSE
    ),
    startAnchor = reactable::colDef(
      show = FALSE
    ),
    endAnchor = reactable::colDef(
      show = FALSE
    ),
    
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = TRUE,
      minWidth = 300
    ),
    covariateId = reactable::colDef(
      show = FALSE
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE
    ), 
    outcomeWashoutDays = reactable::colDef(
      show = FALSE
    ),
    
    caseCountValue = reactable::colDef(
      name = "Number",
        header = withTooltip("Number",
                             "Case number with feature"),
        filterable = TRUE
      , 
      format = reactable::colFormat(
        percent = FALSE,
        separators = TRUE
      ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >=0) value else paste0('< ', abs(value))
      }
    ),
    targetCountValue = reactable::colDef(
      name = "Number",
      header = withTooltip("Number",
                           "Target number with feature"),
      filterable = TRUE
      , 
      format = reactable::colFormat(
        percent = FALSE,
        separators = TRUE
      ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >=0) value else paste0('< ', abs(value))
      }
    ),
    
    caseAverageValue = reactable::colDef(
      name = "Mean",
      header = withTooltip("Mean",
                           "Mean value of the feature in the case population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    targetAverageValue = reactable::colDef(
      name = "Mean",
      header = withTooltip("Mean",
                           "Mean value of the feature in the target population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    caseStandardDeviation = reactable::colDef(
      name = "StDev",
      header = withTooltip("StDev",
                           "Standard deviation of the feature value in the case population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    targetStandardDeviation = reactable::colDef(
      name = "StDev",
      header = withTooltip("StDev",
                           "Standard deviation of the feature value in the target population"), 
      filterable = TRUE,
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    
    caseMedianValue  = reactable::colDef(
      name = "Median",
      header = withTooltip("Median",
                           "Median of the feature value in the cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    targetMedianValue  = reactable::colDef(
      name = "Median",
      header = withTooltip("Median",
                           "Median of the feature value in the targets"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    
    caseP10Value  = reactable::colDef(
      show = FALSE
    ),
    targetP10Value  = reactable::colDef(
      show = FALSE
    ),
    caseP25Value  = reactable::colDef(
      show = FALSE
    ),
    targetP25Value  = reactable::colDef(
      show = FALSE
    ),
    caseP75Value  = reactable::colDef(
      show = FALSE
    ),
    targetP75Value  = reactable::colDef(
      show = FALSE
    ),
    caseP90Value  = reactable::colDef(
      show = FALSE
    ),
    targetP90Value  = reactable::colDef(
      show = FALSE
    ),
    caseMaxValue  = reactable::colDef(
      name = "Max",
      header = withTooltip("Max",
                           "Maximum of the feature value in the cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    targetMaxValue  = reactable::colDef(
      name = "Max",
      header = withTooltip("Max",
                           "Maximum of the feature value in the targets"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    caseMinValue  = reactable::colDef(
      name = "Min",
      header = withTooltip("Min",
                           "Minimum of the feature value in the cases"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    targetMinValue  = reactable::colDef(
      name = "Min",
      header = withTooltip("Min",
                           "Minimum of the feature value in the targets"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ),
    
    #targetBoxPlot = reactable::colDef(cell = function(value, index) {
    #  ggplot2::ggplot() +
    #        ggplot2::geom_boxplot(
    #              ggplot2::aes(
    #                x = 1, 
    #                ymin = data$targetMinValue[index], 
    #                lower = data$targetP10Value[index], 
    #                middle = data$targetMedianValue[index], 
    #                upper = data$targetP90Value[index], 
    #                ymax = data$targetMaxValue[index]
    #                ),
    #              stat = "identity"
    #          )
    #}), 
    #caseBoxPlot  = reactable::colDef(cell = function(value, index) {
    #    sparkline(vcs_boxp_data$em_red_per_th[[index]], type = "box")
    #  }), 
    #caseBoxPlot  = reactable::colDef(cell = function(value, index) {
    #  sparkline::sparkline(vcs_boxp_data$em_red_per_th[[index]], type = "box")
    #  }), 
    
    # low_outlier, low_whisker, q1, median, q3, high_whisker, high_outlier
    #sparkline::spk_chr(c(data$targetMinValue[index], data$targetP10Value[index], data$targetP25Value[index], data$targetMedianValue[index], 3, 6, 6), type="box", raw = TRUE, width = 200)
    
    SMD = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference"), 
      filterable = TRUE, 
      format = reactable::colFormat(digits = 2, percent = FALSE)
    ), 
    absSMD = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD",
                           "Absolute value of the standardized mean difference"), 
      format = reactable::colFormat(digits = 2, percent = FALSE),
      filterable = TRUE
    )
  )
  return(result)
}

