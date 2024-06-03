# @file characterization-timeToEvent.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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


# view two cohorts and compare
characterizationCohortComparisonViewer <- function(id) {
  ns <- shiny::NS(id)
  
    # module that does input selection for a single row DF
    shiny::div(
      
      # UI for inputs
      # summary table
      shinydashboard::box(
        collapsible = TRUE,
        title = "Options",
        width = "100%",
        shiny::uiOutput(ns("inputs"))
      ),
      
      # displayed inputs
      shiny::conditionalPanel(
        condition = "input.generate != 0",
        ns = ns,
        
        inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
        
        # add basic table 
        shiny::tabsetPanel(
          type = 'pills',
          shiny::tabPanel(
            title = 'Counts',
            resultTableViewer(id = ns('countTable'), boxTitle = 'Counts')
          ),
          shiny::tabPanel(
            title = 'Binary',
            resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
          ),
          shiny::tabPanel(
            title = 'Continuous',
            resultTableViewer(id = ns('continuousTable'), boxTitle = 'Continuous')
          )
        )
        
      )
    )
}



characterizationCohortComparisonServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    options,
    parents,
    parentIndex, # reactive
    subTargetId # reactive
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      inputVals <- shiny::reactive({characterizationGetCohortsInputs(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        targetId = subTargetId
      )})
      
      
      # initial comp chilren
      comparatorOptions <- characterizationGetChildren(options, 1)
      output$inputs <- shiny::renderUI({
        
        shiny::div(
          shinyWidgets::pickerInput(
            inputId = session$ns('comparatorGroup'),
            label = 'Comparator Group: ',
            choices = parents,
            selected = parents[1],
            multiple = F,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50,
              #container = "div.tabbable",
              dropupAuto = FALSE
            )
          ),
          
          shiny::selectInput(
            inputId = session$ns('comparatorId'),
            label = 'Comparator: ',
            choices = comparatorOptions,
            selected = comparatorOptions[1],
            multiple = F
          ),
        
        shinyWidgets::pickerInput(
          inputId = session$ns('databaseId'),
          label = 'Database: ',
          choices = inputVals()$databaseIds,
          selected = inputVals()$databaseIds[1],
          multiple = F,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            dropupAuto = F,
            size = 10,
            liveSearchStyle = "contains",
            liveSearchPlaceholder = "Type here to search",
            virtualScroll = 500
          )
        ),
        
        shiny::actionButton(
          inputId = session$ns('generate'), 
          label = 'Generate'
            )
      )
      
      })
      
      # update comparatorId
      comparatorGroups <- shiny::reactiveVal()
      comparatorIndex <- shiny::reactiveVal(1)
      shiny::observeEvent(input$comparatorGroup,{
        comparatorIndex(which(input$comparatorGroup == parents))
        result <- characterizationGetChildren(options, comparatorIndex())
        comparatorGroups(result)
        shiny::updateSelectInput(
          session = session, 
          inputId = 'comparatorId', 
          label = 'Comparator: ',
          choices = result,
          selected = result[1]
        )
      })
      
    

      # show selected inputs to user
      inputSelectionDfServer(
        id = 'inputSelected', 
        dataFrameRow = selected,
        ncol = 1
      )
      
      #get results
      selected <- shiny::reactiveVal()
      shiny::observeEvent(input$generate,{
        
        runTables <- TRUE
        
        if(is.null(subTargetId()) | is.null(input$comparatorId)){
          runTables <- FALSE
        }
        if(is.null(input$databaseId)){
          runTables <- FALSE
        }
        
        if(subTargetId() == input$comparatorId){
          runTables <- FALSE
          shiny::showNotification('Must select different cohorts')
        }
        
        # ADDED
        subTargetIds <- unlist(lapply(options[[parentIndex()]]$children, function(x){x$subsetId}))
        subTargetNames <- unlist(lapply(options[[parentIndex()]]$children, function(x){x$subsetName}))
        
        selected(
          data.frame(
            Comparator = names(comparatorGroups())[which(comparatorGroups() == input$comparatorId)],
            Database = names(inputVals()$databaseIds)[input$databaseId == inputVals()$databaseIds]
          )
        )
        
        selection1 <- subTargetId()
        #selection1 <- options[[parentIndex()]]$children[[which(subTargetIds == subTargetId())]]$charIds %>%
        #  dplyr::filter(.data$databaseId == input$databaseId) %>%
        #  dplyr::filter(.data$cohortType %in% c('T','O')) %>%
        # dplyr::filter(.data$cohortDefinitionId == subTargetId()) # is this needed?
        
        #if(nrow(selection1) == 0){
        if(length(selection1) == 0){
          runTables <- FALSE
          shiny::showNotification('No results for section 1')
        }
        
        selection2 <- input$comparatorId
        #selection2 <- options[[comparatorIndex()]]$children[[which(comparatorGroups() == input$comparatorId)]]$charIds %>%
        #  dplyr::filter(.data$databaseId == input$databaseId) %>%
        #  dplyr::filter(.data$cohortType %in% c('T','O')) %>%
        #  dplyr::filter(.data$cohortDefinitionId == input$comparatorId)
        
        #if(nrow(selection2) == 0){
        if(length(selection2) == 0){
          runTables <- FALSE
          shiny::showNotification('No results for section 2')
        }

        
        if(runTables){
          resultTable <- characterizatonGetCohortComparisonData(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            #targetId1 = selection1$charCohortId[1],
            #targetId2 = selection2$charCohortId[1],
            targetId1 = selection1,
            targetId2 = selection2,
            databaseId = input$databaseId,
            minThreshold = 0.01,
            addSMD = T
          )
          
          countTable <- characterizatonGetCohortCounts(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            #targetIds = c(selection1$charCohortId[1],selection2$charCohortId[1]),
            targetIds = c(selection1,selection2),
            targetNames = c(
              subTargetNames[which(subTargetIds == subTargetId())], # modified
              names(comparatorGroups())[which(input$comparatorId== comparatorGroups())]
            ),
            databaseId = input$databaseId
          )
          
          characteriationCountTableColDefs <- function(){
            result <- list(
              selection = reactable::colDef(
                header = withTooltip("Selected Cohort",
                                     "Which cohort of the above selections"),
                filterable = T
              ),
              covariateName = reactable::colDef(
                header = withTooltip("Covariate Name",
                                     "Name of the covariate"),
                filterable = T
              ),
              rowCount = reactable::colDef(
                header = withTooltip("Record Count",
                                     "Count of the number of records"),
                filterable = T
              ), 
              personCount = reactable::colDef(
                header = withTooltip("Person Count",
                                     "Count of the number of persons"),
                filterable = T
              )
            )
            return(result)
          }
          
          continuousTable <- characterizatonGetCohortComparisonDataContinuous(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            #targetIds = c(selection1$charCohortId[1],selection2$charCohortId[1]),
            targetIds = c(selection1,selection2),
            databaseIds = rep(input$databaseId,2)
          )
          
          resultTableServer(
            id = 'mainTable',
            df = resultTable,
            colDefsInput = characterizationCohortsColumns()
          ) 
          
          resultTableServer(
            id = 'continuousTable',
            df = continuousTable,
            colDefsInput = characterizationCohortsColumnsContinuous()
          ) 
          
          resultTableServer(
            id = 'countTable',
            df = countTable,
            colDefsInput = characteriationCountTableColDefs()
          )} 
        # else{
        #     resultTableServer(
        #       id = 'mainTable',
        #       df = data.frame(),
        #       colDefsInput = columns
        #     ) 
        #     
        #     resultTableServer(
        #       id = 'countTable',
        #       df = data.frame(),
        #       colDefsInput = NULL
        #     ) 
        #   }
      })

      return(invisible(NULL))
      
    })
  
}


characterizationCohortsColumns <- function(x){
  list(
  covariateName = reactable::colDef(
    header = withTooltip(
      "Covariate Name",
      "The name of the covariate"
    )
  ),
  covariateId = reactable::colDef(
    show = F,
    header = withTooltip("Covariate ID",
                         "Unique identifier of the covariate")
  ),
  firstVar = reactable::colDef(
    show = F
  ),
  secondVar = reactable::colDef(
    show = F
  ),
  sumValue_1 = reactable::colDef(
    header = withTooltip("First Sum",
                         "The total sum of the covariate for the first selected cohort."),
    cell = function(value) {
      if (value >= 0) value else '< min threshold'
    }
  ),
  sumValue_2 = reactable::colDef(
    header = withTooltip("Second Sum",
                         "The total sum of the covariate for the second selected cohort."),
    cell = function(value) {
      if (value >= 0) value else '< min threshold'
    }
  ),
  averageValue_1 = reactable::colDef(
    header = withTooltip("First Mean",
                         "The mean of the covariate for the first selected cohort."), 
    cell = function(value) {
      if (value >= 0) round(value, digits = 3) else '< min threshold'
    }
  ),
  averageValue_2 = reactable::colDef(
    header = withTooltip("Second Mean",
                         "The mean of the covariate for the second selected cohort."),
    cell = function(value) {
      if (value >= 0) round(value, digits = 3) else '< min threshold'
    }
  ),
  SMD = reactable::colDef(
    header = withTooltip("SMD",
                         "Standardized mean difference"),
    format = reactable::colFormat(digits = 3)
  ),
  absSMD = reactable::colDef(
    header = withTooltip("absSMD",
                         "Absolute standardized mean difference"),
    format = reactable::colFormat(digits = 3)
  ),
  analysisName = reactable::colDef(
    header = withTooltip(
      "Covariate Class",
      "Class/type of the covariate"
    )
  )
  
  # add other columns
)
}

characterizationCohortsColumnsContinuous <- function(x){
  list(
    covariateName = reactable::colDef(
      header = withTooltip(
        "Covariate Name",
        "The name of the covariate"
      )
    ),
    covariateId = reactable::colDef(
      show = F,
      header = withTooltip("Covariate ID",
                           "Unique identifier of the covariate")
    ),
    countValue_1 = reactable::colDef(
      header = withTooltip("First Count",
                           "Number of people with the covariate for the first selected cohort."),
      cell = function(value) {
        if (value >= 0) value else '< min threshold'
      }
    ),
    countValue_2 = reactable::colDef(
      header = withTooltip("Second Count",
                           "Number of people with the covariate for the second selected cohort."),
      cell = function(value) {
        if (value >= 0) value else '< min threshold'
      }
    ),
    averageValue_1 = reactable::colDef(
      header = withTooltip("First Mean",
                           "The mean of the covariate for the first selected cohort."), 
      cell = function(value) {
        if (value >= 0) round(value, digits = 3) else '< min threshold'
      }
    ),
    averageValue_2 = reactable::colDef(
      header = withTooltip("Second Mean",
                           "The mean of the covariate for the second selected cohort."),
      cell = function(value) {
        if (value >= 0) round(value, digits = 3) else '< min threshold'
      }
    ),
    standardDeviation_1 = reactable::colDef(
      header = withTooltip("First StDev",
                           "The standard deviation of the covariate for the first selected cohort."), 
      cell = function(value) {
        if (value >= 0) round(value, digits = 3) else '< min threshold'
      }
    ),
    standardDeviation_2 = reactable::colDef(
      header = withTooltip("Second StDev",
                           "The standard deviation of the covariate for the second selected cohort."),
      cell = function(value) {
        if (value >= 0) round(value, digits = 3) else '< min threshold'
      }
    ),
    medianValue_1 = reactable::colDef(
      header = withTooltip("First Median",
                           "The median of the covariate for the first selected cohort."), 
      cell = function(value) {
        round(value, digits = 3)
      }
    ),
    medianValue_2 = reactable::colDef(
      header = withTooltip("Second Median",
                           "The median of the covariate for the second selected cohort."),
      cell = function(value) {
        round(value, digits = 3)
      }
    ),
    SMD = reactable::colDef(
      header = withTooltip("SMD",
                           "Standardized mean difference"),
      format = reactable::colFormat(digits = 3)
    ),
    absSMD = reactable::colDef(
      header = withTooltip("absSMD",
                           "Absolute standardized mean difference"),
      format = reactable::colFormat(digits = 3)
    ),
    minValue_2 = reactable::colDef(
      header = withTooltip("Second Min Value",
                           "Minimum value of the second selected cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    minValue_1 = reactable::colDef(
      header = withTooltip("First Min Value",
                           "Minimum value of the first selected cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    maxValue_2 = reactable::colDef(
      header = withTooltip("Second Max Value",
                           "Maximum value of the second selected cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    maxValue_1 = reactable::colDef(
      header = withTooltip("First Max Value",
                           "Maximum value of the first selected cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p25Value_2 = reactable::colDef(
      header = withTooltip("Second 25th %tile",
                           "25th percentile value of the second selected cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p25Value_1 = reactable::colDef(
      header = withTooltip("First 25th %tile",
                           "25th percentile value of the first selected cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p75Value_2 = reactable::colDef(
      header = withTooltip("Second 75th %tile",
                           "75th percentile value of the second selected cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p75Value_1 = reactable::colDef(
      header = withTooltip("First 75th %tile",
                           "75th percentile value of the first selected cohort"),
      format = reactable::colFormat(digits = 3)
    )
    
    # add other columns
  )
}

characterizatonGetCohortComparisonData <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId1,
    targetId2,
    databaseId,
    minThreshold, 
    addSMD
){
  result <- characterizatonGetCohortData(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetIds = c(targetId1, targetId2),
    databaseIds = c(databaseId,databaseId),
    minThreshold = minThreshold, 
    addSMD = addSMD
  )
  
  return(result)
}


characterizatonGetCohortCounts <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    targetNames,
    databaseId
){
  
  start <- Sys.time()
  result <- connectionHandler$queryDb(
    sql = "
select  distinct cc.target_cohort_id as cohort_definition_id,
        cc.min_prior_observation,
        cc.row_count,
        cc.person_count
       from 
  @schema.@c_table_prefixcohort_counts cc 
  where 
  cc.target_cohort_id in (@target_ids) 
  and cc.cohort_type = 'Target'
  AND cc.database_id = '@database_id'
  
  union
  
  select  cc.outcome_cohort_id as cohort_definition_id,
        cc.min_prior_observation,
        cc.row_count,
        cc.person_count
       from 
  @schema.@c_table_prefixcohort_counts cc 
  where 
  cc.outcome_cohort_id in (@target_ids) 
  and cc.cohort_type = 'Outcome'
  AND cc.database_id = '@database_id'
  ;
  ", 
    schema = resultDatabaseSettings$schema, 
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_ids =   paste0(targetIds, collapse= ','),
    database_id = databaseId
  )
  end <- Sys.time() - start 
  message(paste0('Extracting ', nrow(result) ,' cohort count rows took: ', round(end, digits = 2), ' ', units(end)))
  
  result <- merge(
    x = result,
    y = data.frame(
      selection = c('First','Second'),
      cohortDefinitionId = targetIds,
      cohortName = targetNames
    ), 
    by = 'cohortDefinitionId'
  )
  
  result <- result %>% dplyr::select(
    'selection',
    'cohortName', 
    'minPriorObservation',
    'rowCount',
    'personCount'
  )
  
  return(result)
  
}


characterizatonGetCohortData <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    databaseIds,
    minThreshold = 0.01,
    addSMD = F
){
  
  if(is.null(targetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
   return(NULL)
  }

  shiny::withProgress(message = 'characterizatonGetCohortData', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Setting types"))
    
    types <- data.frame(
      type = 1:length(targetIds),
      cohortDefinitionId = targetIds,
      databaseId = databaseIds
    )
    
    # 
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    sql <- paste0(
      paste0(
        paste0(
          "select  ref.covariate_name, 
          s.min_prior_observation,
          cov.target_cohort_id as cohort_definition_id,
          cov.* from   
    @schema.@c_table_prefixCOVARIATES cov 
    inner join 
    @schema.@c_table_prefixcovariate_ref ref
    on cov.covariate_id = ref.covariate_id
    and cov.setting_id = ref.setting_id
    and cov.database_id = ref.database_id
    inner join 
    @schema.@c_table_prefixsettings s
    on s.database_id = cov.database_id
    and s.setting_id = cov.setting_id
    
    where 
    (cov.target_cohort_id = @target_id",1:length(targetIds),
    " and cov.cohort_type = 'Target') 
     AND cov.database_id = '@database_id",1:length(targetIds),"'
     AND cov.average_value >= @min_threshold"
        ), collapse = ' union '
      ),';'
    )
    
    inputList <- as.list(c(targetIds, databaseIds))
    names(inputList) <- c(paste0('target_id', 1:length(targetIds)), paste0('database_id', 1:length(databaseIds)))
    inputList$sql <- sql
    inputList$schema <- resultDatabaseSettings$schema
    inputList$c_table_prefix <- resultDatabaseSettings$cTablePrefix
    inputList$min_threshold <- minThreshold
    
    start <- Sys.time()
    # settings.min_characterization_mean needed?
    res <- do.call(what = connectionHandler$queryDb, args = inputList)
    end <- Sys.time() - start 
    shiny::incProgress(3/4, detail = paste("Extracted data"))
    message(paste0('Extracting ', nrow(res) ,' characterization cohort rows took: ', round(end, digits = 2), ' ', units(end)))
    
    # add the first/section type
    res <- merge(res, types, by = c('cohortDefinitionId','databaseId'))
    
    # pivot
    result <- tidyr::pivot_wider(
      data = res, 
      id_cols = c('covariateName', 'covariateId','minPriorObservation'), 
      names_from = 'type', 
      values_from = c('sumValue', 'averageValue'), 
      values_fn = mean, 
      values_fill = -1
    ) 
    
    if(addSMD == T){
      # TODO get min_characterization_mean from settings table
      # minCharacterizationMean <- minThreshold
      # add SMD
      convertMissing <- function(vec){sapply(vec, function(x) ifelse(x==-1, minThreshold, x))}
      result$firstVar <- convertMissing(result$averageValue_1)*(1-convertMissing(result$averageValue_1))^2
      result$secondVar <- convertMissing(result$averageValue_2)*(1-convertMissing(result$averageValue_2))^2
      result$SMD <- (convertMissing(result$averageValue_1) - convertMissing(result$averageValue_2))/(sqrt((result$firstVar+result$secondVar)/2))
      result$absSMD <- abs(result$SMD)
    }
    
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
  return(result)
}


characterizatonGetCohortComparisonDataContinuous <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetIds,
  databaseIds
){
  
  if(is.null(targetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
    return(NULL)
  }
  
  shiny::withProgress(message = 'characterizatonGetCohortDataContinuous', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Setting types"))
    
    types <- data.frame(
      type = 1:length(targetIds),
      cohortDefinitionId = targetIds,
      databaseId = databaseIds
    )
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    sql <- paste0(
      paste0(
        paste0(
          "select  ref.covariate_name, 
          s.min_prior_observation,
          cov.target_cohort_id as cohort_definition_id,
          cov.* from   
    @schema.@c_table_prefixCOVARIATES_continuous cov 
    inner join 
    @schema.@c_table_prefixcovariate_ref ref
    on cov.covariate_id = ref.covariate_id
    and cov.setting_id = ref.setting_id
    and cov.database_id = ref.database_id
    inner join
    @schema.@c_table_prefixsettings s 
    on cov.setting_id = s.setting_id
    and cov.database_id = s.database_id
    
    where 
    (cov.target_cohort_id = @target_id",1:length(targetIds),
          " and cov.cohort_type = 'Target') 
     AND cov.database_id = '@database_id",1:length(targetIds),"'
     "
        ), collapse = ' union '
      ),';'
    )
    
    inputList <- as.list(c(targetIds, databaseIds))
    names(inputList) <- c(paste0('target_id', 1:length(targetIds)), paste0('database_id', 1:length(databaseIds)))
    inputList$sql <- sql
    inputList$schema <- resultDatabaseSettings$schema
    inputList$c_table_prefix <- resultDatabaseSettings$cTablePrefix
    
    start <- Sys.time()
    # settings.min_characterization_mean needed?
    res <- do.call(what = connectionHandler$queryDb, args = inputList)
    end <- Sys.time() - start 
    shiny::incProgress(3/4, detail = paste("Extracted data"))
    message(paste0('Extracting ', nrow(res) ,' characterization cohort rows took: ', round(end, digits = 2), ' ', units(end)))
    
    # add the first/section type
    res <- merge(res, types, by = c('cohortDefinitionId','databaseId'))
    
    # pivot
    result <- tidyr::pivot_wider(
      data = res, 
      id_cols = c('covariateName', 'covariateId','minPriorObservation'), 
      names_from = 'type', 
      values_from = c('countValue', 'averageValue', 'standardDeviation', 'medianValue','minValue', 'maxValue', 'p25Value','p75Value'), 
      values_fn = mean, 
      values_fill = -1
    ) 
    
    # if both have results then add SMD
    if(length(unique(res$type)) > 1){
      result <- result %>% 
        dplyr::mutate(
          SMD = (.data$averageValue_1-.data$averageValue_2)/(sqrt((.data$standardDeviation_1^2 + .data$standardDeviation_2^2)/2))
        ) %>%
        dplyr::mutate(
          absSMD = abs(.data$SMD)
        )
    }
    
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
  return(result)
}



characterizationGetCohortsInputs <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId # reactive
) {

  sql <- "select distinct 
  d.database_id, d.cdm_source_abbreviation as database_name
  from @schema.@database_table d
  
  inner join 
  @schema.@c_table_prefixcohort_details cd
  on d.database_id = cd.database_id
  where cd.target_cohort_id = @target_id
  and cd.cohort_type = 'Target'
  ;"
  
  database <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    database_table = resultDatabaseSettings$databaseTable,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId()
  )
  databaseIds <- database$databaseId
  names(databaseIds) <- database$databaseName

  return(
    list(
      databaseIds = databaseIds
    )
  )
}
