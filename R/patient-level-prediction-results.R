patientLevelPredictionResultsViewer  <- function(id=1) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("patient-level-prediction-www", "help-fullResults.html", package = utils::packageName())
    ),
    
    # states the selected results
    inputSelectionDfViewer(
      id = ns("df-md-selection"),
      title = 'Model Design Selected'
    ),
    
    inputSelectionDfViewer(
      id = ns("df-result-selection"),
      title = 'Result Selected'
    ),
    
    shiny::tabsetPanel(
      type = 'pills',
      id = ns('singleView'),
      
      shiny::tabPanel(
        "Model",
        patientLevelPredictionCovariateSummaryViewer(ns('covariateSummary'))
      ),
      
      shiny::tabPanel(
        "Discrimination",  
        patientLevelPredictionDiscriminationViewer(ns('discrimination'))
      ),
      
      shiny::tabPanel(
        "Calibration", 
        patientLevelPredictionCalibrationViewer(ns('calibration'))
      ),
      
      shiny::tabPanel(
        "Threshold Dependant", 
        patientLevelPredictionCutoffViewer(ns('cutoff'))
      ), 
      
      shiny::tabPanel(
        "Net Benefit", 
        patientLevelPredictionNbViewer(ns('netBenefit'))
      ),
      
      
      shiny::tabPanel(
        "Validation",
        patientLevelPredictionValidationViewer(ns('validation'))
      ),
      
      shiny::tabPanel(
        "Design Settings",
        patientLevelPredictionSettingsViewer(ns('settings'))
      )
      
    )
  )
}


patientLevelPredictionResultsServer <- function(
    id,
    modelDesignId, 
    developmentDatabaseId,
    performanceId,
    connectionHandler,
    resultDatabaseSettings,
    performance,
    tracker
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # if a different tab is viewed reset the results tab to models
      # the tracker adds 1 to value if a different tab is viewed
      shiny::observeEvent(
        tracker(),
        {
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'singleView',
          selected = 'Model'
        )
        }
      )

      # selections displayed
      selectedModelDesign <- shiny::reactive(
        getModelDesignInfo(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          modelDesignId = modelDesignId
        )
      )
      
      inputSelectionDfServer(
        id = "df-md-selection", 
        dataFrameRow = selectedModelDesign
      )
      
      selectedResults <- shiny::reactive(
        getPlpPerformanceSelection(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          performanceId = performanceId
        )
      )
      
      inputSelectionDfServer(
        id = "df-result-selection", 
        dataFrameRow = selectedResults
      )
      
      # keep a reactive variable tracking the active tab
      singleViewValue <- shiny::reactive({
        input$singleView
      })
      
      shiny::observeEvent(performance$performanceId(), {
        # hide validation tab if non internal val
        if(performance$modelDevelopment() == 1){ # how to get this
          shiny::showTab(inputId = "singleView", session = session, target = "Validation")
        } else{
          shiny::hideTab(inputId = "singleView", session = session, target = "Validation")
        }
      })
      
      #output$resultSelectText <- shiny::renderUI(
      #  getPlpResultSelection(
      #    connectionHandler = connectionHandler, 
      #    resultDatabaseSettings = resultDatabaseSettings,
      #    modelDesignId = modelDesignId,
      #    performanceId = performanceId
      #  )
      #)
      
      patientLevelPredictionCovariateSummaryServer(
        id = 'covariateSummary',
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        connectionHandler = connectionHandler,
        inputSingleView = singleViewValue,
        resultDatabaseSettings = resultDatabaseSettings
      ) 
      
      patientLevelPredictionSettingsServer(
        id = 'settings', 
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      patientLevelPredictionCutoffServer(
        id = 'cutoff', 
        performanceId = performanceId, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      patientLevelPredictionDiscriminationServer(
        id = 'discrimination', 
        performanceId = performanceId, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      patientLevelPredictionCalibrationServer(
        id = 'calibration', 
        performanceId = performanceId, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        resultDatabaseSettings = resultDatabaseSettings
      ) 
      
      patientLevelPredictionNbServer(
        id = 'netBenefit', 
        performanceId = performanceId, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        resultDatabaseSettings = resultDatabaseSettings
      ) 
      
      patientLevelPredictionValidationServer(
        id = 'validation', 
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        resultDatabaseSettings = resultDatabaseSettings
      ) 
      
    
      }
    )
  }



# name too generic
getPlpPerformanceSelection <- function(
    connectionHandler, 
    resultDatabaseSettings,
    performanceId
){
  
  if(!inherits(performanceId, 'reactive')){
    performanceId <- shiny::reactiveVal(performanceId)
  }
  
  if(!is.null(performanceId())){
    
    developmentDb = connectionHandler$queryDb(
      'select distinct d.cdm_source_abbreviation as development_db from 
    @schema.@database_table_prefixdatabase_meta_data d
    inner join
    @schema.@plp_table_prefixdatabase_details dd
    on dd.database_meta_data_id = d.database_id
    inner join
    @schema.@plp_table_prefixperformances p 
    on dd.database_id = p.development_database_id
    where p.performance_id = @performance_id;',
      schema = resultDatabaseSettings$schema,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
      performance_id = performanceId(),
      database_table_prefix = resultDatabaseSettings$databaseTablePrefix
    )
    
    validationDb = connectionHandler$queryDb(
      'select distinct d.cdm_source_abbreviation as validation_db from 
    @schema.@database_table_prefixdatabase_meta_data d
    inner join
    @schema.@plp_table_prefixdatabase_details dd
    on dd.database_meta_data_id = d.database_id
    inner join
    @schema.@plp_table_prefixperformances p 
    on dd.database_id = p.validation_database_id
    where p.performance_id = @performance_id;',
      schema = resultDatabaseSettings$schema,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
      performance_id = performanceId(),
      database_table_prefix = resultDatabaseSettings$databaseTablePrefix
    )
    
    target <- connectionHandler$queryDb(
      'select distinct targets.cohort_name as validation_target from 
    @schema.@plp_table_prefixperformances p
    inner join 
    @schema.@plp_table_prefixcohorts c
    on p.target_id = c.cohort_id
    inner join
    @schema.@cg_table_prefixcohort_definition AS targets
    on c.cohort_definition_id = targets.cohort_definition_id
    where p.performance_id = @performance_id;',
      schema = resultDatabaseSettings$schema,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      performance_id = performanceId()
    )
    outcome <- connectionHandler$queryDb(
      'select distinct targets.cohort_name as validation_outcome from 
    @schema.@plp_table_prefixperformances p
    inner join 
    @schema.@plp_table_prefixcohorts c
    on p.outcome_id = c.cohort_id
    inner join
    @schema.@cg_table_prefixcohort_definition AS targets
    on c.cohort_definition_id = targets.cohort_definition_id
    where p.performance_id = @performance_id;',
      schema = resultDatabaseSettings$schema,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      performance_id = performanceId()
    )
    
    tar <- connectionHandler$queryDb(
      'select distinct 
    tars.tar_start_day, 
    tars.tar_start_anchor,
    tars.tar_end_day,
    tars.tar_end_anchor
    from @schema.@plp_table_prefixperformances p 
    inner join 
    @schema.@plp_table_prefixtars AS tars
    on p.tar_id = tars.tar_id
    where p.performance_id = @performance_id;',
      schema = resultDatabaseSettings$schema,
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
      performance_id = performanceId()
    )
    # replace with editTar?
    tar <- paste0(
      '( ', tar$tarStartAnchor, ' + ', tar$tarStartDay, ' ) - ( ',
      tar$tarEndAnchor, ' + ', tar$tarEndDay, ' )'
    )
    
    return(
      data.frame(
        developmentDb = developmentDb,
        validationDb = validationDb,
        validationTarget = target,
        validationOutcome = outcome,
        validationTar = tar
      )
    )
  }
}