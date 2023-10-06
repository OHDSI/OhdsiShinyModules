getPredictionResult <- function(
    connectionHandler, 
    resultDatabaseSettings,
    tableName, 
    performanceId 
    ){
  
  # add prefix to tableName
  tableName <- paste0(resultDatabaseSettings$plpTablePrefix,tableName)
  
  shiny::withProgress(message = paste('Extracting PLP results from', tableName), value = 0, {

  shiny::incProgress(1/3, detail = paste("Translating and rendering "))

  sql <- "SELECT * FROM @schema.@table_name WHERE performance_id = @performance_id"

  shiny::incProgress(2/3, detail = paste("Extracting data "))
  
  result <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    table_name = tableName,
    performance_id = performanceId()
  )
  
  if('evaluation' %in% colnames(result)){
    result$evaluation <- trimws(result$evaluation)
  }
  
  shiny::incProgress(3/3, detail = paste("Finished "))
  
  })

  return(result)
}

getPredictionTar <- function(
    connectionHandler,
    resultDatabaseSettings,
    modelDesignId
    ){
  tar <- connectionHandler$queryDb(
    'select distinct 
    tars.tar_start_day, 
    tars.tar_start_anchor,
    tars.tar_end_day,
    tars.tar_end_anchor
    from @schema.@plp_table_prefixmodel_designs md 
    inner join 
    @schema.@plp_table_prefixtars AS tars
    on md.tar_id = tars.tar_id
    where md.model_design_id = @model_design_id;',
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    model_design_id = modelDesignId()
  )
  
  tar <- paste0(
    '( ', tar$tarStartAnchor, ' + ', tar$tarStartDay, ' ) - ( ',
    tar$tarEndAnchor, ' + ', tar$tarEndDay, ' )'
  )
  return(tar)
}
