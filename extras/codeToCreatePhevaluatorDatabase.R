# #load specification
# 
#  mypath <- "D:/shiny/resultModelSpecs"
#  serverPV <- "tests/resources/pvDatabase/phevaluator.sqlite"
#  
#  #delete entire database
#  unlink(serverPV)
#  
#  #load tje results data model spec
#  specification <- ResultModelManager::loadResultsDataModelSpecifications(file.path(mypath,
#                                                                                    "resultsDataModelSpecificationReordered.csv")
#  )
#  
#  #create sql to generate schema
#  sql <- ResultModelManager::generateSqlSchema(schemaDefinition = specification)
#  
#  #create the schema
#  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite",
#                                                                  server = serverPV)
#  
#  
#  # #need to re run the below qns when re-making the database
#  qns <- ResultModelManager::createQueryNamespace(connectionDetails = connectionDetails,
#                                                  tableSpecification = specification,
#                                                  tablePrefix = "",
#                                                  database_schema = "main")
#  #execute the SQL
#  qns$executeSql(sql)
#  
#  
#  #upload the results to the schema
#  ResultModelManager::uploadResults(connectionDetails = connectionDetails,
#                                    schema = "main",
#                                    resultsFolder = file.path(mypath, "results"),
#                                    tablePrefix = "",
#                                    specifications = specification,
#                                    purgeSiteDataBeforeUploading = F)
# 
# #DBI::dbConnect(RSQLite::SQLite(), dbname = "phevaluator.sqlite")
# 
# #create connection to DB
# connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
# 
# #disconnect
# DatabaseConnector::disconnect(connection)

 
 
 
 
 
 
# code to manually create test data

resultsPV <- "D:/shiny/resultModelSpecs/results"
serverPV <- "tests/resources/pvDatabase/phevaluator.sqlite"
connectionDetailsPV <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverPV
)
connectionPV <- DatabaseConnector::connect(
  connectionDetails = connectionDetailsPV,
  dbms = 'sqlite',
  user = NULL,
  password = NULL,
  server = serverPV,
  port = NULL
)
# 
DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_ALGORITHM_PERFORMANCE_RESULTS',
  data = read.csv(file.path(resultsPV, "pv_algorithm_performance_results.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_COHORT_DEFINITION_SET',
  data = read.csv(file.path(resultsPV, "pv_cohort_definition_set.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_DIAGNOSTICS',
  data = read.csv(file.path(resultsPV, "pv_diagnostics.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_EVALUATION_INPUT_PARAMETERS',
  data = read.csv(file.path(resultsPV, "pv_evaluation_input_parameters.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_MODEL_COVARIATE_SUMMARY',
  data = read.csv(file.path(resultsPV, "pv_model_covariate_summary.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_MODEL_COVARIATES',
  data = read.csv(file.path(resultsPV, "pv_model_covariates.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_MODEL_INPUT_PARAMETERS',
  data = read.csv(file.path(resultsPV, "pv_model_input_parameters.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_MODEL_PERFORMANCE',
  data = read.csv(file.path(resultsPV, "pv_model_performance.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_MODEL_RUN_TIME_VALUES',
  data = read.csv(file.path(resultsPV, "pv_model_run_time_values.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_TEST_SUBJECTS',
  data = read.csv(file.path(resultsPV, "pv_test_subjects.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionPV,
  databaseSchema = 'main',
  tableName = 'PV_TEST_SUBJECTS_COVARIATES',
  data = read.csv(file.path(resultsPV, "pv_test_subjects_covariates.csv")),
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)












