# code to create test data

testDir <- tempdir()

sccs_server <- '/Users/jreps/Documents/GitHub/OhdsiShinyModules/tests/resources/sccsDatabase/databaseFile.sqlite'

server <- "/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/esDatabase/databaseFile.sqlite"
results <- '/Users/jreps/Documents/fakeSccs'

# create sqlite and insert each table:
tables <- dir(file.path(results), pattern = '.csv')
resultDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = server #"/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
    )
sqliteCon <- DatabaseConnector::connect(connectionDetails = resultDetails)
for(table in tables){
  DatabaseConnector::insertTable(
    connection = sqliteCon,
    databaseSchema = 'main', 
    tableName = gsub('.csv','',table), 
    data = utils::read.csv(file.path(results, table)), 
    createTable = T, 
    dropTableIfExists = T, 
    camelCaseToSnakeCase = F
      )
}

# copy over tables from cm
cm_server <- "/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
cmResultDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = cm_server #"/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
)
cmSqliteCon <- DatabaseConnector::connect(connectionDetails = cmResultDetails)
tables <- DatabaseConnector::getTableNames(cmSqliteCon, 'main')
for(table in tables){
  data <- DatabaseConnector::querySql(
    cmSqliteCon,
    paste0('select * from main.',table,';')
  )
  DatabaseConnector::insertTable(
    connection = sqliteCon,
    databaseSchema = 'main', 
    tableName = table, 
    data = data, 
    createTable = T, 
    dropTableIfExists = T, 
    camelCaseToSnakeCase = F
  )
}

DatabaseConnector::disconnect(cmSqliteCon)

DatabaseConnector::getTableNames(sqliteCon, 'main')

cmres <- DatabaseConnector::querySql(sqliteCon, 'select * from main.cm_result;')

# Now add the evidence synth tables

es_analysis <- data.frame(
  evidence_synthesis_analysis_id = c(1,1),
  evidence_synthesis_description	= rep("Random-effects - alpha 0.05",2),
  source_method	= c('CohortMethod', 'SelfControlledCaseSeries'),
  definition = c('','')
)

DatabaseConnector::insertTable(
  connection = sqliteCon,
  databaseSchema = 'main', 
  tableName = 'es_analysis', 
  data = es_analysis, 
  createTable = T, 
  dropTableIfExists = T, 
  camelCaseToSnakeCase = F
)

es_cm_diagnostics_summary <- data.frame(
  target_id	= 1,
  comparator_id	= 2,
  outcome_id	= 3,
  analysis_id	= 1,
  evidence_synthesis_analysis_id = 1,	
  i_2	= 0,
  tau	= NA,
  ease = 0.2,
  i_2_diagnostic = 'PASS',	
  tau_diagnostic = 'PASS',
  ease_diagnostic	= 'WARNING',
  unblind = 1
)

DatabaseConnector::insertTable(
  connection = sqliteCon,
  databaseSchema = 'main', 
  tableName = 'es_cm_diagnostics_summary', 
  data = es_cm_diagnostics_summary, 
  createTable = T, 
  dropTableIfExists = T, 
  camelCaseToSnakeCase = F
)

es_cm_result <- data.frame(
  target_id	= 1,
  comparator_id	= 2,
  outcome_id	= 3,
  analysis_id	= 1,
  evidence_synthesis_analysis_id =1,	
  rr = 2,	
  ci_95_lb = 1,	
  ci_95_ub = 2.2,	
  p = 0.5,
  log_rr = 0.2,
  se_log_rr = 0.3,	
  target_subjects = 100,	
  comparator_subjects = 100,	
  target_days = 10000,	
  comparator_days = 10000,	
  target_outcomes = 50,	
  comparator_outcomes = 25,
  n_databases = 3,	
  calibrated_rr	= 1.6,
  calibrated_ci_95_lb	= 1,
  calibrated_ci_95_ub	= 1.8,
  calibrated_p	= 0.5,
  calibrated_log_rr	= 0.2,
  calibrated_se_log_rr = 0.3
)

DatabaseConnector::insertTable(
  connection = sqliteCon,
  databaseSchema = 'main', 
  tableName = 'es_cm_result', 
  data = es_cm_result, 
  createTable = T, 
  dropTableIfExists = T, 
  camelCaseToSnakeCase = F
)

es_sccs_result <- data.frame(
  analysis_id = 13,	
  exposures_outcome_set_id = 1,	
  covariate_id = 1001,	
  evidence_synthesis_analysis_id = 1,
  rr = 1,
  ci_95_lb = 0.5,
  ci_95_ub = 1.5,
  p = 0.8,	
  outcome_subjects = 10000,	
  outcome_events = 100,	
  outcome_observation_periods = 100000,	
  covariate_subjects = 1000,	
  covariate_days = 10,
  covariate_eras = 10,	
  covariate_outcomes = 10,	
  observed_days = 100000,	
  n_databases = 1,	
  log_rr = 0,	
  se_log_rr = 0.4,
  calibrated_rr = 1,
  calibrated_ci_95_lb = 0.5,	
  calibrated_ci_95_ub = 1.5,
  calibrated_p = 0.8,	
  calibrated_log_rr = 0,	
  calibrated_se_log_rr = 0.4
)

DatabaseConnector::insertTable(
  connection = sqliteCon,
  databaseSchema = 'main', 
  tableName = 'es_sccs_result', 
  data = es_sccs_result, 
  createTable = T, 
  dropTableIfExists = T, 
  camelCaseToSnakeCase = F
)

es_sccs_diagnostics_summary <- data.frame(
  exposures_outcome_set_id = 1,
  covariate_id = 1001,	
  analysis_id = 13,	
  evidence_synthesis_analysis_id = 1,
  i_2 = 0,
  tau = NA,	
  ease = 01,	
  i_2_diagnostic = "PASS",	
  tau_diagnostic = "PASS",	
  ease_diagnostic	= "WARNING",
  unblind = 1
)

DatabaseConnector::insertTable(
  connection = sqliteCon,
  databaseSchema = 'main', 
  tableName = 'es_sccs_diagnostics_summary', 
  data = es_sccs_diagnostics_summary, 
  createTable = T, 
  dropTableIfExists = T, 
  camelCaseToSnakeCase = F
)
