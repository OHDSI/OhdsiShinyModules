# code to create test data

testDir <- tempdir()
testDir <- '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources'

serverCG <- "tests/resources/cgDatabase/databaseFile.sqlite"
connectionDetailsCG <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverCG
)
connectionCG <- DatabaseConnector::connect(
  connectionDetails = connectionDetailsCG, 
  dbms = 'sqlite', 
  user = NULL, 
  password = NULL, 
  server = serverCG,
  port = NULL
)

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_COUNT', 
  data = data.frame(
    database_id = 1, 
    cohort_id = 1, 
    cohort_entries = 200, 
    cohort_subjects = 100
    ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
  )

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_GENERATION', 
  data = data.frame(
    database_id = 1,
    cohort_id = 1,
    cohort_name = 'test',
    generation_status = 1,
    start_time = '20100101',
    end_time = '20100101',
    made_up_colunm = 1
  ), 
  createTable = T, dropTableIfExists = T, 
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_SUMMARY_STATS', 
  data = data.frame(
    database_id = 1, 
    cohort_definition_id =1,
    made_up_colunm = 1,
    base_count = 1000, 
    final_count = 1000, 
    mode_id = 1
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_INCLUSION', 
  data = data.frame(
    database_id = 1, 
    cohort_definition_id = 1, 
    rule_sequence = 1, 
    name = 'test'
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_INC_RESULT', 
  data = data.frame(
    database_id = 1, 
    cohort_definition_id = 1, 
    inclusion_rule_mask = 1, 
    person_count = 1000, 
    mode_id = 1
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)


DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'database_meta_data', 
  data = data.frame(
    databaseId  = '1',
    cdmSourceName = 'eunomia',
    cdmSourceAbbreviation = 'eunomia'
  ), 
  createTable = T, 
  camelCaseToSnakeCase = T
)

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'cg_cohort_definition', 
  data = data.frame(
    cohortDefinitionId = c(1,2,3,4),
    cohortName = c('target 1 example','target 2 example','outcome example','target 4 example'),
    description = rep('',4),
    json = rep('{}', 4),
    sqlCommand = rep('',4)
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = T
)
