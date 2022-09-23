# code to create test data

testDir <- tempdir()
testDir <- '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources'

library(DescriptiveStudies)
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

targetIds <- c(1,2,4)
outcomeIds <- 3

covSet <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                     useDemographicsAge = T, 
                                                     useDemographicsRace = T,
                                                     useDemographicsEthnicity = T, 
                                                     useDemographicsAgeGroup = T,
                                                     useConditionGroupEraLongTerm = T, 
                                                     useDrugEraStartLongTerm  = T, 
                                                     endDays = -1
)

descSet <- DescriptiveStudies::createCharacterizationSettings(
  timeToEventSettings = DescriptiveStudies::createTimeToEventSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds
  ) , 
  dechallengeRechallengeSettings = DescriptiveStudies::createDechallengeRechallengeSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds
  ), 
  aggregateCovariateSettings = DescriptiveStudies::createAggregateCovariateSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds, 
    riskWindowStart = 1, riskWindowEnd = 365, 
    covariateSettings = covSet
  )
)

DescriptiveStudies::runCharacterizationAnalyses(
  connectionDetails = connectionDetails, 
  targetDatabaseSchema = "main", 
  targetTable = "cohort", 
  outcomeDatabaseSchema = "main", 
  outcomeTable = "cohort", 
  cdmDatabaseSchema = "main", 
  characterizationSettings = descSet, 
  saveDirectory = file.path(testDir,'descDatabase'), 
  databaseId = 'eunomia', 
  tablePrefix = 'c_'
  )


if(F){
# add in rhe database_meta_data and cohort_definitions tables

serverDesc <- "tests/resources/descDatabase/databaseFile.sqlite"
connectionDetailsDesc <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverDesc
)
connectionDesc <- DatabaseConnector::connect(
  connectionDetails = connectionDetailsDesc, 
  dbms = 'sqlite', 
  user = NULL, 
  password = NULL, 
  server = serverDesc,
  port = NULL
)

DatabaseConnector::insertTable(
  connection = connectionDesc, 
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

DatabaseConnector::insertTable(
  connection = connectionDesc, 
  databaseSchema = 'main', 
  tableName = 'database_meta_data', 
  data = data.frame(
    databaseId  = 'eunomia',
    cdmSourceName = 'eunomia',
    cdmSourceAbbreviation = 'eunomia'
  ), 
  createTable = T, 
  camelCaseToSnakeCase = T
)


# add results to c_DECHALLENGE_RECHALLENGE as no repeats so can't test
DatabaseConnector::insertTable(
  connection = connectionDesc, 
  databaseSchema = 'main', 
  tableName = 'c_DECHALLENGE_RECHALLENGE', 
data.frame(
  DATABASE_ID = 'eunomia',
  DECHALLENGE_STOP_INTERVAL = 30,
  DECHALLENGE_EVALUATION_WINDOW = 30,
  TARGET_COHORT_DEFINITION_ID = 1,
  OUTCOME_COHORT_DEFINITION_ID = 3,
  NUM_EXPOSURE_ERAS = 10,
  NUM_PERSONS_EXPOSED = 10,
  NUM_CASES = 4,
  DECHALLENGE_ATTEMPT = 3,
  DECHALLENGE_FAIL = 2,
  DECHALLENGE_SUCCESS = 1,
  RECHALLENGE_ATTEMPT = 1,
  RECHALLENGE_FAIL = 0,
  RECHALLENGE_SUCCESS = 1,
  PCT_DECHALLENGE_ATTEMPT = 0.4,
  PCT_DECHALLENGE_SUCCESS  = 0.4,
  PCT_DECHALLENGE_FAIL = 0.4,
  PCT_RECHALLENGE_ATTEMPT = 0.4,
  PCT_RECHALLENGE_SUCCESS = 0.4,
  PCT_RECHALLENGE_FAIL = 0.4
),
createTable = T, 
camelCaseToSnakeCase = F
)


# add results to i_INCIDENCE_SUMMARY
DatabaseConnector::insertTable(
  connection = connectionDesc, 
  databaseSchema = 'main', 
  tableName = 'i_INCIDENCE_SUMMARY', 
  data.frame(
    ref_id = 1,
    DATABASE_ID = 'eunomia',
    source_name = '',
    target_cohort_definition_id = 1,
    target_name = 'target 1',
    tar_id = 1,
    tar_start_with = 'start',
    tar_start_offset = 0,
    tar_end_with = 'end',
    tar_end_offset = 0,
    subgroup_id = 1,
    subgroup_name = '',
    outcome_id = 3,
    outcome_cohort_definition_id = 3,
    outcome_name = 'outcome 3',
    clean_window = 0,
    age_id = 1,
    age_group_name = '',
    gender_id = 1,
    gender_name = '',
    start_year = 1,
    persons_at_risk_pe = 1,
    persons_at_risk = 1,
    person_days_pe = 1,
    person_days = 1,
    person_outcomes_pe = 1,
    person_outcomes = 1,
    outcomes_pe = 1,
    outcomes = 1,
    incidence_proportion_p100p = 0.1,
    incidence_rate_p100py = 0.1
  ),
  createTable = T, 
  camelCaseToSnakeCase = F
)


}
