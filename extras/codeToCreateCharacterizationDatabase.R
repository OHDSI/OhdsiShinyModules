# code to create test data

testDir <- tempdir()
testDir <- '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources'

library(Characterization)
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

charSet <- Characterization::createCharacterizationSettings(
  timeToEventSettings = Characterization::createTimeToEventSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds
  ) , 
  dechallengeRechallengeSettings = Characterization::createDechallengeRechallengeSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds
  ), 
  aggregateCovariateSettings = Characterization::createAggregateCovariateSettings(
    targetIds = targetIds, minPriorObservation = 365, outcomeWashoutDays = 90,
    outcomeIds = outcomeIds, 
    riskWindowStart = 1, riskWindowEnd = 365, 
    covariateSettings = covSet
  )
)

Characterization::runCharacterizationAnalyses(
  connectionDetails = connectionDetails, 
  targetDatabaseSchema = "main", 
  targetTable = "cohort", 
  outcomeDatabaseSchema = "main", 
  outcomeTable = "cohort", 
  cdmDatabaseSchema = "main", 
  characterizationSettings = charSet, incremental = F,
  executionPath = file.path(testDir,'charDatabase','execution'),
  outputDirectory =  file.path(testDir,'charDatabase', 'results'), 
  databaseId = 'eunomia', 
  csvFilePrefix =  'c_'
  )

#serverDesc <- file.path(getwd(),
#                        'inst/extdata/results.sqlite')
serverDesc <- "tests/resources/charDatabase/databaseFile.sqlite"
connectionDetailsDesc <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverDesc
)

if(F){
for(table in c('c_time_to_event', 'c_dechallenge_rechallenge',
               'c_analysis_ref', 'c_covariate_ref', 'c_covariates',
               'c_covariates_continuous', 'c_settings', 
               'c_cohort_details', 'c_cohort_counts')){
  sql <- "UPDATE main.@tbl SET database_id = '85642205';"
  sql <- SqlRender::render(sql, tbl = table)
  DatabaseConnector::executeSql(con, sql)
}
}

Characterization::createCharacterizationTables(
  connectionDetails = connectionDetailsDesc, 
  resultSchema = 'main', 
  createTables = T, 
  deleteExistingTables = T
  )

Characterization::insertResultsToDatabase(
  connectionDetails = connectionDetailsDesc, 
  schema = 'main', 
  resultsFolder = file.path(testDir,'charDatabase', 'results'), 
  csvTablePrefix =  'c_'
  )

if(F){
# add in rhe database_meta_data and cohort_definitions tables

serverDesc <- "tests/resources/cDatabase/databaseFile.sqlite"
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
    #,isSubset = rep(0, 4)
    #,subsetParent = rep(NULL, 4)
    #,subsetDefinitionId = rep(NULL, 4)
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

}


# adding cohort incidence
##remotes::install_github('ohdsi/CohortIncidence')
serverDesc <- "tests/resources/cDatabase/databaseFile.sqlite"
connectionDetailsDesc <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverDesc
)
con <- DatabaseConnector::connect(connectionDetails = connectionDetailsDesc)

t1 <- CohortIncidence::createCohortRef(id=1, name="Target cohort 1")

o1 <- CohortIncidence::createOutcomeDef(id=1,name="Outcome 3, 30d Clean", 
                                        cohortId =3,
                                        cleanWindow =30)

tar1 <- CohortIncidence::createTimeAtRiskDef(id=1, 
                                             startWith="start", 
                                             endWith="end", 
                                             endOffset=30)

# Note: c() is used when dealing with an array of numbers, 
# later we use list() when dealing with an array of objects
analysis1 <- CohortIncidence::createIncidenceAnalysis(targets = c(t1$id),
                                                      outcomes = c(o1$id),
                                                      tars = c(tar1$id))

subgroup1 <- CohortIncidence::createCohortSubgroup(
  id=1, 
  name="Subgroup 1", 
  cohortRef = CohortIncidence::createCohortRef(id=300)
  )


# Create Design (note use of list() here):
irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = list(t1),
                                                   outcomeDefs = list(o1),
                                                   tars=list(tar1),
                                                   analysisList = list(analysis1),
                                                   subgroups = list(subgroup1))

buildOptions <- CohortIncidence::buildOptions(cohortTable = "main.cohort",
                                              cdmDatabaseSchema = "main",
                                              sourceName = "eunomia",
                                              refId = 1)


executeResults <- CohortIncidence::executeAnalysis(connectionDetails = connectionDetails,
                                                   incidenceDesign = irDesign,
                                                   buildOptions = buildOptions)

# adding database
#executeResults$incidence_summary$database_id <- '85642205'
executeResults$incidence_summary$database_id <- 'eunomia'
# insert results
DatabaseConnector::insertTable(
  connection = con, 
  databaseSchema = 'main', 
  tableName = 'ci_incidence_summary', 
  data = executeResults$incidence_summary
  )
DatabaseConnector::insertTable(
  connection = con, 
  databaseSchema = 'main', 
  tableName = 'ci_target_def', 
  data = executeResults$target_def
)
DatabaseConnector::insertTable(
  connection = con, 
  databaseSchema = 'main', 
  tableName = 'ci_outcome_def', 
  data = executeResults$outcome_def
)
DatabaseConnector::insertTable(
  connection = con, 
  databaseSchema = 'main', 
  tableName = 'ci_tar_def', 
  data = executeResults$tar_def
)
DatabaseConnector::insertTable(
  connection = con, 
  databaseSchema = 'main', 
  tableName = 'ci_age_group_def', 
  data = executeResults$age_group_def
)
DatabaseConnector::insertTable(
  connection = con, 
  databaseSchema = 'main', 
  tableName = 'ci_subgroup_def', 
  data = executeResults$subgroup_def
)

