# code to create test data

#testDir <- tempdir()

outputFolder <- file.path(getwd(), 'tests', 'resources', 'estDatabase')

library(CohortMethod)
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

tcos1 <- createTargetComparatorOutcomes(
  targetId = 1,
  comparatorId = 2,
  outcomes = list(
    createOutcome(
      outcomeId = 3,
      priorOutcomeLookback = 30
    ),
    createOutcome(
      outcomeId = 4,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  ),
  excludedCovariateConceptIds = c(1118084, 1124300)
)

tcos2 <- createTargetComparatorOutcomes(
  targetId = 1,
  comparatorId = 4,
  outcomes = list(
    createOutcome(
      outcomeId = 3,
      priorOutcomeLookback = 30
    )
  ),
  excludedCovariateConceptIds = c(1118084, 1124300)
)

targetComparatorOutcomesList <- list(tcos1, tcos2)

covarSettings <- createDefaultCovariateSettings(addDescendantsToExclude = TRUE)
covarSettings$endDays <- -1

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
  washoutPeriod = 183,
  firstExposureOnly = TRUE,
  removeDuplicateSubjects = "remove all",
  covariateSettings = covarSettings
)

# Duplicating some operations from createGetDbCohortMethodDataArgs just so we test them:
createStudyPopArgs1 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  firstExposureOnly = TRUE,
  restrictToCommonPeriod = TRUE,
  removeDuplicateSubjects = "remove all",
  washoutPeriod = 183,
  censorAtNewRiskWindow = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

createStudyPopArgs2 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  firstExposureOnly = TRUE,
  restrictToCommonPeriod = TRUE,
  removeDuplicateSubjects = "keep first",
  washoutPeriod = 183,
  censorAtNewRiskWindow = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox")

cmAnalysis1 <- createCmAnalysis(
  analysisId = 1,
  description = "No matching, simple outcome model",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs1,
  fitOutcomeModelArgs = fitOutcomeModelArgs1
)

createPsArgs <- createCreatePsArgs(
  prior = createPrior("laplace", variance = 0.01),
  estimator = "att"
)

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

# computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = 0:20 * 1000 + 3)
computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = FeatureExtraction::getDefaultTable1Specifications())

fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE
)

cmAnalysis2 <- createCmAnalysis(
  analysisId = 2,
  description = "Matching",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  computeCovariateBalanceArgs = computeCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs2
)

truncateIptwArgs <- createTruncateIptwArgs(maxWeight = 10)

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(
  modelType = "cox",
  inversePtWeighting = TRUE
)
cmAnalysis3 <- createCmAnalysis(
  analysisId = 3,
  description = "IPTW",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  truncateIptwArgs = truncateIptwArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs3
)

fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE,
  interactionCovariateIds = 8532001
)

cmAnalysis4 <- createCmAnalysis(
  analysisId = 4,
  description = "Matching with gender interaction",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs4
)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4)

#analysesToExclude <- data.frame(
#  targetId = c(998, 998),
#  analysisId = c(3, 4)
#)

# cmAnalysis4 includes interaction terms which should throw a warning

result <- runCmAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  outputFolder = outputFolder,
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList#,
  #analysesToExclude = analysesToExclude
)

CohortMethod::exportToCsv(outputFolder, databaseId = 'eunomia')

resultDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
    )
resultsDataModel <- ResultModelManager::loadResultsDataModelSpecifications(
  filePath = system.file("csv", "resultsDataModelSpecification.csv", package = "CohortMethod")
)
sql <- ResultModelManager::generateSqlSchema(
  schemaDefinition = resultsDataModel
)
sql <- SqlRender::render(
  sql = sql,
  database_schema = 'main'
)
connection <- DatabaseConnector::connect(
  connectionDetails = resultDetails
)
DatabaseConnector::executeSql(
  connection = connection,
  sql = sql
)

# create tables

# upload results
ResultModelManager::uploadResults(
  connectionDetails = resultDetails, 
  schema = 'main', 
  resultsFolder = '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/cm_export', 
  tablePrefix = '', 
  specifications = resultsDataModel, 
  purgeSiteDataBeforeUploading = F
  )

DatabaseConnector::insertTable(
  connection = connection, 
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
  connection = connection, 
  databaseSchema = 'main', 
  tableName = 'database_meta_data', 
  data = data.frame(
    databaseId  = 'eunomia',
    cdmSourceName = 'eunomia',
    cdmSourceAbbreviation = 'eunomia'
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = T
)

DatabaseConnector::disconnect(connection)



# NOW SCCS
library(SelfControlledCaseSeries)

sccsAnalysisList <- list(
  SelfControlledCaseSeries::createSccsAnalysis(
    analysisId = 1, 
    description = 'test 1', 
    getDbSccsDataArgs = SelfControlledCaseSeries::createGetDbSccsDataArgs(
      maxCasesPerOutcome = 0,
      exposureIds = c("exposureId1", "exposureId2", "exposureId4")
    ), 
    createStudyPopulationArgs = SelfControlledCaseSeries::createCreateStudyPopulationArgs(
      firstOutcomeOnly = T, 
      naivePeriod = 90, 
      minAge = 18
      ), 
    createIntervalDataArgs = SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
      eraCovariateSettings = SelfControlledCaseSeries::createEraCovariateSettings(
        label = "Exposure of interest",
        includeEraIds = "exposureId1",
        start = 1,
        end = 0,
        endAnchor = "era end",
        profileLikelihood = TRUE,
        exposureOfInterest = TRUE
      ), 
      ageCovariateSettings = SelfControlledCaseSeries::createAgeCovariateSettings(), 
      seasonalityCovariateSettings = SelfControlledCaseSeries::createSeasonalityCovariateSettings(), 
      calendarTimeCovariateSettings = SelfControlledCaseSeries::createCalendarTimeCovariateSettings()
        ), 
    fitSccsModelArgs = SelfControlledCaseSeries::createFitSccsModelArgs()
  ),
  SelfControlledCaseSeries::createSccsAnalysis(
    analysisId = 2, 
    description = 'test 2', 
    getDbSccsDataArgs = SelfControlledCaseSeries::createGetDbSccsDataArgs(
      maxCasesPerOutcome = 0,
      exposureIds = c("exposureId1")
    ),
    createStudyPopulationArgs = SelfControlledCaseSeries::createCreateStudyPopulationArgs(
      firstOutcomeOnly = T, 
      naivePeriod = 90, 
      minAge = 18
    ), 
    createIntervalDataArgs = SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
      eraCovariateSettings = SelfControlledCaseSeries::createEraCovariateSettings(
        label = "Exposure of interest",
        includeEraIds = "exposureId1",
        start = 1,
        end = 0,
        endAnchor = "era end",
        profileLikelihood = TRUE,
        exposureOfInterest = TRUE
      ), 
      ageCovariateSettings = SelfControlledCaseSeries::createAgeCovariateSettings(), 
      seasonalityCovariateSettings = SelfControlledCaseSeries::createSeasonalityCovariateSettings(), 
      calendarTimeCovariateSettings = SelfControlledCaseSeries::createCalendarTimeCovariateSettings()
    ),
    fitSccsModelArgs = SelfControlledCaseSeries::createFitSccsModelArgs()
  )
)
  
exposuresOutcomeList <- list(
  SelfControlledCaseSeries::createExposuresOutcome(
    outcomeId = 3, 
    exposures = list(
      SelfControlledCaseSeries::createExposure(
      exposureId = 1, exposureIdRef = 'exposureId1',
      trueEffectSize = NA
      ),
      SelfControlledCaseSeries::createExposure(
        exposureId = 2, exposureIdRef = 'exposureId2',
        trueEffectSize = 1
      ),
      SelfControlledCaseSeries::createExposure(
        exposureId = 4, exposureIdRef = 'exposureId4', 
        trueEffectSize = 1
      )
    )
))


SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = 'main', 
  exposureDatabaseSchema = 'main', 
  exposureTable = 'cohort', 
  outcomeDatabaseSchema = 'main', 
  outcomeTable = 'cohort',
  outputFolder = file.path(outputFolder, 'sccs'), 
  sccsAnalysisList = sccsAnalysisList, 
  exposuresOutcomeList = exposuresOutcomeList
    )

SelfControlledCaseSeries::exportToCsv(
  outputFolder = file.path(outputFolder, 'sccs'), 
  exportFolder = file.path(outputFolder, 'sccs_export'), 
  databaseId = 'eunomia'
    )

resultDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
)
resultsDataModel <- ResultModelManager::loadResultsDataModelSpecifications(
  filePath = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries")
)
sql <- ResultModelManager::generateSqlSchema(
  schemaDefinition = resultsDataModel
)
sql <- SqlRender::render(
  sql = sql,
  database_schema = 'main'
)
connection <- DatabaseConnector::connect(
  connectionDetails = resultDetails
)
DatabaseConnector::executeSql(
  connection = connection,
  sql = sql
)

# create tables

# upload results
ResultModelManager::uploadResults(
  connectionDetails = resultDetails, 
  schema = 'main', 
  resultsFolder = '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/sccs_export', 
  tablePrefix = '', 
  specifications = resultsDataModel, 
  purgeSiteDataBeforeUploading = F
)

DatabaseConnector::disconnect(connection)

# NOW ES

resultDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
)
source('https://raw.github.com/OHDSI/EvidenceSynthesisModule/main/EvidenceSynthesisFunctions.R')

createEvidenceSynthesisSource <- function(sourceMethod = "CohortMethod",
                                          databaseIds = NULL,
                                          analysisIds = NULL,
                                          likelihoodApproximation = "adaptive grid") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertChoice(sourceMethod, c("CohortMethod", "SelfControlledCaseSeries"), add = errorMessages)
  if (is.character(databaseIds)) {
    checkmate::assertCharacter(databaseIds, null.ok = TRUE, add = errorMessages)
  } else {
    checkmate::assertIntegerish(databaseIds, null.ok = TRUE, add = errorMessages)
  }
  checkmate::assertIntegerish(analysisIds, null.ok = TRUE, add = errorMessages)
  checkmate::assertChoice(likelihoodApproximation, c("adaptive grid", "normal"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  
  analysis <- list()
  for (name in names(formals(createEvidenceSynthesisSource))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "EvidenceSynthesisSource"
  return(analysis)
}
createBayesianMetaAnalysis <- function(chainLength = 1100000,
                                       burnIn = 1e+05,
                                       subSampleFrequency = 100,
                                       priorSd = c(2, 0.5),
                                       alpha = 0.05,
                                       robust = FALSE,
                                       df = 4,
                                       seed = 1,
                                       evidenceSynthesisAnalysisId = 1,
                                       evidenceSynthesisDescription = "Bayesian random-effects",
                                       evidenceSynthesisSource = NULL,
                                       controlType = "outcome") {
  analysis <- list()
  for (name in names(formals(createBayesianMetaAnalysis))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- c("BayesianMetaAnalysis", "EvidenceSynthesisAnalysis")
  return(analysis)
}
createEsDiagnosticThresholds <- function(mdrrThreshold = 10,
                                         easeThreshold = 0.25,
                                         i2Threshold = 0.4,
                                         tauThreshold = log(2)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(mdrrThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::assertNumeric(easeThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::assertNumeric(i2Threshold, len = 1, lower = 0, add = errorMessages)
  checkmate::assertNumeric(tauThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  thresholds <- list(
    mdrrThreshold = mdrrThreshold,
    easeThreshold = easeThreshold,
    i2Threshold = i2Threshold,
    tauThreshold = tauThreshold
  )
  class(thresholds) <- "EsDiagnosticThresholds"
  return(thresholds)
}

writeAnalysisSpecs(
  analysisSpecs = list(
    createBayesianMetaAnalysis(
      evidenceSynthesisSource = createEvidenceSynthesisSource(), 
      evidenceSynthesisAnalysisId = 1
  ),
  createBayesianMetaAnalysis(
    evidenceSynthesisSource = createEvidenceSynthesisSource(
      sourceMethod = "SelfControlledCaseSeries"
      ), evidenceSynthesisAnalysisId = 2
  )
  ),
  resultsFolder = file.path(outputFolder, 'es')
)
# Note: need to download https://github.com/OHDSI/EvidenceSynthesisModule/blob/main/resultsDataModelSpecification.csv
executeEvidenceSynthesis(
  connectionDetails = resultDetails,
  databaseSchema = 'main',
  settings = list(
    createBayesianMetaAnalysis(
      evidenceSynthesisSource = createEvidenceSynthesisSource(), 
      evidenceSynthesisAnalysisId = 1
    ),
    createBayesianMetaAnalysis(
      evidenceSynthesisSource = createEvidenceSynthesisSource(
        sourceMethod = "SelfControlledCaseSeries"
      ), evidenceSynthesisAnalysisId = 2
    )
  ), 
  esDiagnosticThresholds = createEsDiagnosticThresholds(),
  resultsFolder = file.path(outputFolder, 'es'),
  minCellCount = 5
)

# upload ES
resultDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
)
resultsDataModel <- read.csv('resultsDataModelSpecification.csv')
colnames(resultsDataModel) <- SqlRender::snakeCaseToCamelCase(colnames(resultsDataModel))
sql <- ResultModelManager::generateSqlSchema(
  schemaDefinition = resultsDataModel
)
sql <- SqlRender::render(
  sql = sql,
  database_schema = 'main'
)
connection <- DatabaseConnector::connect(
  connectionDetails = resultDetails
)
DatabaseConnector::executeSql(
  connection = connection,
  sql = sql
)

# upload results
ResultModelManager::uploadResults(
  connectionDetails = resultDetails, 
  schema = 'main', 
  resultsFolder = '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/es', 
  tablePrefix = '', 
  specifications = resultsDataModel, 
  purgeSiteDataBeforeUploading = F
)

DatabaseConnector::disconnect(connection)

