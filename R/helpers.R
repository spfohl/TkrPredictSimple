# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of oxfordKneeValidation
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

#' Create the exposure and outcome cohorts
#'
#' @details
#' This function will create the exposure and outcome cohorts following the definitions included in
#' this package.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the exposure and outcome cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#'
#' @export
createCohorts <- function(connectionDetails,
                          cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortTable = "cohort",
                          oracleTempSchema,
                          outputFolder) {
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  conn <- DatabaseConnector::connect(connectionDetails)

  .createCohorts(connection = conn,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 cohortTable = cohortTable,
                 oracleTempSchema = oracleTempSchema,
                 outputFolder = outputFolder)

  # Check number of subjects per cohort:
  ParallelLogger::logInfo("Counting cohorts")
  sql <- SqlRender::loadRenderTranslateSql("GetCounts.sql",
                                           "oxfordKneeValidation",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           work_database_schema = cohortDatabaseSchema,
                                           study_cohort_table = cohortTable)
  counts <- DatabaseConnector::querySql(conn, sql)
  colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
  counts <- addCohortNames(counts)
  utils::write.csv(counts, file.path(outputFolder, "CohortCounts.csv"), row.names = FALSE)

  DatabaseConnector::disconnect(conn)
}

addCohortNames <- function(data, IdColumnName = "cohortDefinitionId", nameColumnName = "cohortName") {
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "oxfordKneeValidation")
  cohortsToCreate <- utils::read.csv(pathToCsv)

  idToName <- data.frame(cohortId = c(cohortsToCreate$cohortId),
                         cohortName = c(as.character(cohortsToCreate$name)))
  idToName <- idToName[order(idToName$cohortId), ]
  idToName <- idToName[!duplicated(idToName$cohortId), ]
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  # Change order of columns:
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data) , (idCol+1):(ncol(data)-1))]
  }
  return(data)
}

.createCohorts <- function(connection,
                           cdmDatabaseSchema,
                           vocabularyDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           oracleTempSchema,
                           outputFolder) {

  # Create study cohort table structure:
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                           packageName = "oxfordKneeValidation",
                                           dbms = attr(connection, "dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)



  # Instantiate cohorts:
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "oxfordKneeValidation")
  cohortsToCreate <- utils::read.csv(pathToCsv)
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Creating cohort:", cohortsToCreate$name[i]))
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(cohortsToCreate$name[i], ".sql"),
                                             packageName = "oxfordKneeValidation",
                                             dbms = attr(connection, "dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabularyDatabaseSchema,

                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             target_cohort_id = cohortsToCreate$cohortId[i])
    DatabaseConnector::executeSql(connection, sql)
  }
}


#' Creates the target population and outcome summary characteristics
#'
#' @details
#' This will create the patient characteristic table
#'
#' @param connectionDetails The connections details for connecting to the CDM
#' @param cdmDatabaseSchema  The schema holding the CDM data
#' @param cohortDatabaseSchema The schema holding the cohort table
#' @param cohortTable         The name of the cohort table
#' @param targetId          The cohort definition id of the target population
#' @param outcomeId         The cohort definition id of the outcome
#' @param tempCohortTable   The name of the temporary table used to hold the cohort
#'
#' @return
#' A dataframe with the characteristics
#'
#' @export
getTable1 <- function(connectionDetails,
                      cdmDatabaseSchema,
                      cohortDatabaseSchema,
                      cohortTable,
                      targetId,
                      outcomeId,
                      tempCohortTable='#temp_cohort'){

  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T)

  plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                cohortId = targetId, outcomeIds = outcomeId,
                                                cohortDatabaseSchema = cohortDatabaseSchema,
                                                outcomeDatabaseSchema = cohortDatabaseSchema,
                                                cohortTable = cohortTable,
                                                outcomeTable = cohortTable,
                                                covariateSettings=covariateSettings)

  population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                              outcomeId = outcomeId,
                                                              binary = T,
                                                              includeAllOutcomes = T,
                                                              requireTimeAtRisk = T,
                                                              minTimeAtRisk = 364,
                                                              riskWindowStart = 1,
                                                              riskWindowEnd = 365,
                                                              removeSubjectsWithPriorOutcome = T)

  table1 <- PatientLevelPrediction::getPlpTable(cdmDatabaseSchema = cdmDatabaseSchema,
                                                longTermStartDays = -9999,
                                                population=population,
                                                connectionDetails=connectionDetails,
                                                cohortTable=tempCohortTable)

  return(table1)
}

#==========================
#  Example of implementing an exisitng model in the PredictionComparison repository
#==========================

#' Checks the plp package is installed sufficiently for the network study and does other checks if needed
#'
#' @details
#' This will check that the network study dependancies work
#'
#' @param connectionDetails The connections details for connecting to the CDM
#'
#' @return
#' A number (a value other than 1 means an issue with the install)
#'
#' @export

checkInstall <- function(connectionDetails=NULL){
  result <- PatientLevelPrediction::checkPlpInstallation(connectionDetails=connectionDetails,
                                 python=F)
  return(result)
}


#' Transport trained PLP models into the validation package
#'
#' @details
#' This will tranport PLP models into a validation package
#'
#' @param analysesDir  The directory containing folders with PLP models
#' @param minCellCount  The min cell count when trasporting the PLP model evaluation results
#' @param databaseName  The name of the database as a string
#' @param outputDir  the location to save the transported models (defaults to inst/plp_models)
#'
#' @return
#' The models will now be in the package
#'
#' @export
transportPlpModels <- function(analysesDir,
                               minCellCount = 5,
                               databaseName = 'sharable name of development data',
                               outputDir
){
  if(missing(outputDir)){
    outputDir <- 'inst/plp_models'
  }

  files <- dir(analysesDir, recursive = F, full.names = F)
  files <- files[grep('Analysis_', files)]
  filesIn <- file.path(analysesDir, files , 'plpResult')
  filesOut <- file.path(outputDir, files, 'plpResult')

  for(i in 1:length(filesIn)){
    plpResult <- PatientLevelPrediction::loadPlpResult(filesIn[i])
    PatientLevelPrediction::transportPlp(plpResult,
                 modelName= files[i], dataName=databaseName,
                 outputFolder = filesOut[i],
                 n=minCellCount,
                 includeEvaluationStatistics=T,
                 includeThresholdSummary=T, includeDemographicSummary=T,
                 includeCalibrationSummary =T, includePredictionDistribution=T,
                 includeCovariateSummary=T, save=T)

  }
}
##### user designed models
### todo: add best model
getModel <- function(model = 'SimpleModel.csv'){
  pathToCustom <- system.file("settings", model , package = "oxfordKneeValidation")
  coefficients <- utils::read.csv(pathToCustom)
  return(coefficients)
}

# get custom data:
getData <- function(connectionDetails,
                    cohortId,
                    outcomeIds,
                    cdmDatabaseSchema,
                    cdmDatabaseName,
                    cohortDatabaseSchema,
                    cohortTable,
                    oracleTempSchema,
                    standardCovariates,
                    endDay,
                    firstExposureOnly,
                    sampleSize,
                    cdmVersion,
                    studyStartDate,
                    studyEndDate){

  pathToCustom <- system.file("settings", 'CustomCovariates.csv', package = "oxfordKneeValidation")
  cohortVarsToCreate <- utils::read.csv(pathToCustom)
  covSets <- list()
  length(covSets) <- nrow(cohortVarsToCreate)+1
  covSets[[1]] <- standardCovariates

  for(i in 1:nrow(cohortVarsToCreate)){
    covSets[[1+i]] <- createCohortCovariateSettings(covariateName = as.character(cohortVarsToCreate$cohortName[i]),
                                                    covariateId = cohortVarsToCreate$cohortId[i]*1000+456,
                                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                                    cohortTable = cohortTable,
                                                    cohortId = cohortVarsToCreate$atlasId[i],
                                                    startDay=cohortVarsToCreate$startDay[i],
                                                    endDay=endDay,
                                                    count= as.character(cohortVarsToCreate$count[i]))
  }

  result <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         oracleTempSchema = oracleTempSchema,
                                                         cohortId = as.double(as.character(cohortId)),
                                                         outcomeIds = as.double(as.character(outcomeIds)),
                                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                                         outcomeDatabaseSchema = cohortDatabaseSchema,
                                                         cohortTable = cohortTable,
                                                         outcomeTable = cohortTable,
                                                         cdmVersion = cdmVersion,
                                                         firstExposureOnly = firstExposureOnly,
                                                         sampleSize =  sampleSize,
                                                         covariateSettings = covSets,
                                                         studyStartDate = studyStartDate,
                                                         studyEndDate = studyEndDate)

  return(result)

}
getSettings <- function(predictTkrSimple,
                        usePackageCohorts){

  settingR <- c()

  if(predictTkrSimple){

    if(!usePackageCohorts){
      settingR <- rbind(settingR,
                        c(cohortId = 1,
                          outcomeId = 2,
                          analysisId = 1001,
                          model = 'TkrSimple.csv'))
    } else{
      settingR <- rbind(settingR,
                        data.frame(cohortId = c(8220),
                                   outcomeId = c(8210),
                                   studyStartDate = c(""),
                                   studyEndDate =c(""),
                                   analysisId = c(1001),
                                   model = "TkrSimple.csv"))
    }
  }

  settingR <- as.data.frame(settingR)
  return(settingR)

}





createCohortCovariateSettings <- function(covariateName, covariateId,
                                          cohortDatabaseSchema, cohortTable, cohortId,
                                          startDay=-30, endDay=0, count=T) {
  covariateSettings <- list(covariateName=covariateName, covariateId=covariateId,
                            cohortDatabaseSchema=cohortDatabaseSchema,
                            cohortTable=cohortTable,
                            cohortId=cohortId,
                            startDay=startDay,
                            endDay=endDay,
                            count=count)

  attr(covariateSettings, "fun") <- "getCohortCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


getPredict <- function(model){
  predictExisting <- function(plpData, population){
    coefficients <- model

    prediction <- merge(plpData$covariates, ff::as.ffdf(coefficients), by = "covariateId")
    prediction$value <- prediction$covariateValue * prediction$points
    prediction <- PatientLevelPrediction:::bySumFf(prediction$value, prediction$rowId)
    colnames(prediction) <- c("rowId", "value")
    prediction <- merge(population, prediction, by ="rowId", all.x = TRUE)
    prediction$value[is.na(prediction$value)] <- 0

    # add any final mapping here (e.g., add intercept and mapping)
    prediction$value <- prediction$value + model$points[model$covariateId==0]
    prediction$value <- prediction$value/10
    prediction$value <- 1/(1+exp(-1*prediction$value))

    scaleVal <- max(prediction$value)
    if(scaleVal>1){
      prediction$value <- prediction$value/scaleVal
    }

    attr(prediction, "metaData") <- list(predictionType = 'binary', scale = scaleVal)

    return(prediction)
  }
  return(predictExisting)
}

#' Extracts covariates based on cohorts
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param connection  The database connection
#' @param oracleTempSchema  The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' The models will now be in the package
#'
#' @export
getCohortCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cdmVersion = "5",
                                   cohortTable = "#cohort_person",
                                   rowIdField = "row_id",
                                   aggregated,
                                   cohortId,
                                   covariateSettings) {

  # Some SQL to construct the covariate:
  sql <- paste(
    "select a.@row_id_field AS row_id, @covariate_id AS covariate_id,
    {@countval}?{count(distinct b.cohort_start_date)}:{max(1)} as covariate_value",
    "from @cohort_temp_table a inner join @covariate_cohort_schema.@covariate_cohort_table b",
    " on a.subject_id = b.subject_id and ",
    " b.cohort_start_date >= dateadd(day, @startDay, a.cohort_start_date) and ",
    " b.cohort_start_date <= dateadd(day, @endDay, a.cohort_start_date) ",
    "where b.cohort_definition_id = @covariate_cohort_id
    group by a.@row_id_field "
  )

  sql <- SqlRender::render(sql,
                           covariate_cohort_schema = covariateSettings$cohortDatabaseSchema,
                           covariate_cohort_table = covariateSettings$cohortTable,
                           covariate_cohort_id = covariateSettings$cohortId,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           covariate_id = covariateSettings$covariateId,
                           endDay=covariateSettings$endDay,
                           countval = covariateSettings$count)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql.ffdf(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  # Construct covariate reference:
  sql <- "select @covariate_id as covariate_id, '@concept_set' as covariate_name,
  12 as analysis_id, -1 as concept_id"
  sql <- SqlRender::render(sql, covariate_id = covariateSettings$covariateId,
                           concept_set=paste(covariateSettings$covariateName,' days before:', covariateSettings$startDay, 'days after:', covariateSettings$endDay))
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariateRef:
  covariateRef  <- DatabaseConnector::querySql.ffdf(connection, sql)
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))

  analysisRef <- data.frame(analysisId = 4,
                            analysisName = "cohort covariate",
                            domainId = "cohort covariate",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  analysisRef <- ff::as.ffdf(analysisRef)

  metaData <- list(sql = sql, call = match.call())
  result <- list(covariates = covariates,
                 covariateRef = covariateRef,
                 analysisRef=analysisRef,
                 metaData = metaData)
  class(result) <- "covariateData"
  return(result)
}

