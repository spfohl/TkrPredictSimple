library(oxfordKneeValidation)
# USER INPUTS
#=======================
# Specify where the temporary files (used by the ff package) will be created:
options(andromedatempdir = "S://temp//tempandromeda")

# The folder where the study intermediate and result files will be written:
outputFolder <- "./tkrSimpleResults"

dbms <-
user <-
pw <-
server <-
port <-

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)
# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- ''
# Add the name of database containing the OMOP CDM data
cdmDatabaseName <- ''

# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- ''
oracleTempSchema <- NULL


# table name where the cohorts will be generated
cohortTable <- 'tkrSimpleTest'

#============== Pick Study Parts To Run: ===========
createCohorts = FALSE
predictTkrSimple = TRUE
runValidation = TRUE

packageResults = TRUE

minCellCount <- 5
sampleSize <- NULL


#============== Pick T and O cohorts ===========

# [option 1] use default cohorts
usePackageCohorts <- TRUE
newTargetCohortId <- NULL
newOutcomeCohortId <- NULL
newCohortDatabaseSchema <- NULL
newCohortTable <- NULL

#=======================
# TAR settings - recommended to not edit
#=======================
riskWindowStart <- 0
startAnchor <- 'cohort start'
riskWindowEnd <- 90
endAnchor <- 'cohort start'
firstExposureOnly <- F
removeSubjectsWithPriorOutcome <- F
priorOutcomeLookback <- 99999
requireTimeAtRisk <- F
minTimeAtRisk <- 1
includeAllOutcomes <- T

execute(connectionDetails = connectionDetails,
        usePackageCohorts = usePackageCohorts,
        newTargetCohortId = newTargetCohortId,
        newOutcomeCohortId = newOutcomeCohortId,
        newCohortDatabaseSchema = newCohortDatabaseSchema,
        newCohortTable = newCohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cdmDatabaseName = cdmDatabaseName,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        sampleSize = sampleSize,
        riskWindowStart = riskWindowStart,
        startAnchor = startAnchor,
        riskWindowEnd = riskWindowEnd,
        endAnchor = endAnchor,
        firstExposureOnly = firstExposureOnly,
        removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = priorOutcomeLookback,
        requireTimeAtRisk = requireTimeAtRisk,
        minTimeAtRisk = minTimeAtRisk,
        includeAllOutcomes = includeAllOutcomes,
        outputFolder = outputFolder,
        createCohorts = createCohorts,
        predictTkrSimple = predictTkrSimple,
        runValidation = runValidation,
        packageResults = packageResults,
        minCellCount = minCellCount,
        verbosity = "INFO",
        cdmVersion = 5)
