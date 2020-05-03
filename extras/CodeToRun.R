library(oxfordKneeValidation)

# Specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "T:/temp")

# Details for connecting to the server:
dbms <- ""
user <- ""
pw <- ""
server <- ""
port <-

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)



# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- ''
# Add a sharebale name for the database containing the OMOP CDM data
cdmDatabaseName <- ''
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- ''

oracleTempSchema <- ""

outputFolder <- ""
cohortTable <- ''


# Now run the study
oxfordKneeValidation::execute(connectionDetails = connectionDetails,
                              databaseName = cdmDatabaseName,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              oracleTempSchema = oracleTempSchema,
                              cohortTable = cohortTable,
                              outputFolder = outputFolder,
                              createCohorts = T,
                              runValidation = T,
                              runSimple = T,
                              packageResults = F,
                              minCellCount = 5,
                              sampleSize = NULL)



