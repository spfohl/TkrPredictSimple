oxfordKneeValidation
======================
<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- Analytics use case(s): **Patient-Level Prediction**
- Study type: **Clinical Application**
- Tags: **Study-a-thon, COVID-19**
- Study lead: **Jenna Reps, Ross Williams**
- Study lead forums tag: **[jreps](https://forums.ohdsi.org/u/jreps), [RossW](https://forums.ohdsi.org/u/RossW),**
- Study start date: **Dec 16, 2018**
- Study end date: **-**
- Protocol: ****
- Publications: **-**
- Results explorer: **-**

The objective of this study is to develop and validate various patient-level prediction models for total knee replacement patients. 


Introduction
============
This repo contains the simple models needed for validation of the Oxford Ehden study-athon simple models to predict mortality following a total knee replacement

Features
========
  - Validates the full models developed in Thin and OptumDod as well as a user designed simple and a data driven simple developed in optumDod

Technology
==========
  oxfordKneeValidation is an R package.

System Requirements
===================
  Requires R (version 3.3.0 or higher).

Dependencies
============
  * PatientLevelPrediction

Getting Started
===============
  1. In R, use the following commands to run the study:

  ```r
  # If not building locally uncomment and run:
#install.packages("devtools")
#devtools::install_github("OHDSI/StudyProtocolSandbox/oxfordKneeValidation")

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
                 
# add code to submit results to study admin here


```

License
=======
  oxfordKneeValidation is licensed under Apache License 2.0

Development
===========
  oxfordKneeValidation is being developed in R Studio.
