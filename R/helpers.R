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
                                     covariateSettings = covSets)

  return(result)

}


getModel <- function(model = 'SimpleModel.csv'){
  pathToCustom <- system.file("settings", model , package = "oxfordKneeValidation")
  coefficients <- utils::read.csv(pathToCustom)
   return(coefficients)
}

getPredict <- function(model){
  predictExisting <- function(plpData, population){
    coefficients <- model

    if('covariateData'%in%names(plpData)){
      plpData$covariateData$coefficients <- tibble::as_tibble(coefficients)
      on.exit(plpData$covariateData$coefficients <- NULL)

      prediction <- plpData$covariateData$covariates %>%
        dplyr::inner_join(plpData$covariateData$coefficients, by = "covariateId") %>%
        dplyr::mutate(value = covariateValue*points) %>%
        dplyr::select(rowId, value) %>%
        dplyr::group_by(rowId) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% dplyr::collect()


    } else{

      prediction <- merge(plpData$covariates, ff::as.ffdf(coefficients), by = "covariateId")
      prediction$value <- prediction$covariateValue * prediction$points
      prediction <- PatientLevelPrediction:::bySumFf(prediction$value, prediction$rowId)
      colnames(prediction) <- c("rowId", "value")
    }


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


getSettings <- function(predictTkrSimple ,
                        usePackageCohorts){

  settingR <- c()

  if(predictTkrSimple){


      settingR <- rbind(settingR,
                        data.frame(cohortId = 8220,
                                   outcomeId = 8210,
                                   model = 'TkrSimple.csv',
                                   analysisId = 1001))
    }




  settingR <- as.data.frame(settingR)
  return(settingR)

}


