# Start
start_time <- Sys.time()
outputFolder <-  here::here("Results")
source(here::here("scripts", "functions.R"))

logfile <- file.path( paste0(outputFolder, 
  "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"), file = logfile, append = TRUE)
  cli::cli_inform(paste(Sys.time(), "-", message, "\n"))
}

log_message("Start time recorded. Code version: 1.1.0")

# Snapshot
log_message("Getting cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm) 

if (characterisation) {
  log_message("Starting characterisation")
  
  # parameters
  tableName <- c("observation_period", "visit_occurrence", "condition_occurrence", "drug_exposure", "procedure_occurrence", 
                 "device_exposure", "measurement" , "observation", "death")
  sex <- TRUE # FALSE
  ageGroup <- list(c(0,19), c(20, 39),c(40, 59), c(60, 79), c(80, Inf) ) 
  
  dateRange <- as.Date(c("2012-01-01", NA))
  
  # Population Characteristics
  log_message("Getting population characteristics")
  
  cdm <- omopgenerics::bind(
    CohortConstructor::demographicsCohort(cdm, "population_1", sex = "Both"),
    CohortConstructor::demographicsCohort(cdm, "population_2", sex = "Both", ageRange = ageGroup),
    name = "population"
  )
  
  set <- omopgenerics::settings(cdm$population) |>
    dplyr::mutate(cohort_name = tolower(dplyr::if_else(
      is.na(.data$age_range), "general_population", paste0("age_group_", .data$age_range)
    ))) |>
    dplyr::select("cohort_definition_id", "cohort_name")
  
  result_populationCharacteristics <- cdm$population |>
    omopgenerics::newCohortTable(cohortSetRef = set, .softValidation = TRUE) |>
    CohortConstructor::trimToDateRange(dateRange = dateRange) |>
    PatientProfiles::addSexQuery() |>
    CohortCharacteristics::summariseCharacteristics(
      strata = list("sex"), 
      estimates = list(
        date = c("min", "q25", "median", "q75", "max"),
        numeric = c("min", "q25", "median", "q75", "max", "mean", "sd", "density"),
        categorical = c("count", "percentage"),
        binary = c("count", "percentage")
      )
    )
  omopgenerics::dropSourceTable(cdm = cdm, c("population_1", "population_2", "population"))
  
  # Summarise person table
  log_message("Summarising person table")
  summaryPersonTable <- characterisePersonTable(cdm)
  
  # Summarise IMD records
  log_message("Summarising IMD records")
  summaryIMD <- summariseImdRecords(cdm)
  
  # Summarise ethnicity records
  log_message("Summarising ethnicity records")
  summaryEthnicity <- summariseEthnicityRecords(cdm)
  
  # Summarise missing data
  log_message("Summarising missing data")
  result_missingData <- OmopSketch::summariseMissingData(cdm ,
                                                         omopTableName = c("person", tableName), 
                                                         sex = sex, 
                                                         ageGroup = ageGroup,
                                                         interval = "years", 
                                                         dateRange = dateRange)
  
  # Summarise clinical records
  log_message("Summarising clinical records")
  result_clinicalRecords<- OmopSketch::summariseClinicalRecords(cdm, 
                                                                omopTableName = tableName, 
                                                                sex = sex, 
                                                                ageGroup = ageGroup,
                                                                dateRange = dateRange) 
  
  # Summarize record counts
  log_message("Summarising record counts")
  result_recordCounts <- OmopSketch::summariseRecordCount(cdm,  tableName,
                                                          sex = sex,
                                                          ageGroup = ageGroup,
                                                          interval = "years",
                                                          dateRange = dateRange)

  # Summarize in observation records
  log_message("Summarising in observation records and person-days")
  result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, 
                                                             output = c("record","person-days"), 
                                                             interval = "years",
                                                             sex = sex,
                                                             ageGroup = ageGroup,
                                                             dateRange = dateRange) 

  # Summarise observation period
  log_message("Summarising observation period")
  result_observationPeriod1 <- OmopSketch::summariseObservationPeriod(cdm$observation_period, 
                                                                     sex = sex, 
                                                                     ageGroup = ageGroup, 
                                                                     dateRange = dateRange)
  colsStrata <- omopgenerics::strataColumns(result_observationPeriod1)
  result_observationPeriod1 <- result_observationPeriod1 |>
    omopgenerics::splitStrata() |>
    dplyr::mutate(observation_period_name = "First to last") |>
    omopgenerics::uniteStrata(cols = c(colsStrata, "observation_period_name")) |>
    omopgenerics::newSummarisedResult()
  
  # Measurement diagnostics
  log_message("Running measurement diagnostics")
  measurement_codes <- omopgenerics::importCodelist(here::here("measurement_codes"), type = "csv")
  result_MeasurementUse <- MeasurementDiagnostics::summariseMeasurementUse(cdm = cdm, 
                                                                           codes = measurement_codes,
                                                                           dateRange = dateRange)
  
  # Characterise observation period created by OmopConstructor to compare
  log_message("Create observation period with OmopConstructor - first to extraction")
  cdm <- OmopConstructor::buildObservationPeriod(cdm = cdm)
  
  log_message("Summarising created observation period (first to extract)")
  result_observationPeriod2 <- OmopSketch::summariseObservationPeriod(
    cdm$observation_period, 
    sex = sex, 
    ageGroup = ageGroup, 
    dateRange = dateRange
  )
  colsStrata <- omopgenerics::strataColumns(result_observationPeriod2)
  result_observationPeriod2 <- result_observationPeriod2 |>
    omopgenerics::splitStrata() |>
    dplyr::mutate(observation_period_name = "First to last") |>
    omopgenerics::uniteStrata(cols = c(colsStrata, "observation_period_name")) |>
    omopgenerics::newSummarisedResult()
  
  # Characterise observation period created by OmopConstructor to compare
  log_message("Create observation period with OmopConstructor - 365 days")
  cdm <- OmopConstructor::buildObservationPeriod(cdm = cdm, 
                                                 collapseDays = 365, 
                                                 persistenceDays = 365)
  
  log_message("Summarising created observation period (365)")
  result_observationPeriod3 <- OmopSketch::summariseObservationPeriod(
    cdm$observation_period, 
    sex = sex, 
    ageGroup = ageGroup, 
    dateRange = dateRange
  )
  colsStrata <- omopgenerics::strataColumns(result_observationPeriod3)
  result_observationPeriod3 <- result_observationPeriod3 |>
    omopgenerics::splitStrata() |>
    dplyr::mutate(observation_period_name = "Collapse 365") |>
    omopgenerics::uniteStrata(cols = c(colsStrata, "observation_period_name")) |>
    omopgenerics::newSummarisedResult()
  
  log_message("Characterisation finished")
} else {
  log_message("Skipping characterisation")
}

if (conceptCounts) {
  # Summarise concept counts
  log_message("Summarising concept id counts")
  result_conceptIdCount <- OmopSketch::summariseConceptIdCounts(cdm, 
                                                                omopTableName = tableName, 
                                                                sex = sex, 
                                                                ageGroup = ageGroup, 
                                                                interval = "years", 
                                                                dateRange = dateRange)
} else {
  log_message("Skipping concept id counts")
}

log_message("Binding results")
# bind results
results <- list(snapshot)
if (characterisation) {
  results <- c(results, list(
    result_populationCharacteristics, summaryPersonTable, result_missingData,
    result_clinicalRecords, result_recordCounts, result_inObservation,
    result_observationPeriod1, result_observationPeriod2, 
    result_observationPeriod3, summaryIMD, summaryEthnicity,
    result_MeasurementUse
  ))
}
if (conceptCounts) {
  results <- c(results, list(result_conceptIdCount))
}
results <- omopgenerics::bind(results)

# Export results
log_message("Export results")
omopgenerics::exportSummarisedResult(results, minCellCount = minCellCount, path = outputFolder, fileName = paste0(
  "result_characterisation_", dbName, ".csv"))

# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log_message(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

# Close connection
CDMConnector::cdmDisconnect(cdm)
log_message("Database connection closed")

# Zip the results
log_message("Zipping results") 

files_to_zip <- list.files(outputFolder)
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip, dbName)]

zip::zip(
  zipfile = file.path(paste0(outputFolder, "/results_characterisation_", dbName, ".zip")),
  files = files_to_zip,
  root = outputFolder
)
