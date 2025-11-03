# Start
start_time <- Sys.time()

outputFolder <-  here::here("Results")
source(here::here("scripts", "functions.R"))

log_file <- file.path(outputFolder, paste0("/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"), ".txt"))

omopgenerics::createLogFile(logFile = log_file)

omopgenerics::logMessage("Reading tables in write schema (initial)")
initialTables <- omopgenerics::listSourceTables(cdm = cdm)

result <- list()

# Summarise IMD records
omopgenerics::logMessage("Summarising IMD records")
result[["summaryIMD"]] <- summariseImdRecords(cdm)

omopTableName <- c("person", "visit_occurrence", "visit_detail",
  "condition_occurrence", "drug_exposure", "procedure_occurrence",
  "device_exposure", "measurement", "observation", "death")
sex <- TRUE

ageGroup <- list(c(0,19), c(20, 39),c(40, 59), c(60, 79), c(80, Inf) ) 

dateRange <- as.Date(c("2012-01-01", NA))

interval <- "years"

omopgenerics::logMessage("Starting characterisation")

result[["characterisation"]] <- OmopSketch::databaseCharacteristics(cdm, 
                                                                    
                                                                    sex = sex, 
                                                                    ageGroup = ageGroup, 
                                                                    interval = interval, 
                                                                    inObservation = TRUE, 
                                                                    conceptIdCounts = FALSE,
                                                                    sample = NULL, 
                                                                    dateRange = dateRange)

omopgenerics::logMessage("Running measurement diagnostics")

measurement_codes <- omopgenerics::importCodelist(here::here("measurement_codes"), type = "csv")

result[["measurementUse"]] <- MeasurementDiagnostics::summariseMeasurementUse(cdm = cdm, 
                                                                         codes = measurement_codes,
                                                                         dateRange = dateRange)

omopgenerics::logMessage("Building observation period 'First to extract'")

cdm <- OmopConstructor::buildObservationPeriod(cdm = cdm)

omopgenerics::logMessage("Summarise observation period 'First to extract'")

result$observation_period_2 <- OmopSketch::summariseObservationPeriod(
  cdm = cdm,
  sex = sex, 
  ageGroup = ageGroup, 
  interval = interval, 
  dateRange = dateRange,
  # nameObservationPeriod = "First to extract"
)

omopgenerics::logMessage("Building observation period 'Collapse 365'")

cdm <- OmopConstructor::buildObservationPeriod(cdm = cdm, 
                                               collapseDays = 365, 
                                               persistenceDays = 365)

omopgenerics::logMessage("Summarise observation period 'Collapse 365'")

result$observation_period_3 <- OmopSketch::summariseObservationPeriod(
  cdm = cdm,
  sex = sex, 
  ageGroup = ageGroup, 
  interval = interval, 
  dateRange = dateRange,
  # nameObservationPeriod = "Collapse 365"
)
 
result <- omopgenerics::bind(result)

# Close connection
CDMConnector::cdmDisconnect(cdm)
omopgenerics::logMessage("Database connection closed")

# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
omopgenerics::logMessage(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

# Export results
omopgenerics::logMessage("Export and zipping results")

omopgenerics::exportSummarisedResult(result,
                                     minCellCount = minCellCount, 
                                     path = outputFolder, 
                                     fileName = "result_characterisation_{cdm_name}.csv")

files_to_zip <- list.files(outputFolder)
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip, dbName)]

zip::zip(
  zipfile = file.path(paste0(outputFolder, "/results_characterisation_", dbName, ".zip")),
  files = files_to_zip,
  root = outputFolder
)
