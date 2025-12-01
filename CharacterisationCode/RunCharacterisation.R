# Start
start_time <- Sys.time()

outputFolder <- here::here("Results")
source(here::here("scripts", "functions.R"))

if (isTRUE(logSql)) {
  options(omopgenerics.log_sql_path = here::here("Results", "sql_logs"))
}
if (isTRUE(logSqlExplain)) {
  options(omopgenerics.log_sql_explain_path = here::here("Results", "sql_explain"))
}

log_file <- file.path(outputFolder, paste0("/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"), ".txt"))

omopgenerics::createLogFile(logFile = log_file)

omopgenerics::logMessage("Reading tables in write schema (initial)")
initialTables <- omopgenerics::listSourceTables(cdm = cdm)

result <- list()

# Summarise IMD records
omopgenerics::logMessage("Summarising IMD records")
result[["summaryIMD"]] <- summariseImdRecords(cdm)

omopTableName <- c(
  "visit_occurrence", "visit_detail", "condition_occurrence", "drug_exposure",
  "procedure_occurrence", "device_exposure", "measurement", "observation",
  "death", "note", "payer_plan_period",
  "drug_era", "dose_era", "condition_era"
)

sex <- TRUE

ageGroup <- list(c(0, 17), c(18, 65), c(66, Inf))
dateRange <- as.Date(c("2012-01-01", NA))

interval <- "years"

omopgenerics::logMessage("Starting characterisation")

result[["characterisation"]] <- databaseCharacteristicsLocal(cdm,
                                                             omopTableName = omopTableName,
  sex = sex,
  ageGroup = ageGroup,
  interval = interval,
  inObservation = TRUE,
  dateRange = dateRange
)

omopgenerics::logMessage("Running measurement diagnostics")

measurement_codes <- omopgenerics::importCodelist(here::here("measurement_codes"), type = "csv")

result[["measurementUse"]] <- MeasurementDiagnostics::summariseMeasurementUse(
  cdm = cdm,
  codes = measurement_codes,
  dateRange = dateRange
)

# characterise observation periods
result$first_to_extract <- summariseCustomObservationPeriod(
  cdm = cdm,
  collapseDays = Inf,
  persistenceDays = Inf,
  dateRange = dateRange,
  name = "First to extract"
)
result$first_to_last <- summariseCustomObservationPeriod(
  cdm = cdm,
  collapseDays = Inf,
  persistenceDays = 0,
  dateRange = dateRange,
  name = "First to last"
)
result$ongoing <- summariseCustomObservationPeriod(
  cdm = cdm,
  collapseDays = 1,
  persistenceDays = 0,
  dateRange = dateRange,
  name = "Ongoing"
)
result$collapse_365 <- summariseCustomObservationPeriod(
  cdm = cdm,
  collapseDays = 365,
  persistenceDays = 0,
  dateRange = dateRange,
  name = "Collapse 365"
)
result$persistence_365 <- summariseCustomObservationPeriod(
  cdm = cdm,
  collapseDays = 365,
  persistenceDays = 364,
  dateRange = dateRange,
  name = "Persistence 365"
)
result$collapse_730 <- summariseCustomObservationPeriod(
  cdm = cdm,
  collapseDays = 730,
  persistenceDays = 0,
  dateRange = dateRange,
  name = "Collapse 730"
)
result$persistence_730 <- summariseCustomObservationPeriod(
  cdm = cdm,
  collapseDays = 730,
  persistenceDays = 729,
  dateRange = dateRange,
  name = "Persistence 730"
)

# bind results
result <- omopgenerics::bind(result)

# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
omopgenerics::logMessage(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

# Export results
omopgenerics::logMessage("Exporting results")

omopgenerics::exportSummarisedResult(result,
  minCellCount = minCellCount,
  path = outputFolder,
  fileName = "result_characterisation_{cdm_name}_{date}.csv",
  logSqlPath = NULL,
  logExplainPath = NULL
)
