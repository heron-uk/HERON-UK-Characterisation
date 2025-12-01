summariseImdRecords <- function(cdm) {
  x <- cdm$observation |>
    dplyr::filter(.data$observation_source_concept_id == 35812882) |>
    dplyr::collect() |>
    dplyr::ungroup()
  persons <- cdm$person |>
    dplyr::select("person_id") |>
    dplyr::collect()

  result <- list()

  # total records
  result$total <- dplyr::tibble(
    variable_name = "Number IMD records",
    variable_level = NA_character_,
    count = x |> dplyr::tally() |> dplyr::pull()
  )

  # records per person
  result$records_per_person <- x |>
    dplyr::group_by(.data$person_id) |>
    dplyr::tally(name = "counts") |>
    dplyr::right_join(persons, by = "person_id") |>
    dplyr::mutate(counts = dplyr::coalesce(as.integer(.data$counts), 0L)) |>
    dplyr::summarise(
      mean = mean(.data$counts),
      sd = sd(.data$counts),
      min = min(.data$counts),
      q25 = quantile(.data$counts, 0.25),
      median = median(.data$counts),
      q75 = quantile(.data$counts, 0.75),
      max = max(.data$counts),
      count = sum(as.integer(.data$counts >= 1))
    ) |>
    dplyr::mutate(
      variable_name = "Number IMD records per person",
      variable_level = NA_character_
    )

  if (TRUE) { # nrow(x) > 0) {
    # values
    result$values <- x |>
      dplyr::group_by(.data$value_as_number) |>
      dplyr::tally(name = "count") |>
      dplyr::mutate(
        variable_name = "IMD value",
        variable_level = as.character(.data$value_as_number)
      ) |>
      dplyr::select(!"value_as_number")

    # most recent value
    result$recent_value <- persons |>
      dplyr::left_join(
        x |>
          dplyr::group_by(.data$person_id) |>
          dplyr::filter(
            .data$observation_date == min(.data$observation_date, na.rm = TRUE)
          ) |>
          dplyr::summarise(value_as_number = dplyr::first(.data$value_as_number)),
        by = "person_id"
      ) |>
      dplyr::group_by(.data$value_as_number) |>
      dplyr::tally(name = "count") |>
      dplyr::mutate(
        variable_name = "Most recent IMD value",
        variable_level = as.character(.data$value_as_number)
      ) |>
      dplyr::select(!"value_as_number")

    # records per year
    result$record_per_year <- x |>
      dplyr::mutate(year = as.integer(clock::get_year(.data$observation_date))) |>
      dplyr::group_by(.data$year) |>
      dplyr::tally(name = "count") |>
      dplyr::arrange(.data$year) |>
      dplyr::mutate(
        variable_name = "Year of IMD recording",
        variable_level = as.character(.data$year)
      ) |>
      dplyr::select(!"year")
  }

  # format result
  result <- result |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      cdm_name = omopgenerics::cdmName(cdm),
      package_name = "HERON-UK Chracterisation",
      package_version = "0.0.1",
      result_type = "summarise_imd"
    ) |>
    omopgenerics::transformToSummarisedResult(
      group = character(),
      strata = character(),
      additional = character(),
      settings = c("package_name", "package_version", "result_type"),
      estimates = c("count", "mean", "sd", "min", "q25", "median", "q75", "max")
    ) |>
    dplyr::filter(!is.na(.data$estimate_value))

  return(result)
}
summariseCustomObservationPeriod <- function(cdm,
                                             collapseDays,
                                             persistenceDays,
                                             dateRange,
                                             name) {
  omopgenerics::logMessage(paste0("Building observation period ", name))
  cdm <- OmopConstructor::buildObservationPeriod(
    cdm = cdm,
    collapseDays = collapseDays,
    persistenceDays = persistenceDays
  )

  omopgenerics::logMessage(paste0("Summarise observation period table: ", name))
  res1 <- OmopSketch::summariseObservationPeriod(
    cdm = cdm,
    byOrdinal = TRUE,
    sex = FALSE,
    ageGroup = NULL,
    dateRange = dateRange, 
    quality = FALSE, 
    missingData = FALSE
  )
  set <- omopgenerics::settings(x = res1) |>
    dplyr::mutate(name_observation_period = .env$name)
  res1 <- omopgenerics::newSummarisedResult(x = res1, settings = set)
  

  omopgenerics::logMessage(paste0("Summarise trends of observation period: ", name))
  res2 <- OmopSketch::summariseTrend(
    cdm = cdm,
    ageGroup = NULL,
    sex = FALSE,
    episode = "observation_period",
    interval = "years",
    output = c("record", "person", "person-days", "age"),
    dateRange = dateRange
  )
  set <- omopgenerics::settings(x = res2) |>
    dplyr::mutate(name_observation_period = .env$name)
  res2 <- omopgenerics::newSummarisedResult(x = res2, settings = set)

  omopgenerics::bind(res1, res2)
}


databaseCharacteristicsLocal <- function(cdm,
                                         omopTableName,
                                         sex,
                                         ageGroup,
                                         interval,
                                         inObservation,
                                         dateRange) {


  empty_tables <- c()
  for (table in omopTableName) {
    empty_tables <- c(empty_tables, table[omopgenerics::isTableEmpty(cdm[[table]])])
  }

  omopTableName <- omopTableName[!(omopTableName %in% empty_tables)]
  cli::cli_inform(paste("The characterisation will focus on the following OMOP tables: {omopTableName}"))

  startTime <- Sys.time()
  startTables <- omopgenerics::listSourceTables(cdm)

  result <- list()
  
  ageGroup <- omopgenerics::validateAgeGroupArgument(ageGroup, multipleAgeGroup = FALSE)
  
  # Snapshot
  cli::cli_inform(paste(cli::symbol$arrow_right, "Getting cdm snapshot"))
  result$snapshot <- OmopSketch::summariseOmopSnapshot(cdm)

  # Population Characteristics
  cli::cli_inform(paste(cli::symbol$arrow_right, "Getting population characteristics"))

  sexCohort <- if (sex) "Both"
  dateRangeCohort <- dateRange %||% c(NA, NA)


  if (!is.null(ageGroup) & omopgenerics::sourceType(cdm) != "local") { # to remove when https://github.com/OHDSI/CohortConstructor/issues/675 is fixed
    cdm <- omopgenerics::bind(
      CohortConstructor::demographicsCohort(cdm, "population_1", sex = sexCohort),
      CohortConstructor::demographicsCohort(cdm, "population_2", sex = sexCohort, ageRange = ageGroup$age_group),
      name = "population"
    )

    set <- omopgenerics::settings(cdm$population) |>
      dplyr::mutate(cohort_name = tolower(dplyr::if_else(
        is.na(.data$age_range), "general_population", paste0("age_group_", .data$age_range)
      ))) |>
      dplyr::select("cohort_definition_id", "cohort_name")
  } else {
    cdm$population <- CohortConstructor::demographicsCohort(cdm, "population", sex = sexCohort)

    set <- omopgenerics::settings(cdm$population) |>
      dplyr::mutate(cohort_name = "general_population") |>
      dplyr::select("cohort_definition_id", "cohort_name")
  }


  cdm$population <- cdm$population |>
    omopgenerics::newCohortTable(cohortSetRef = set, .softValidation = TRUE) |>
    CohortConstructor::trimToDateRange(dateRange = dateRangeCohort)

  if (sex) {
    cdm$population <- cdm$population |>
      PatientProfiles::addSexQuery()
  }

  result$populationCharacteristics <- cdm$population |>
    CohortCharacteristics::summariseCharacteristics(
      estimates = list(
        date = c("min", "q25", "median", "q75", "max"),
        numeric = c("min", "q25", "median", "q75", "max", "mean", "sd", "density"),
        categorical = c("count", "percentage"),
        binary = c("count", "percentage")
      )
    )

  omopgenerics::dropSourceTable(cdm = cdm, c("population_1", "population_2", "population"))

  # Summarising Person table
  cli::cli_inform(paste(cli::symbol$arrow_right, "Summarising person table"))

  result$person <- OmopSketch::summarisePerson(cdm)


  # Summarise clinical records
  cli::cli_inform(paste(cli::symbol$arrow_right, "Summarising clinical records"))
  result$clinicalRecords <- OmopSketch::summariseClinicalRecords(cdm,
    omopTableName = omopTableName,
    recordsPerPerson = c("median", "q25", "q75", "min", "max"),
    sex = FALSE,
    ageGroup = NULL,
    dateRange = dateRange, 
    conceptSummary = TRUE, 
    missingData = TRUE
  )


  # Summarise observation period
  cli::cli_inform(paste(cli::symbol$arrow_right, "Summarising observation period"))
  result$observationPeriod <- OmopSketch::summariseObservationPeriod(
    cdm = cdm,
    sex = sex,
    ageGroup = ageGroup,
    dateRange = dateRange
  )

  # Summarize in observation records
  cli::cli_inform(paste(cli::symbol$arrow_right, "Summarising trends: records, subjects, person-days, age and sex"))
  result$trend <- OmopSketch::summariseTrend(
    cdm = cdm,
    episode = "observation_period",
    event = c(omopTableName, "observation_period"),
    output = c("record", "person", "person-days"),
    interval = interval,
    sex = FALSE,
    ageGroup = NULL,
    dateRange = dateRange,
    inObservation = inObservation
  )

  # Combine results and export
  result <- result |>
    omopgenerics::bind()


  # Calculate duration and log
  dur <- abs(as.numeric(Sys.time() - startTime, units = "secs"))
  cli::cli_inform(
    paste(
      cli::symbol$smiley,
      "Database characterisation finished. Code ran in",
      floor(dur / 60), "min and",
      dur %% 60 %/% 1, "sec"
    )
  )
  endTables <- omopgenerics::listSourceTables(cdm)
  newTables <- setdiff(endTables, startTables)

  if (length(newTables)) {
    cli::cli_inform(c(
      "i" = "{length(newTables)} table{?s} created: {.val {newTables}}."
    ))
  }

  return(result)
}
