
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
  
  if (TRUE) { #nrow(x) > 0) {
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
  omopgenerics::logMessage("Building observation period '{name}'")
  cdm <- OmopConstructor::buildObservationPeriod(cdm = cdm, 
                                                 collapseDays = collapseDays, 
                                                 persistenceDays = persistenceDays)
  
  omopgenerics::logMessage("Summarise observation period '{name}'")
  res1 <- OmopSketch::summariseObservationPeriod(
    cdm = cdm,
    byOrdinal = FALSE,
    sex = FALSE, 
    ageGroup = FALSE, 
    dateRange = dateRange,
    nameObservationPeriod = name
  )
  
  omopgenerics::logMessage("Summarise in observation '{name}'")
  res2 <- OmopSketch::summariseTrend(
    cdm = cdm,
    ageGroup = FALSE, 
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
