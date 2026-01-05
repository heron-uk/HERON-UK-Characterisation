# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period"),
  summarise_clinical_records = list(result_type = "summarise_clinical_records"),
  summarise_trend_episode = list(result_type = "summarise_trend", type = "episode"),
  summarise_trend_event = list(result_type = "summarise_trend", type = "event"),
  summarise_person = list(result_type = "summarise_person"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_imd = list(result_type = "summarise_imd"),
  summarise_log_file = list(result_type = "summarise_log_file")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
data <- prepareResult(result, resultList)

set <- omopgenerics::settings(data[["summarise_observation_period"]])

set <- set |>
  dplyr::mutate("name_observation_period" = dplyr::coalesce(.data$name_observation_period, "Default"))

data[["summarise_observation_period"]] <- omopgenerics::newSummarisedResult(x = data[["summarise_observation_period"]], settings = set)


set <- omopgenerics::settings(data[["summarise_trend_episode"]])

set <- set |>
  dplyr::mutate("name_observation_period" = dplyr::coalesce(.data$name_observation_period, "Default"))

data[["summarise_trend_episode"]] <- omopgenerics::newSummarisedResult(x = data[["summarise_trend_episode"]], settings = set)



result <- data |> omopgenerics::bind()

values <- getValues(result, resultList)


# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, values, choices, selected, resultList, data)
