# Characterisation

This Shiny app presents the results of analyses conducted on the following databases:

-   IDRIL_1 (Lancashire) - secondary care

-   Barts Health- secondary care

-   UCLH from 2019 - secondary care

-   CPRD Aurum - primary care

-   DataLoch (University of Edinburgh) - secondary care

-   LTHT (Leeds) - secondary care

-   GOSH - secondary care

The analyses include:

- **Snapshot**: Metadata extracted from the `cdm_source` table, using the output of [`summariseOmopSnapshot()`](https://ohdsi.github.io/OmopSketch/reference/summariseOmopSnapshot.html).
- **Population Characteristics**: Summary of the demographics of the population in observation, generated using [**CohortConstructor**](https://ohdsi.github.io/CohortConstructor/) and [**CohortCharacteristics**](https://darwin-eu.github.io/CohortCharacteristics/).
- **Person**: Summary of person table, from [`summarisePerson()`]()
- **Observation Period**: Distribution and length of observation periods, based on [`summariseObservationPeriod()`](https://ohdsi.github.io/OmopSketch/reference/summariseObservationPeriod.html).
- **Trends**: Temporal trends of individuals and records in observation, including changes in median age, proportion of females, and number of person-days, generated from [`summariseTrend()`](https://ohdsi.github.io/OmopSketch/reference/summariseTrend.html).
- **Clinical Records**: Summary of clinical tables focused on vocabulary usage and quality checks, from [`summariseClinicalRecords()`](https://ohdsi.github.io/OmopSketch/reference/summariseClinicalRecords.html).

The focus of the analysis is the study period 01/01/2012 - present.

::: {style="display: flex; justify-content: center; align-items: center; height: 50vh;"}
<img src="hdruk_logo.svg" style="max-width: 200px; height: auto;"/>
:::
