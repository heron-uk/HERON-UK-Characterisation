# Background

This Shiny app presents database characterisation results produced as part of the [HERON UK](https://ukhealthdata.org/data-standards/common-data-models/rwe-network/) Year 2 onboarding process.

The aim is to provide a overview of data content, population coverage, and data quality across participating data partners mapped to the OMOP Common Data Model.

## Data Sources

The analyses were run on the following databases:

- IDRIL_1 (Lancashire) – secondary care  
- Barts Health – secondary care  
- UCLH (from 2019 onwards) – secondary care  
- CPRD Aurum – primary care  
- CPRD Gold – primary care  
- DataLoch (University of Edinburgh) – secondary care  
- LTHT (Leeds Teaching Hospitals Trust) – secondary care  
- GOSH (Great Ormond Street Hospital) – secondary care  

## Analyses

The app includes the following characterisation outputs:

- **Snapshot**: Metadata extracted from the `cdm_source` table, using the output of [`summariseOmopSnapshot()`](https://ohdsi.github.io/OmopSketch/reference/summariseOmopSnapshot.html).
- **Population Characteristics**: Summary of the demographics of the population in observation, generated using [**CohortConstructor**](https://ohdsi.github.io/CohortConstructor/) and [**CohortCharacteristics**](https://darwin-eu.github.io/CohortCharacteristics/).
- **Person**: Summary of person table, from [`summarisePerson()`](https://ohdsi.github.io/OmopSketch/reference/summarisePerson.html)
- **IMD**: Summary of available data about [IMD](https://www.gov.uk/government/collections/english-indices-of-deprivation)
- **Observation Period**: Distribution and length of observation periods, based on [`summariseObservationPeriod()`](https://ohdsi.github.io/OmopSketch/reference/summariseObservationPeriod.html).
- **Clinical Records**: Summary of clinical tables focused on vocabulary usage and quality checks, from [`summariseClinicalRecords()`](https://ohdsi.github.io/OmopSketch/reference/summariseClinicalRecords.html).

The focus of the analysis is the study period 01/01/2012 - present.

::: {style="display: flex; justify-content: center; align-items: center; height: 50vh;"}
<img src="hdruk_logo.svg" style="max-width: 200px; height: auto;"/>
:::
