
# to restore renv environment
renv::restore()

# data base name, use an acronym to identify the database (e.g "CPRD GOLD")
dbName <- "..."

# create a DBI connection to your database
con <- DBI::dbConnect("...")

# schema in the database that contains OMOP CDM standard tables
cdmSchema <- "..."

# schema in the database where you have writing permissions
writeSchema <- "..."

# created tables will start with this prefix
prefix <- "..."

# minimum cell counts used for suppression
minCellCount <- 5

# to create the cdm object
cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmSchema,
  writeSchema =  writeSchema,
  writePrefix = prefix,
  cdmName = dbName
)

runCharacterisation <- TRUE
runOmopConstructor <- TRUE #turn to FALSE if you don't to run the characterisation of the observation period

source("RunCharacterisation.R")
