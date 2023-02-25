## Set variables
data_pull_start_date <- "2022-06-01"
data_pull_end_date <- "2023-06-01"
ascend_survey_name <- "ascend22" #matches the column in the copilot tables above
elevate_survey_name <- "elevate22" #matches the column in the copilot tables above

## Load packages
library(tidyverse)

## Import modules
saturn <- import_module("saturn")
util <- import_module("util")

## Load data (this is the test)
paths <- list(creds = "rserve_credential_body.json")
creds_json <- util$find_crypt_paths(paths)$creds
creds <- jsonlite::fromJSON(creds_json)

# Make sure given arguments will work with functions
if (length(creds) == 0) {
  stop("No creds found in read-access crypt. Is it mounted?")
}

# Get data from Saturn
saturn_service <- saturn$create_service(creds$saturn_sql_credentials)

elevate <- saturn_service$get_responses(
  survey_label = "elevate22",
  start_date = data_pull_start_date,
  end_date = data_pull_end_date
)

ascend <- saturn_service$get_responses(
  survey_label = "ascend22",
  start_date = data_pull_start_date,
  end_date = data_pull_end_date
)
