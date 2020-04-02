# In this file we will keep functions for working with google keys. Currently we
# only read them, but we might also want to write, which will require additional
# functions

# packages: httr, utils

read_google_key <- function(gk_url, na_string = " ") {
  # Description: reads a google key from a worksheet from an url
  # Input:  a google key which identifies a single spreadsheet: "https://docs.google.com/spreadsheets...."
  # Output: a data frame based on the shared google sheet
  # Notes:
  ## 1. Make sure that the google sheet is shared on the web and get the shared link
  ## 2. The function is intended to work as part of lapply
  ### Example: google_keys %>% lapply(., function(x) read_google_key(x))
  ### where google_keys is a list(table = google_key, ...)
  tryCatch({
    response <- httr::GET(gk_url)
    df <- utils::read.csv(
      textConnection(httr::content(response, "text")),
      na.strings = na_string,
      stringsAsFactors = FALSE
    )
  }, error = function(e){
    print(e)
    msg = paste0("Cannot read key ", gk_url,  " from googlesheets")
    stop(msg)
  })
  return (df)
}
