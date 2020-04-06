logging <- import_module("logging")

get_response_body <- function (response) {
  # Takes an httr response.
  is_json <- grepl(
    'application/json',
    response$header[['content-type']],
    fixed = TRUE
  )

  if (is_json) {
    content <- httr::content(response)  # should automatically parse JSON
  } else {
    content <- httr::content(response, as = "text")  # don't parse
  }

  return(content)
}

retry <- function (f, tries = 5, delay_seconds = 1) {
  # Doubles delay seconds between every try. With default of 1s and 5 tries:
  # Try (1s sleep) try (2s sleep) try (4s sleep) try (8s sleep) try (return)
  result <- NULL
  for (try in 1:tries) {
    tryCatch(
      { result <- f(); break; },
      error = function (e) {
        logging$error(e)
        if (try != tries) {
          logging$info("retrying...")
          Sys.sleep(delay_seconds * 2^(try - 1))
        } # Else don't delay after the last try.
      }
    )
  }
  return(result)
}
