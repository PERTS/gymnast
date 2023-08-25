# Communicate with the Qualtrics API
#
# Import as a module and create the service:
#
# saturn <- import_module("saturn")
# saturn_service <- saturn$create_service(credentials)
#
# Then use exported methods:
#
# * saturn_service$get_responses(survey_label) - returns a data frame

# packages: dplyr, lubridate

modules::import('dplyr', `%>%`)

json_utils <- import_module("json_utils")
logging <- import_module("logging")
sql <- import_module("sql")

IP_ANALYSIS_REPLICA <- '127.0.0.1'
IP_LOCALHOST <- '127.0.0.1'

create_service <- function (
  credentials = list(),
  mysql_user = 'saturn',
  password = 'saturn',
  db_name = 'saturn'
) {
  if (length(credentials) == 0) {
    logging$info("saturn credentials not found, using localhost defaults.")
    ip <- IP_LOCALHOST
    mysql_user <- 'saturn'
    password <- 'saturn'
    ssl_credentials <- list()
  } else {
    expected_creds <- c('ca', 'cert', 'key', 'password')
    for (n in expected_creds) {
      if (is.null(credentials[[n]])) {
        stop(paste0("saturn.R couldn't find credential: ", n))
      }
    }

    ip <- IP_ANALYSIS_REPLICA
    mysql_user <- 'readonly'

    password <- credentials$password
    ssl_credentials <- credentials
    ssl_credentials$password <- NULL
  }


  get_raw_responses <- function (
    survey_label,
    codes = NULL,
    start_date = NULL,
    end_date = NULL
  ) {
    logging$info("saturn_service$get_responses()")

    conn <- sql$create_service(
      ip,
      dbname = db_name,
      ssl_credentials = ssl_credentials,
      password = password,
      mysql_user = mysql_user,
      port = 3421
    )

    safe_survey_label <- conn$escape_strings(survey_label)
    safe_codes <- NULL
    if (!is.null(codes)) {
      safe_codes <- conn$escape_strings(codes)
    }
    safe_start_date <- NULL
    if (!is.null(start_date)) {
      # Handles both characters and dates:
      safe_start_date <- start_date %>%
        lubridate::ymd() %>%
        as.character() %>%
        conn$escape_strings()
    }
    safe_end_date <- NULL
    if (!is.null(end_date)) {
      # Handles both characters and dates:
      safe_end_date <- end_date %>%
        lubridate::ymd() %>%
        as.character() %>%
        conn$escape_strings()
    }

    query <- paste0("
      SELECT *
      FROM `response`
      WHERE `survey_label` = '", safe_survey_label, "'",
      ifelse(
        is.null(codes),
        "",
        paste0(
          "\nAND code IN('",
          paste0(safe_codes, collapse = "', '"),
          "')"
        )
      ),
      ifelse(
        is.null(start_date),
        "",
        paste0("\nAND modified >= '", safe_start_date, "'")
      ),
      ifelse(
        is.null(end_date),
        "",
        paste0("\nAND modified <= '", safe_end_date, "'")
      ),
      ";
    ")

    results <- conn$query(query)

    conn$disconnect()

    logging$info("saturn service get_raw_responses() got dimensions:", dim(results))

    return(results)
  }

  get_responses <- function (...) {
    qa_codes <- c(
      'BASIC1',
      'BASIC2',
      'OVERLAP1',
      'OVERLAP2',
      'OVERLAP3'
    )

    wide <- get_raw_responses(...) %>%
      dplyr::filter(!code %in% qa_codes) %>%
      # It's typical for these to be duplicated with data in meta; rename to
      # prevent collision. We'll check sanity on them next and clean up.
      dplyr::rename(
        code_original = code,
        participant_id_original = participant_id,
      ) %>%
      json_utils$widen_object_column(meta) %>%
      json_utils$widen_object_column(answers)

    for (n in c('code', 'participant_id')) {
      if (!all(wide[[n]] == wide[[paste0(n, '_original')]])) {
        stop(paste0(
          "saturn$get_responses(): `", n, "` in meta didn't match; ",
          "indicates a problem with saturn data storage."
        ))
      }
    }

    wide$code_original <- NULL
    wide$participant_id_original <- NULL

    return(wide)
  }

  fake_responses <- function (...) {
    stop("saturn$fake_responses() not implemented yet.")

    # refer to qualtrics$fake_responses()
  }

  query <- function (query_str) {
    logging$info("saturn_service$query()")

    conn <- sql$create_service(
      ip,
      dbname = db_name,
      ssl_credentials = ssl_credentials,
      password = password,
      mysql_user = mysql_user
    )

    results <- conn$query(query_str)

    conn$disconnect()

    logging$info("saturn service query() got dimensions:", dim(results))

    return(results)
  }

  return(list(
    get_responses = get_responses,
    get_raw_responses = get_raw_responses,
    fake_responses = fake_responses,
    query = query
  ))
}
