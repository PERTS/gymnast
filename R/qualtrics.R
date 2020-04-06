# Communicate with the Qualtrics API
#
# Import as a module and create the service:
#
# qualtrics <- import_module("qualtrics")
# qualtrics_service <- qualtrics$create_service(api_key)
#
# Then use exported methods:
#
# * qualtrics_service$get_responses(survey_id) - returns a data frame of legacy-
#   format responses for that survey.

# packages: httr

modules::import('dplyr', `%>%`)

http_helpers <- import_module("http_helpers")
logging <- import_module("logging")
perts_ids <- import_module("perts_ids")

# Note the subdomains on this URL, re: "Cannot download response export file
# through datacenter proxing."
# https://www.qualtrics.com/community/discussion/763/trouble-using-the-api-to-get-survey-responses
# https://www.qualtrics.com/support/integrations/api-integration/finding-qualtrics-ids/#LocatingtheDatacenterID
QUALTRICS_DOMAIN <- 'https://sshs.ca1.qualtrics.com/API/v3/'

random_hex <- function (len) {
  hex_digits <- c(as.character(0:9), letters[1:6])
  paste0(sample(hex_digits, len, replace=TRUE), collapse='')
}

create_service <- function (api_key) {
  if (is.null(api_key)) {
    warning("qualtrics api key not found, won't be able to connect.")
  }
  get_export_id <- function(survey_key) {
    r <- httr::POST(
      paste0(QUALTRICS_DOMAIN, "responseexports"),
      body = list(
        "surveyId" = survey_key,
        "format" = "csv2013"
      ),
      httr::add_headers(
        "X-API-TOKEN" = api_key,
        "Content-Type" = "application/json"
      ),
      encode = "json"
    )

    response <- httr::content(r, "parsed", "application/json")
    logging$info("Requested export from Qualtrics:")
    logging$info(response)

    return(response$result$id)
  }
  get_file <- function(export_id){
    tmp <- paste0(tempfile(),".zip")
    r <- httr::GET(
      paste0(
        QUALTRICS_DOMAIN,
        "responseexports/",
        export_id,
        "/file"
      ),
      httr::add_headers("X-API-TOKEN" = api_key),
      httr::write_disk(tmp, overwrite=TRUE)
    )
    df <- utils::read.csv(utils::unzip(tmp))
    print("Dimensions of downloaded file: ")
    print(dim(df))
    return(df)
  }
  check_export <- function(export_id){
    r <- httr::GET(
      paste0(
        QUALTRICS_DOMAIN,
        "responseexports/",
        export_id
      ),
      httr::add_headers("X-API-TOKEN" = api_key)
    )
    response = httr::content(r, "parsed", "application/json")

    return(response$result)
  }

  get_responses <- function (survey_key) {
    print("qualtrics_service$get_responses()")
    export_id <- get_export_id(survey_key)
    http_helpers$retry(
      function () {
        result <- check_export(export_id)
        if (result$status != 'complete') {
          logging$info(paste0("status: '", result$status, "'"))
          if ('info' %in% names(result) && 'reason' %in% names(result$info)) {
            logging$info(result$info$reason)
          }
          stop("retry loop")
        }
      },
      tries = 12,
      delay = 30 # change to 60 if not testing
    )

    export_data <- get_file(export_id)

    return(export_data)
  }

  fake_responses <- function (classrooms_df, start_date, end_date) {
    # Generate plausible but fake qualtrics respones for any set of classrooms
    # in a given date range.
    #
    # Actual question responses (e.g. "strongly agree") are drawn from a set of
    # real but hand-deidentified responses to the Copilot survey from 2018. Then
    # important meta data and text responses are generated randomly, and they're
    # repeated as often as necessary, based on the number of classrooms (and
    # and their students) passed in.
    #
    # Note that the response, survey, and participant ids generated here will
    # NOT be mergeable to any other data because they're random, but they will
    # have the correct format and length.

    ANONYMOUS_COPILOT_RESPONSES <- utils::read.csv(
      'data/qualtrics_anonymous_sample_ep_2018.csv',
      stringsAsFactors = FALSE
    )
    # Drop the double header so we can treat all rows as real data to sample.
    responses_only <- ANONYMOUS_COPILOT_RESPONSES[-1, ]

    # Mapper functions for generating random fake data.
    get_response_id <- function (...) perts_ids$create_uid('R')
    get_start_date <- function (...) {
      if (is.character(start_date)) {
        start_date <- as.Date(start_date)
      }
      if (is.character(end_date)) {
        end_date <- as.Date(end_date)
      }
      day_span <- as.numeric(end_date - start_date)
      if (day_span < 0) {
        stop("qualtrics$fake_responses: start date must be before end date.")
      }
      response_date <- start_date + sample(0:day_span, 1)
      return(paste0(as.character(response_date), " 00:00:00"))
    }
    get_participant_id <- function (...)
      perts_ids$create_uid('Participant')
    get_token <- function (...) random_hex(40)
    get_survey_id <- function (...) perts_ids$create_uid('Survey')
    # Not used, but here for reference b/c CAM used it to replace the actual
    # text responses of the sample data.
    get_text_response <- function (...) {
      paragraph_population <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
      num_paragraphs <- sample(paragraph_population, 1)
      return(ifelse(
        num_paragraphs,
        stringi::stri_rand_lipsum(num_paragraphs, start_lipsum = FALSE),
        ""
      ))
    }

    fake_df <- data.frame()
    fake_for_class <- function (row) {
      # Generate as many responses as the number of students in this class,
      # starting with a random sample from the anonymous set. Then generate/fake
      # any needed meta data.
      #
      # The results are rbound to `fake_df`.

      class_df <- data.frame()
      # For simplicity, assume that the full roster of students participates.
      num_participating <- as.numeric(row[['num_students']])
      # If the anonymous set isn't long enough, sample it repeatedly.
      while (nrow(class_df) < num_participating) {
        num_remaining <- num_participating - nrow(class_df)
        num_to_sample <- ifelse(
          num_remaining < nrow(responses_only),
          num_remaining,
          nrow(responses_only)
        )
        class_df <- rbind(
          class_df,
          dplyr::sample_n(responses_only, num_to_sample)
        )
      }

      n <- 1:nrow(class_df)
      class_df <- dplyr::mutate(
        class_df,
        V1 = Map(get_response_id, n),  # ResponseID
        V8 = Map(get_start_date, n),   # StartDate
        V9 = V8,                       # EndDate
        participant_id = Map(get_participant_id, n),
        token = Map(get_token, n),
        survey_id = Map(get_survey_id, n),
        code = row[['code']]           # Must be mergeable, so use real codes.
      )
      fake_df <<- rbind(fake_df, class_df)
    }
    apply(classrooms_df, 1, fake_for_class)

    # Re-create the double header.
    fake_df <- rbind(ANONYMOUS_COPILOT_RESPONSES[1, ], fake_df)

    # We started with a 0x0 data frame, so we need to create the column names.
    names(fake_df) <- names(ANONYMOUS_COPILOT_RESPONSES)

    return(fake_df)
  }

  return(list(
    get_responses = ifelse(
      is.null(api_key),
      function (...) {
        print("qualtrics service has no api key, returning empty data frame.")
        return(data.frame())
      },
      get_responses
    ),
    fake_responses = fake_responses
  ))
}

# # You would use it like this...
#
# qualtrics_service <- qualtrics$create_service(api_key)
# responses <- qualtrics_service$get_responses(survey_id)
# faked <- qualtrics_service$fake_responses(classes, Sys.Date() - 7, Sys.Date())
