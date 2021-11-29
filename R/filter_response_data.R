modules::import("dplyr", `%>%`, "filter", "mutate")

logging <- import_module("logging")
util <- import_module("util")
json_utils <- import_module("json_utils")
modules::import("dplyr")

`%+%` <- paste0

tag_filter_progress <- function(participant_id, cycle_ordinal, progress, StartDate){

  # returns a vector of TRUE/FALSE values indicating whether a row should be
  # removed according to our rules for retaining complete and partial responses

  # define n_complete_responses and n_partial_responses _within_ participant_id
  # and cycle_ordinal, so that tagging decisions occur within those variables
  response_data_tagged <- data.frame(participant_id, cycle_ordinal, progress, StartDate) %>%
    group_by(participant_id, cycle_ordinal) %>%
    mutate(n_complete_responses = sum(progress %in% 100),
           n_partial_responses = sum(!progress %in% 100)) %>%
    # now mark whether each response is the LAST of its kind
    group_by(participant_id, cycle_ordinal, progress) %>%
    mutate(is_last_of_kind = StartDate == max(StartDate)) %>%
    ungroup()

  # by default, we DON'T filter out rows, so set filter_progress to FALSE by
  # default
  filter_progress <- rep(FALSE, nrow(response_data_tagged))

  ############
  ############
  # if there are complete responses, discard the partial responses and keep only
  # the last complete response

  # discard partial responses IF there's one complete response
  filter_progress[response_data_tagged$n_complete_responses > 0 &
                    !response_data_tagged$progress %in% 100] <- TRUE

  # keep the last complete response
  filter_progress[response_data_tagged$n_complete_responses > 0 &
                    response_data_tagged$progress %in% 100 &
                    !response_data_tagged$is_last_of_kind] <- TRUE

  ############
  ############
  # if there are no complete responses, tag all but the last partial response
  filter_progress[response_data_tagged$n_complete_responses == 0 &
                    !response_data_tagged$progress %in% 100 &
                    !response_data_tagged$is_last_of_kind] <- TRUE

  return(filter_progress)
}

validate_response_data <- function(response_data){

  # check for required columns
  required_columns <- c("StartDate", "cycle_ordinal", "testing", "progress",
                        "participant_id")
  missing_columns <- setdiff(required_columns, names(response_data))
  if(length(missing_columns) > 0){
    stop("response data cannot be filtered because it is missing the following columns: " %+%
           paste0(missing_columns, collapse = ", "))
  }
}

filter_response_data <- function(response_data, report_date) {
  # This function performs all operations that filter response-level data.

  validate_response_data(response_data)

  # We have seen cases where there are multiple responses for exactly the same
  # participant, cycle, and to-the-second StartDate. When this happens we can't
  # stop the whole job. Log a warning and filter out the duplicates.

  combined_index_is_duplicate <- response_data %>%
    select(participant_id, cycle_ordinal, StartDate) %>%
    duplicated()

  response_data_indexed <- response_data
  if(TRUE %in% combined_index_is_duplicate){
    warning(
      "Duplicated combined index: " %+% sum(combined_index_is_duplicate) %+%
        " responses had identical participant ids, cycles, " %+%
        "and start times. Dropping.")
    logging$info(response_data[combined_index_is_duplicate, "uid"])
    response_data_indexed <- response_data[!combined_index_is_duplicate, ]
  }

  # tag responses for removal. (We tag instead of just removing for logging
  # purposes.)
  response_data_tagged <- response_data_indexed %>%
    mutate(
      filter_after_reports = StartDate >= as.Date(report_date),
      filter_uncycled = is.na(cycle_ordinal),
      filter_testers = !util$is_blank(testing),
      filter_progress = tag_filter_progress(participant_id,
                                            cycle_ordinal,
                                            progress,
                                            StartDate))

  # log removals of responses from data and check resulting dataset
  n_removed_after_reports <- sum(response_data_tagged$filter_after_reports)
  n_removed_uncycled <- sum(response_data_tagged$filter_uncycled)
  n_removed_progress <- sum(response_data_tagged$filter_progress)

  if(n_removed_after_reports > 0) {
    logging$info("Some records were removed from the response data because the start " %+%
                   "date was later than the report date. N = " %+% n_removed_after_reports)
  }
  if(n_removed_uncycled > 0){
    logging$info("N = " %+% n_removed_uncycled %+% " of " %+% nrow(response_data_tagged) %+%
                  " total rows in the response data removed because responses did not fall " %+%
                  " within cycle dates.")
  }
  if(n_removed_progress > 0){
    logging$info(
      "N = " %+% n_removed_progress %+% " of " %+%
        nrow(response_data_tagged) %+%
        " total rows in the response data removed by tag_filter_progress " %+%
        " to achieve uniqueness by participant/cycle combination."
    )
  }

  response_data_filtered <- filter(
    response_data_tagged,
    !filter_after_reports,
    !filter_uncycled,
    !filter_testers,
    !filter_progress
  )

  return(response_data_filtered)
}

check_filtered_data_rows_exist <- function(response_data_filtered){
  # if data has 0 rows after filtering, exit with a message
  if (nrow(response_data_filtered) == 0 ) {
    return(paste0(
      "After filtering the response data there are no rows left ",
      "for processing. Exiting the metascript. Dimensions of data:",
      paste0(dim(response_data_filtered), collapse = ", ")
    ))
  }
}

filter_unrostered <- function(saturn_data_input, triton.participant, triton.classroom){
  saturn_filtered <- saturn_data_input %>%
    filter(code %in% triton.classroom$classroom.code)
  ppt_filtered <- triton.participant %>%
    filter(participant.team_id %in% unique(triton.classroom$classroom.team_id))

  # participant table contains classroom membership in the participant.classroom_ids
  # field. Note that this field can be blank if a participant is rostered in one
  # or more classes and then removed from ALL the classes in which they are rostered.
  # we filter these explicitly here for clarity, though they would probably be
  # dropped with the inner join anyways.
  pc_assc <- ppt_filtered %>%
    dplyr::filter(!participant.classroom_ids %in% '[]') %>%
    json_utils$expand_string_array_column(participant.classroom_ids) %>%
    select(
      participant_id = participant.uid,
      classroom_id = participant.classroom_ids
    ) %>%
    unique()

  pc_code_assc <- pc_assc %>%
    left_join(triton.classroom, by = c("classroom_id" = "classroom.uid")) %>%
    select(
      participant_id,
      code = classroom.code
    )

  saturn_rostered_only <- saturn_filtered %>%
    inner_join(
      pc_code_assc,
      by = c("participant_id", "code")
    )

  saturn_rostered_only
}