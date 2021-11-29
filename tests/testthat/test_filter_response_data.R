context("filter response data")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the rserve directory
# * Run this command:
#   RScript -e "testthat::test_file('tests/testthat/test_filter_response_data.R')"
#
# The test runner will watch your code files and re-run your tests when you
# change something. Get all those yummy green checks!

if (grepl("tests/testthat", getwd())) {
  # Switch to rserve root, not the tests directory.
  setwd("../../")
}

perts_ids <- import_module("perts_ids")
util <- import_module("util")
filter_response_data <- import_module("filter_response_data")
json_utils <- import_module("json_utils")
modules::import("dplyr")
modules::import("jsonlite")
modules::import("lubridate")
modules::import("testthat")
`%+%` <- paste0


triton.classroom_base <- tibble(
  code = sample(c("code" %+% as.character(1:3))),
  uid = replicate(3, perts_ids$create_uid("Classroom")),
  team_id = replicate(3, perts_ids$create_uid("Team"))
) %>%
  util$prefix_columns('classroom')

triton.participant_base <- tibble(
  uid = replicate(100, perts_ids$create_uid(kind = "Participant")),
  code = sample(triton.classroom_base$classroom.code, 100, replace = T),
  classroom_id = util$recode(
    code,
    triton.classroom_base$classroom.code,
    triton.classroom_base$classroom.uid
  ),
  team_id = util$recode(
    code,
    triton.classroom_base$classroom.code,
    triton.classroom_base$classroom.team_id
  )
) %>%
  util$prefix_columns('participant') %>%
  rowwise() %>%
  mutate(participant.classroom_ids = jsonlite::toJSON(participant.classroom_id)) %>%
  ungroup() %>%
  select(-participant.code, -participant.classroom_id)

saturn_responses_base <- triton.participant_base %>%
  json_utils$expand_string_array_column(participant.classroom_ids) %>%
  mutate(
    code = util$recode(
      participant.classroom_ids,
      originals = triton.classroom_base$classroom.uid,
      replacements = triton.classroom_base$classroom.code
    )
  ) %>%
  select(
    -participant.classroom_ids,
    participant_id = participant.uid
  )

saturn_responses <- bind_rows(
  mutate(saturn_responses_base, StartDate = today()),
  mutate(saturn_responses_base, StartDate = today() - 7)
  ) %>%
  mutate(
    item1_response = sample(1:6, 200, replace = T),
    item2_response = sample(1:6, 200, replace = T)
  )

describe('tag_unfinished', {

  it('does not tag partial responses that are the only response for' %+%
       'a single participant', {

     response_data <- data.frame(participant_id = c(1),
                                 progress = c(1),
                                 cycle_ordinal = c(1),
                                 StartDate = "2021-08-31 11:48:22 CDT",
                                 testing = NA)

     response_data_tagged <- response_data %>%
       mutate(filter_progress = filter_response_data$tag_filter_progress(
         participant_id,
         cycle_ordinal,
         progress,
         StartDate))

     # the single row of this response data should not get tagged for removal,
     # because the partial response was the participant's only response
     expect_equal(response_data_tagged$filter_progress, FALSE)

    })

  it('does tag all partial responses when a complete response exists ' %+%
       'for the same participant/cycle combination', {

    response_data <- data.frame(participant_id = c(1, 1),
                               progress = c(1, 100),
                               cycle_ordinal = c(1, 1),
                               StartDate = c("2021-08-31 11:48:22 CDT",
                                             "2021-08-31 11:48:25 CDT"),
                               testing = NA)
    response_data_tagged <- response_data %>%
      mutate(filter_progress = filter_response_data$tag_filter_progress(
        participant_id,
        cycle_ordinal,
        progress,
        StartDate))

    # In this case, the first row should be tagged for removal because it has
    # progress-1, but the second row should remain because it is the only
    # complete response
    expected_tags <- c(TRUE, FALSE)
    expect_equal(response_data_tagged$filter_progress, expected_tags)


  })

  it('does not tag partial responses when no complete response exists' %+%
       'for the participant/cycle combination', {

    response_data <- data.frame(participant_id = c(1, 1),
                                progress = c(1, 100),
                                cycle_ordinal = c(1, 2),
                                StartDate = c("2021-08-31 11:48:22 CDT",
                                             "2021-08-31 11:48:25 CDT"),
                                testing = NA)
    response_data_tagged <- response_data %>%
      mutate(filter_progress = filter_response_data$tag_filter_progress(
        participant_id,
        cycle_ordinal,
        progress,
        StartDate))

    # neither row should be filtered out in this case, because both responses
    # represent the only response for a participant/cycle combination
    expected_tags <- c(FALSE, FALSE)
    expect_equal(response_data_tagged$filter_progress, expected_tags)

  })

  it('keeps only the last complete response in a given cycle when ' %+%
       'there are multiple complete responses', {
     # The first complete response is the one that contains demographic
     # data.

     response_data <- data.frame(participant_id = c(1, 1, 1),
                                 progress = c(100, 100, 100),
                                 cycle_ordinal = c(1, 2, 2),
                                 StartDate = c("2021-08-31 11:48:22 CDT",
                                               "2021-10-31 11:48:25 CDT",
                                               "2021-10-31 11:55:37 CDT"),
                                 testing = NA)

     response_data_tagged <- response_data %>%
       mutate(filter_progress = filter_response_data$tag_filter_progress(
         participant_id,
         cycle_ordinal,
         progress,
         StartDate))

     # only the middle row should be tagged for removal, because it's one of two
     # complete responses in cycle 2, and it's not the last one
     expected_tags <- c(FALSE, TRUE, FALSE)
     expect_equal(response_data_tagged$filter_progress, expected_tags)

  })
  it('when partial responses are needed, only the last one is kept per cycle', {

    response_data <- data.frame(participant_id = c(1, 1, 1),
                                progress = c(1, 1, 1),
                                cycle_ordinal = c(1, 2, 2),
                                StartDate = c("2021-08-31 11:48:22 CDT",
                                              "2021-10-31 11:48:25 CDT",
                                              "2021-10-31 11:55:37 CDT"),
                                testing = NA)

    response_data_tagged <- response_data %>%
      mutate(filter_progress = filter_response_data$tag_filter_progress(
        participant_id,
        cycle_ordinal,
        progress,
        StartDate))

    # only the second row should be filtered out in this case, because cycle 2
    # has two partial responses and the second row is not the last one
    expected_tags <- c(FALSE, TRUE, FALSE)
    expect_equal(response_data_tagged$filter_progress, expected_tags)

  })

  it('prioritizes partial/complete status over timestamp', {
    response_data <- data.frame(participant_id = c(1, 1),
                                progress = c(1, 100),
                                cycle_ordinal = c(1, 1),
                                StartDate = c("2021-08-31 11:48:22 CDT",
                                              "2021-07-31 11:48:25 CDT"),
                                testing = NA)
    # prove that the complete response happened before the partial response
    complete_timestamp <- response_data %>%
      filter(progress %in% 100) %>%
      pull(StartDate)
    partial_timestamp <- response_data %>%
      filter(progress %in% 1) %>%
      pull(StartDate)
    expect_true(complete_timestamp < partial_timestamp)

    # but we should still tag the partial-response row for removal
    response_data_tagged <- response_data %>%
      mutate(filter_progress = filter_response_data$tag_filter_progress(
        participant_id,
        cycle_ordinal,
        progress,
        StartDate))

    expected_tags <- c(TRUE, FALSE)
    expect_equal(response_data_tagged$filter_progress, expected_tags)
  })

  it('works with more data', {
    response_data <- data.frame(participant_id = c(1, 1, 2, 2, 3, 3),
                                progress = c(1, 1, 100, 100, 100, 1),
                                cycle_ordinal = c(1, 1, 1, 1, 1, 2),
                                StartDate = c("2021-08-31 11:48:22 CDT",
                                              "2021-08-31 11:50:25 CDT",
                                              "2021-08-31 12:48:25 CDT",
                                              "2021-08-31 13:28:25 CDT",
                                              "2021-08-31 10:46:25 CDT",
                                              "2021-09-31 11:48:25 CDT"),
                                testing = NA)
    response_data_tagged <- response_data %>%
      mutate(filter_progress = filter_response_data$tag_filter_progress(
        participant_id,
        cycle_ordinal,
        progress,
        StartDate))


    expected_tags <- c(
      # Participant 1/Cycle 1 has one partial responses, so tag only the first
      # one for removal
      TRUE, FALSE,
      # Participant 2/Cycle 1 has two complete responses, so tag only the first
      # one for removal
      TRUE, FALSE,
      # Participant 3 has only one response for each cycle, so neither are
      # tagged for removal
      FALSE, FALSE
    )
    expect_equal(response_data_tagged$filter_progress, expected_tags)

  })

  it('works with reversed row order', {
    response_data <- data.frame(participant_id = c(1, 1, 2, 2, 3, 3),
                                progress = c(1, 1, 100, 100, 100, 1),
                                cycle_ordinal = c(1, 1, 1, 1, 1, 2),
                                StartDate = c("2021-08-31 11:48:22 CDT",
                                              "2021-08-31 11:50:25 CDT",
                                              "2021-08-31 12:48:25 CDT",
                                              "2021-08-31 13:28:25 CDT",
                                              "2021-08-31 10:46:25 CDT",
                                              "2021-09-31 11:48:25 CDT"),
                                testing = NA) %>%
      dplyr::arrange(-dplyr::row_number())

    response_data_tagged <- response_data %>%
      mutate(filter_progress = filter_response_data$tag_filter_progress(
        participant_id,
        cycle_ordinal,
        progress,
        StartDate))


    expected_tags <- c(
      # Participant 3 has only one response for each cycle, so neither are
      # tagged for removal
      FALSE, FALSE,
      # Participant 2/Cycle 1 has two complete responses, so tag only the first
      # one for removal (appears second in the data now)
      FALSE, TRUE,
      # Participant 1/Cycle 1 has one partial response, so tag only the first
      # one for removal
      FALSE, TRUE
    )
    expect_equal(response_data_tagged$filter_progress, expected_tags)

  })


})


describe('validate_response_data', {

  it('throws an informative error for missing columns', {

    response_data <- data.frame(
      StartDate = as.Date(c("2021-06-25", "2021-06-26", "2021-06-27")),
      cycle_ordinal = c(1, NA, 1),
      testing = NA,
      progress = 100,
      participant_id = 1
    )
    response_data_no_sd <- response_data %>% select(-StartDate)
    response_data_no_co <- response_data %>% select(-cycle_ordinal)
    response_data_no_testing <- response_data %>% select(-testing)
    response_data_no_progress <- response_data %>% select(-progress)
    response_data_no_pid <- response_data %>% select(-participant_id)

    expect_error(filter_response_data$validate_response_data(response_data_no_sd),
                 regexp = "missing the following columns: StartDate")
    expect_error(filter_response_data$validate_response_data(response_data_no_co),
                 regexp = "missing the following columns: cycle_ordinal")
    expect_error(filter_response_data$validate_response_data(response_data_no_testing),
                 regexp = "missing the following columns: testing")
    expect_error(filter_response_data$validate_response_data(response_data_no_progress),
                 regexp = "missing the following columns: progress")
    expect_error(filter_response_data$validate_response_data(response_data_no_pid),
                 regexp = "missing the following columns: participant_id")

  })

  it('throws an informative error if response data is not uniquely indexed by ' %+%
       'participant_id, cycle_ordinal, and StartDate', {
    # While rare, some Saturn responses are recorded for the same participant in
    # the same second. These should be filtered out and show a warning.
    response_data <- data.frame(participant_id = c(1, 1),
                                progress = c(1, 100),
                                cycle_ordinal = c(1, 1),
                                StartDate = c("2021-08-31 11:48:22 CDT",
                                              "2021-08-31 11:48:22 CDT"),
                                testing = NA)
    # prove that the index is duplicated
    index_is_a_duplicate <- response_data %>%
      select(participant_id, cycle_ordinal, StartDate) %>%
      duplicated()
    expect_true(TRUE %in% index_is_a_duplicate)

    expect_warning(
      {
        filtered_data <- filter_response_data$filter_response_data(
          response_data,
          '2021-09-06'
        )
      },
      regexp = "Duplicated combined index"
    )

    # All but one of the duplicate rows are dropped.
    expect_equal(nrow(filtered_data), 1)
  })

})

describe('filter_response_data', {

  it('filters out testers', {
    response_data <- data.frame(StartDate = c("2021-08-31 11:48:22 CDT",
                                              "2021-08-31 11:50:25 CDT",
                                              "2021-08-31 12:48:25 CDT"),
                                cycle_ordinal = c(1, 1, 1),
                                testing = c("true", NA, NA),
                                progress = 100,
                                participant_id = c(1, 2, 3))

    expected_df <- response_data %>%
      filter(!testing %in% "true")

    filtered_response_data <- filter_response_data$filter_response_data(
      response_data,
      report_date = "2021-09-06"
    )
    expect_equal(filtered_response_data %>% select(-matches("^filter_")),
                 expected_df)

  })

  it('filters out uncycled data', {

    # un-cycled data would be anything with an NA value in the cycle_id field
    response_data <- data.frame(StartDate = c("2021-08-31 11:48:22 CDT",
                                              "2021-08-31 11:50:25 CDT",
                                              "2021-08-31 12:48:25 CDT"),
                                cycle_ordinal = c(1, NA, 1),
                                testing = NA,
                                progress = 100,
                                participant_id = 1)

    response_data_filtered <- filter_response_data$filter_response_data(
      response_data = response_data,
      report_date = "2021-07-05"
    )

  })

  it('returns a data.frame that is unique at the level of participant_id/cycle_ordinal', {

    response_data <- data.frame(
      StartDate = rep(c("2021-08-31 11:48:22 CDT", "2021-08-31 11:50:25 CDT",
                        "2021-08-31 12:48:25 CDT", "2021-09-30 12:48:25 CDT",
                        "2021-08-30 12:52:25 CDT", "2021-08-30 12:55:25 CDT"),
                      2),
      cycle_ordinal = rep(c(1, 1, 1, 2, 2, 2), 2),
      testing = NA,
      progress = rep(c(100, 100, 1, 100, 1, 100), 2),
      participant_id = c(rep(1, 6), rep(2, 6))
    )

    response_data_filtered <- filter_response_data$filter_response_data(
      response_data = response_data,
      report_date = "2021-10-04"
    )

    cycle_participant_is_duplicate <- response_data_filtered %>%
      select(participant_id, cycle_ordinal) %>%
      duplicated()

    expect_false(TRUE %in% cycle_participant_is_duplicate)

  })

  it('maintains all of the original participant_id/cycle_ordinal unique combinations', {
    response_data <- data.frame(
      StartDate = rep(c("2021-08-31 11:48:22 CDT", "2021-08-31 11:50:25 CDT",
                        "2021-08-31 12:48:25 CDT", "2021-09-30 12:48:25 CDT",
                        "2021-08-30 12:52:25 CDT", "2021-08-30 12:55:25 CDT"),
                      2),
      cycle_ordinal = rep(c(1, 1, 1, 2, 2, 2), 2),
      testing = NA,
      progress = rep(c(100, 100, 1, 100, 1, 100), 2),
      participant_id = c(rep(1, 6), rep(2, 6))
    )

    response_data_filtered <- filter_response_data$filter_response_data(
      response_data = response_data,
      report_date = "2021-10-04"
    )

    original_combos <- response_data %>%
      select(participant_id, cycle_ordinal) %>%
      unique() %>%
      arrange(participant_id, cycle_ordinal)
    # you have to scrub the row names or else they won't match
    rownames(original_combos) <- 1:nrow(original_combos)

    filtered_combos <- response_data_filtered %>%
      select(participant_id, cycle_ordinal) %>%
      unique() %>%
      arrange(participant_id, cycle_ordinal)
    # you have to scrub the row names or else they won't match
    rownames(filtered_combos) <- 1:nrow(filtered_combos)

    expect_equal(original_combos, filtered_combos)

  })

})

describe('simulated base data', {

  it('has response data with codes that all appear in the triton classroom table', {
    response_codes <- saturn_responses$code %>% unique()
    expect_equal(
      response_codes %in% triton.classroom_base$classroom.code,
      rep(TRUE, length(response_codes))
    )
  })

  it('has response data with participants that all appear in the triton participant table', {

    response_participants <- saturn_responses$participant_id %>% unique()
    expect_equal(
      response_participants %in% triton.participant_base$participant.uid,
      rep(TRUE, length(response_participants))
    )
  })

})

describe('filter_unrostered', {

  it('filters no rows when all students are rostered', {
    # show that all students are rostered in the simulated data
    rostered_only <- filter_response_data$filter_unrostered(
      saturn_data_input = saturn_responses,
      triton.classroom = triton.classroom_base,
      triton.participant = triton.participant_base
    )
    expect_equal(nrow(rostered_only), nrow(saturn_responses))

  })

  it('filters two rows (one per week in the data) when one participant is removed from the roster', {
    sample_removed_participant <- sample(triton.participant_base$participant.uid, 1)
    triton.participant_filtered <- triton.participant_base %>%
      mutate(participant.classroom_ids = ifelse(
        participant.uid %in% sample_removed_participant,
        jsonlite::toJSON(list()),
        participant.classroom_ids
      ))

    rostered_only <- filter_response_data$filter_unrostered(
      saturn_data_input = saturn_responses,
      triton.classroom = triton.classroom_base,
      triton.participant = triton.participant_filtered
    )
    expect_equal(nrow(rostered_only), nrow(saturn_responses) - 2)
  })

  it('filters just the relevant rows in the data when a participant is removed from one roster but not another', {

    sample_two_class_participant <- sample(triton.participant_base$participant.uid, 1)

    class_id_orig <- triton.participant_base %>%
      filter(participant.uid %in% sample_two_class_participant) %>%
      json_utils$expand_string_array_column(participant.classroom_ids) %>%
      pull(participant.classroom_ids)
    class_id_added <- triton.participant_base %>%
      json_utils$expand_string_array_column(participant.classroom_ids) %>%
      pull(participant.classroom_ids) %>%
      unique() %>%
      setdiff(., class_id_orig) %>%
      sample(., 1)

    old_code <- triton.classroom_base %>%
      filter(classroom.uid %in% class_id_orig) %>%
      pull(classroom.code)
    new_code <- triton.classroom_base %>%
      filter(classroom.uid %in% class_id_added) %>%
      pull(classroom.code)


    triton.participant_two_classes <- triton.participant_base %>%
      mutate(participant.classroom_ids = ifelse(
        participant.uid %in% sample_two_class_participant,
        jsonlite::toJSON(class_id_added),
        participant.classroom_ids
      ))

    # have the two-class participant participate in both classes
    saturn_responses_tcp <- saturn_responses %>%
       add_row(
         participant_id = sample_two_class_participant,
         code = new_code,
         item1_response = sample(1:6, 1),
         item2_response = sample(1:6, 1),
         StartDate = today()
       )

    # prove that the participant now has rows for both codes in the response data
    response_codes <- saturn_responses_tcp %>%
      filter(participant_id %in% sample_two_class_participant) %>%
      pull(code) %>%
      unique()
    expect_equal(sort(response_codes), sort(c(old_code, new_code)))

    # prove that the student was since removed from the first roster
    new_class_ids <- triton.participant_two_classes %>%
      filter(participant.uid %in% sample_two_class_participant) %>%
      json_utils$expand_string_array_column(participant.classroom_ids) %>%
      pull(participant.classroom_ids)
    expect_equal(new_class_ids, class_id_added)

    # now make sure the rostered-only data has rows for the second code but not
    # the first
    rostered_only <- filter_response_data$filter_unrostered(
      saturn_data_input = saturn_responses_tcp,
      triton.classroom = triton.classroom_base,
      triton.participant = triton.participant_two_classes
    )
    old_code_rows <- rostered_only %>%
      filter(
        participant_id %in% sample_two_class_participant,
        code %in% old_code
      )
    new_code_rows <- rostered_only %>%
      filter(
        participant_id %in% sample_two_class_participant,
        code %in% new_code
      )
    expect_equal(nrow(old_code_rows), 0)
    expect_equal(nrow(new_code_rows), 1)

  })

})
