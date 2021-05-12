context("imputation.R")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the rserve directory
# * Run this command:
#   Rscript -e "testthat::auto_test('R', 'tests/testthat/test_imputation')"
#
# The test runner will watch your code files and re-run your tests when you
# change something. Get all those yummy green checks!

if (grepl('tests/testthat/test_imputation$', getwd())) {
  setwd('../../..')  # root dir of gymnast repo
}

library(testthat)
modules::import("dplyr")
`%+%` <- paste0

imputation <- import_module("imputation")

describe('imputation', {
  example_response_data_cycle1 <- tribble(
    ~participant_id,  ~team_id, ~cycle_name,         ~ResponseID, ~EndDate,              ~example_question,
    'Participant_A',  'Team_A', 'Cycle 01 (May. 6)', 'A',         '2020-05-12 12:00:00', 3
  )
  example_response_data_cycle2 <- tribble(
    ~participant_id,  ~team_id, ~cycle_name,          ~ResponseID, ~EndDate,              ~example_question,
    'Participant_A',  'Team_A', 'Cycle 02 (May. 13)', 'A',         '2020-05-13 12:00:00', 3
  )
  example_response_data_missing_cycle2 <- tribble(
    ~participant_id,  ~team_id, ~cycle_name,          ~ResponseID, ~EndDate,              ~example_question,
    'Participant_A',  'Team_A', 'Cycle 01 (May. 13)', 'A',         '2020-05-12 12:00:00', 3,
    'Participant_A',  'Team_A', 'Cycle 03 (May. 13)', 'A',         '2020-05-20 12:00:00', 3
  )
  imputed_cols <- 'example_question'

  it('drops unstarted cycle 2, after started cycle 1', {

    imputed <- imputation$impute_responses_downup(
      response_data = example_response_data_cycle1,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "participant_id"),
      cols_to_impute = imputed_cols,
      time_ordinal_scope_vars = "team_id"
    )

    # Result should NOT have added any data for cycle 2, so there's only one
    # response row.
    expect_equal(nrow(imputed), 1)
    # And the cycle of that one row should match the original.
    expect_equal(imputed$cycle_name, example_response_data_cycle1$cycle_name)
  })

  it('drops missing cycle 1, with started cycle 2', {

    imputed <- imputation$impute_responses_downup(
      response_data = example_response_data_cycle2,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "participant_id"),
      cols_to_impute = imputed_cols,
      time_ordinal_scope_vars = "team_id"
    )

    # Result should NOT have added any data for cycle 2, so there's only one
    # response row.
    expect_equal(nrow(imputed), 1)
    # And the cycle of that one row should match the original.
    expect_equal(imputed$cycle_name, example_response_data_cycle2$cycle_name)
  })

  it('drops missing cycle 2 between cycles 1 and 3 (when team has NO cycle 2 participation)', {

    imputed <- imputation$impute_responses_downup(
      response_data = example_response_data_missing_cycle2,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "participant_id"),
      cols_to_impute = imputed_cols,
      time_ordinal_scope_vars = "team_id"
    )

    # Result should NOT have added any data for cycle 2, so there's only one
    # response row.
    expect_equal(nrow(imputed), 2)
    # And the cycle of that one row should match the original.
    expect_equal(imputed$cycle_name, example_response_data_missing_cycle2$cycle_name)
  })

  it('imputes FORWARD within team/code across cycles (from cycle 1 to cycle 2)', {

    response_data <- tibble(
      participant_id = rep("Participant_" %+% 1:10, 2),
      team_id = "Team_1",
      code = "snake shark",
      cycle_name = c(rep("Cycle 01", 10), rep("Cycle 02", 10)),
      survey_var = sample(1:6, 20, replace = T)
    )
    # add in some missing data too...# delete a whole participant from cycle 1
    sample_participant <- sample(response_data$participant_id, 1)
    response_data_w_missing <- response_data %>%
      filter(!(grepl("2", cycle_name) & participant_id %in% sample_participant))
    expect_equal(nrow(response_data_w_missing), 19)

    response_data_imputed <- imputation$impute_responses_downup(
      response_data = response_data_w_missing,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "code", "participant_id"),
      cols_to_impute = "survey_var",
      time_ordinal_scope_vars = "team_id"
    )
    imputed_row <- response_data_imputed %>%
      dplyr::filter((grepl("2", cycle_name) & participant_id %in% sample_participant))

    expected_value <- response_data_imputed$survey_var[
      response_data_imputed$participant_id %in% sample_participant &
        response_data_imputed$cycle_name %in% "Cycle 01"
    ]
    expect_equal(imputed_row$survey_var, expected_value)
    expect_equal(imputed_row$imputed_row, TRUE)
  })

  it('imputes BACKWARDS', {
    response_data <- tibble(
      participant_id = rep("Participant_" %+% 1:10, 2),
      team_id = "Team_1",
      code = "snake shark",
      cycle_name = c(rep("Cycle 01", 10), rep("Cycle 02", 10)),
      survey_var = sample(1:6, 20, replace = T)
    )
    # add in some missing data too...# delete a whole participant from cycle 2
    sample_participant <- sample(response_data$participant_id, 1)
    response_data_w_missing <- response_data %>%
      filter(!(grepl("1", cycle_name) & participant_id %in% sample_participant))
    expect_equal(nrow(response_data_w_missing), 19)

    response_data_imputed <- imputation$impute_responses_downup(
      response_data = response_data_w_missing,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "code", "participant_id"),
      cols_to_impute = "survey_var",
      time_ordinal_scope_vars = "team_id"
    )
    imputed_row <- response_data_imputed %>%
      dplyr::filter((grepl("1", cycle_name) & participant_id %in% sample_participant))

    expect_equal(nrow(imputed_row), 1)
    expect_equal(imputed_row$imputed_row, TRUE)

    expect_equal(nrow(imputed_row), 1)

  })

  it('imputes data into the future before into the past', {
    response_data <- tibble(
      participant_id = c(rep("Participant_1", 4), rep("Participant_2", 4)),
      team_id = "Team_1",
      code = "snake shark",
      cycle_name = rep(c("Cycle 01", "Cycle 02", "Cycle 03", "Cycle 04"), 2),
      survey_var = sample(1:6, 8, replace = T)
    )
    # make sure the survey_var values for participant 1 are all unique,
    # otherwise the test will be flaky
    response_data$survey_var[response_data$participant_id %in% "Participant_1"] <-
      sample(1:6, 4, replace = F)
    expect_equal(
      length(unique(response_data$survey_var[response_data$participant_id %in% "Participant_1"])),
      length(response_data$survey_var[response_data$participant_id %in% "Participant_1"])
    )
    response_data_w_missing <- response_data %>%
      # filter out cycles 1,3,and 4 for JUST participant_1
      filter(!(
        grepl("1|3", cycle_name) &
        participant_id %in% "Participant_1"
      ))

    # note that all of the cycles still must be represented within code snake shark
    # otherwise imputation will not occur. (We don't impute to cycles that are not
    # active within the imputation_index vars...) Since these data only have one
    # team and one code, we'll look in the whole dataset
    expect_equal(
      c("Cycle 01", "Cycle 02", "Cycle 03", "Cycle 04") %in% response_data_w_missing$cycle_name,
      rep(TRUE, 4)
    )

    imputed <- imputation$impute_responses_downup(
      response_data_w_missing,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "code", "participant_id"),
      cols_to_impute = "survey_var",
      time_ordinal_scope_vars = "team_id"
    )

    # if we impute to the future before the past, then cycle 3 should get cycle
    # 2's value, and cycle 1 should also get cycle 2's value
    imputed_c1_value <- imputed$survey_var[
      imputed$participant_id %in% "Participant_1" &
      imputed$cycle_name %in% "Cycle 01"
    ]
    imputed_c3_value <- imputed$survey_var[
      imputed$participant_id %in% "Participant_1" &
      imputed$cycle_name %in% "Cycle 03"
    ]
    original_c2_value <- response_data_w_missing$survey_var[
      response_data_w_missing$participant_id %in% "Participant_1" &
      response_data_w_missing$cycle_name %in% "Cycle 02"
    ]

    expect_equal(imputed_c1_value, original_c2_value)
    expect_equal(imputed_c3_value, original_c2_value)
  })

  it('doesnt impute NA values', {
    response_data <- tibble(
      participant_id = c(rep("Participant_1", 4), rep("Participant_2", 4)),
      team_id = "Team_1",
      code = "snake shark",
      cycle_name = rep(c("Cycle 01", "Cycle 02", "Cycle 03", "Cycle 04"), 2),
      survey_var = sample(1:6, 8, replace = T)
    )
    # add an NA value for participant 1 to cycle 2
    response_data$survey_var[
      response_data$participant_id %in% "Participant_1" &
        response_data$cycle_name %in% "Cycle 02"
    ] <- NA
    # remove the cycle 3 row for participant 1
    response_data <- response_data %>%
      filter(!(
        participant_id %in% "Participant_1" &
          cycle_name %in% "Cycle 03"
      ))
    # the imputed value for Participant 1 cycle 3 should now match the value
    # for Participant 1 cycle 1, NOT the NA from Cycle 02
    expected_value <- response_data$survey_var[
      response_data$participant_id %in% "Participant_1" &
        response_data$cycle_name %in% "Cycle 01"
    ]
    imputed <- imputation$impute_responses_downup(
      response_data = response_data,
      imputation_index = c("team_id", "code", "participant_id"),
      time_ordinal_column = "cycle_name",
      cols_to_impute = "survey_var",
      time_ordinal_scope_vars = "team_id"
    )
    c3_imputed_value <- imputed %>%
      filter(
        participant_id %in% "Participant_1",
        cycle_name %in% "Cycle 03"
      ) %>%
      pull(survey_var)
    expect_false(is.na(c3_imputed_value))
    expect_equal(c3_imputed_value, expected_value)
  })

  it('keeps the same number of responses as before for cycle 1 when certain conditions apply', {
    # I cannot for the life of me figure out what conditions triggered this bug to
    # happen. These data are a doctored version of real data with values
    # switched up and identifiers removed because I was unable to recreate the
    # issue with fake data. In the real data, extra unwanted rows were being
    # generated, BUT ONLY
    offending_response_data <- read.csv("tests/testthat/test_imputation/simulated_offending_data.csv", stringsAsFactors = FALSE)

    imputed_cols <- c("example_question_1", "example_question_2")
    imputed <- imputation$impute_responses_downup(
      response_data = offending_response_data,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "code", "participant_id"),
      cols_to_impute = imputed_cols,
      time_ordinal_scope_vars = "team_id"
    )
    # the imputation function was basically inserting extra responses in the
    # data that were not expected given the structure of the data. This
    # checks whether that is still happening.
    expected_n_responses <- offending_response_data %>%
      dplyr::filter(code %in% "code 4") %>%
      nrow()
    imputed_n_responses <- imputed %>%
      dplyr::filter(code %in% "code 4") %>%
      nrow()
    expect_equal(expected_n_responses, imputed_n_responses)
  })


  it('DOES impute cycles when whole codes are missing a cycle, if the cycle is active according to time_ordinal_scope_vars', {
    # this time we're going to have TWO participation codes within the team,
    # each starting at different times
    response_data <- tibble(
      participant_id = c(
        c("Participant_" %+% 1:2),
        rep(c("Participant_" %+% 3:4), 2)
      ),
      team_id = "Team_1",
      code = c(
        rep("snake shark", 2),
        rep("dank bud", 4)
      ),
      # snake shark missed cycle 1
      cycle_name = c(
        rep("Cycle 02", 2),
        c(rep("Cycle 01", 2), rep("Cycle 02", 2))
      ),
      survey_var = sample(1:6, 6, replace = T)
    )

    imputed <- imputation$impute_responses_downup(
      response_data = response_data,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "code", "participant_id"),
      cols_to_impute = "survey_var",
      time_ordinal_scope_vars = "team_id"
    )
    # this should impute two rows
    expect_equal(nrow(imputed), 8)
  })

  it('handles cases when the same participants participated in different classes', {
    response_data <- tibble(
      participant_id = c(
        rep(c("Participant_" %+% 1:2), 2),
        rep(c("Participant_" %+% 2:3), 2)
      ),
      team_id = "Team_1",
      code = c(
        rep("snake shark", 4),
        rep("dank bud", 4)
      ),
      # snake shark missed cycle 1
      cycle_name = c(
        c(rep("Cycle 01", 2), rep("Cycle 02", 2)),
        c(rep("Cycle 01", 2), rep("Cycle 02", 2))
      ),
      survey_var = sample(1:6, 8, replace = T)
    )

    # participant 2 missed cycle 2 in snake shark but not dank bud
    response_data <- response_data %>%
      filter(!(
        participant_id %in% "Participant_2" &
          cycle_name %in% "Cycle 02" &
          code %in% "snake shark"
      ))

    # now get two pre-specified values for Particpant 2's cycle 1 participation
    # and insert them into the data so we can check which one was imputed
    c1_snake_shark_value <- 8
    c1_dank_bud_value <- 9

    response_data$survey_var[
      response_data$participant_id %in% "Participant_2" &
        response_data$cycle_name %in% "Cycle 01" &
        response_data$code %in% "snake shark"
    ] <- c1_snake_shark_value

    response_data$survey_var[
      response_data$participant_id %in% "Participant_2" &
        response_data$cycle_name %in% "Cycle 01" &
        response_data$code %in% "dank bud"
      ] <- c1_dank_bud_value

    # make sure this removed exactly one row from the response data
    expect_equal(nrow(response_data), 7)

    # now make sure the right one gets imputed
    imputed <- imputation$impute_responses_downup(
      response_data = response_data,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "code", "participant_id"),
      cols_to_impute = "survey_var",
      time_ordinal_scope_vars = "team_id"
    )
    imputed_snake_shark_value <- imputed %>%
      filter(
        participant_id %in% "Participant_2",
        code %in% "snake shark",
        cycle_name %in% "Cycle 02"
      ) %>%
      pull(survey_var)

    expect_equal(imputed_snake_shark_value, c1_snake_shark_value)

  })

  # two cycles, two participants, one of them answers all the questions

  it('doesnt impute data across questions', {

    response_data <- tibble(
      participant_id = c(rep("Participant_1", 2), rep("Participant_2", 2)),
      team_id = "Team_1",
      code = "snake shark",
      cycle_name = rep(c("Cycle 01", "Cycle 02"), 2),
      survey_var1 = sample(1:6, 4, replace = T),
      survey_var2 = sample(1:6, 4, replace = T)
    )

    # give participant 1 a blank response to question 2 in cycle 1, and NO response in
    # cycle 2.
    response_data$survey_var2[
      response_data$participant_id %in% "Participant_1" &
        response_data$cycle_name %in% "Cycle 01"
    ] <- NA
    response_data <- response_data %>%
      filter(!(
        participant_id %in% "Participant_1" &
          cycle_name %in% "Cycle 02"
      ))
    imputed <- imputation$impute_responses_downup(
      response_data = response_data,
      time_ordinal_column = "cycle_name",
      imputation_index = c("team_id", "code", "participant_id"),
      cols_to_impute = c("survey_var1", "survey_var2"),
      time_ordinal_scope_vars = "team_id"
    )

    q1_c1_original <- response_data$survey_var2[
      response_data$participant_id %in% "Participant_1" &
        response_data$cycle_name %in% "Cycle 01"
      ]
    q1_c2_imputed <- imputed$survey_var2[
      imputed$participant_id %in% "Participant_1" &
        imputed$cycle_name %in% "Cycle 02"
    ]
    # both should be the same, and they should be NA
    expect_equal(q1_c1_original, q1_c2_imputed)
    expect_true(is.na(q1_c2_imputed))

  })

  it('extends time scope beyond individual questions', {
    # Imputation should happen across all time for what questions are active, not
    # within units of time independently. A question answered by anyone at time
    # ordinal 1 but missing entirely for time ordinal 2 SHOULD be imputed in
    # time ordinal 2. So if John takes the survey in the week of March 7
    # (week_start == "2021-03-07"), and but only answers half of the survey
    # questions, his responses to the questions he didn't answer should be
    # carried forward from the previous observation.

    rd <- data.frame(
      week_start = as.Date(c("2021-02-28", "2021-03-07")),
      participant_id = c("Participant_1", "Participant_1"),
      q1 = c(2, 3),
      q2 = c(4, NA),
      parent_id = "Network_1"
    )

    rdi <- imputation$impute_to_time_ordinal(
      response_data = rd,
      imputation_index = c("participant_id", "parent_id"),
      time_ordinal_column = "week_start",
      cols_to_impute = c("q1", "q2"),
      time_ordinal_scope_vars = "parent_id"
    )

    # assert that the imputed value for Participant_1 for q2 for 2021-03-07
    # should be 4
    actual_q2_w2 <- rdi %>%
      filter(week_start %in% as.Date("2021-03-07"),
             question_code %in% "q2") %>%
      pull(value_imputed)
    expect_equal(actual_q2_w2, 4)
  })

  it('imputes across time units and participants', {
    rd <- data.frame(
      week_start = as.Date(c("2021-02-28", "2021-02-28", "2021-03-07")),
      participant_id = c("Participant_1", "Participant_2", "Participant_1"),
      q1 = c(2, 1, 3),
      q2 = c(4, 2, NA),
      parent_id = "Network_1"
    )

    rdi <- imputation$impute_to_time_ordinal(
      response_data = rd,
      imputation_index = c("participant_id", "parent_id"),
      time_ordinal_column = "week_start",
      cols_to_impute = c("q1", "q2"),
      time_ordinal_scope_vars = "parent_id"
    )

    # The fact that participant 1 answered q1 at time 2 should trigger
    # imputation of ALL questions (q1 and q2) for ALL participants (1 and 2)
    # into time 2.
    actual_w2 <- rdi %>%
      filter(week_start %in% as.Date("2021-03-07")) %>%
      select(
        participant_id,
        question_code,
        value_imputed
      )
    expected_w2 <- dplyr::tribble(
      ~participant_id, ~question_code, ~value_imputed,
      "Participant_1", "q1",           3,
      "Participant_1", "q2",           4,
      "Participant_2", "q1",           1,
      "Participant_2", "q2",           2,
    )
    expect_equal(actual_w2, expected_w2)
  })

  it('throws an informative error when imputation index columns are not present', {
    rd <- data.frame(
      week_start = as.Date(c("2021-02-28", "2021-03-07")),
      participant_id = c("Participant_1", "Participant_1"),
      q1 = c(2, 3),
      q2 = c(4, 3)
    )

    expect_error(
      imputation$impute_to_time_ordinal(
        response_data = rd,
        imputation_index = c("participant_id", "parent_id"),
        time_ordinal_column = "week_start",
        cols_to_impute = c("q1", "q2"),
        time_ordinal_scope_vars = "parent_id"
      ),
      regexp = "columns in the imputation index were not found"
    )

  })

  it('throws an informative error when time ordinal columns are not present', {
    rd <- data.frame(
      participant_id = c("Participant_1", "Participant_1"),
      q1 = c(2, 3),
      q2 = c(4, 3),
      parent_id = "Network_1"
    )

    expect_error(
      imputation$impute_to_time_ordinal(
        response_data = rd,
        imputation_index = c("participant_id", "parent_id"),
        time_ordinal_column = "week_start",
        cols_to_impute = c("q1", "q2"),
        time_ordinal_scope_vars = "parent_id"
      ),
      regexp = "time ordinal column was not found"
    )

  })

  it('checks the uniqueness of the imputation index and throws an error if it is not unique', {
    rd <- data.frame(
      week_start = as.Date(c("2021-02-28", "2021-03-07", "2021-03-07")),
      participant_id = c("Participant_1", "Participant_1", "Participant_1"),
      q1 = c(2, 3, 4),
      q2 = c(4, 3, 4),
      parent_id = "Network_1"
    )
    imputation_index_and_time_ordinal = c("participant_id", "parent_id", "week_start")
    # prove that the last row duplicates values of the imputation index within time ordinal
    expect_equal(duplicated(rd[c(imputation_index_and_time_ordinal)]), c(FALSE, FALSE, TRUE))

    expect_error(imputation$impute_to_time_ordinal(
      response_data = rd,
      imputation_index = c("participant_id", "parent_id"),
      time_ordinal_column = "week_start",
      cols_to_impute = c("q1", "q2"),
      time_ordinal_scope_vars = "parent_id"
    ))
  })

})