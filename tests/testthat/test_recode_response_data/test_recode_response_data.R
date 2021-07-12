context("recode_response_data.R")

# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the rserve directory
# * Run this command:
#   Rscript -e "testthat::auto_test('R', 'tests/testthat/test_recode_response_data')"
#
# The test runner will watch your code files and re-run your tests when you
# change something. Get all those yummy green checks!

if (grepl('tests/testthat/test_recode_response_data$', getwd())) {
  setwd('../../..')  # root dir of gymnast repo
}

library(testthat)
modules::import("dplyr")
modules::import("tibble")
util <- import_module("util")
perts_ids <- import_module("perts_ids")
recode_response_data <- import_module("recode_response_data")
`%+%` <- paste0

describe('define_gender_disadvantage', {

  it('recodes "female" and values to disadv = TRUE',{
    gender <- c("female", "male")
    expect_identical(
      recode_response_data$define_gender_disadvantage(gender),
      c(TRUE, FALSE)
    )
  })

  it('recodes "other" to NA', {
    gender <- c("female", "male", "other")
    expect_identical(
      recode_response_data$define_gender_disadvantage(gender),
      c(TRUE, FALSE, NA)
    )
  })

  it('leaves NA blank', {
    gender <- c(NA)
    expect_identical(
      recode_response_data$define_gender_disadvantage(gender),
      c(NA)
    )
  })

})


describe('define_race_disadvantage', {

  race_config <- tribble(
    ~question_code, ~race_disadv,
    "race_example1_disadv", TRUE,
    "race_example1_adv", FALSE,
    "race_example2_disadv", TRUE,
    "race_example2_adv", FALSE
  )

  rd_base <- data.frame(
    matrix(ncol = nrow(race_config), nrow = 4, data = NA)
    ) %>%
    setNames(race_config$question_code)

  it('classifies identification with any structurally disadv backgrounds as disadv', {
    rd <- rd_base %>%
      dplyr::mutate(race_example1_disadv = TRUE)
    recoded <- recode_response_data$define_race_disadvantage(rd, race_config)
    expect_identical(recoded, rep(TRUE, nrow(rd)))
  })

  it('classifies identifiaction with structurally adv backgrounds as adv',{
    rd <- rd_base %>%
      dplyr::mutate(race_example1_adv = TRUE)
    recoded <- recode_response_data$define_race_disadvantage(rd, race_config)
    expect_identical(recoded, rep(FALSE, nrow(rd)))
  })

  it('prioritizes identification with disadv over adv', {
    rd <- rd_base %>%
      dplyr::mutate(
        race_example1_disadv = TRUE,
        race_example1_adv = TRUE
      )
    recoded <- recode_response_data$define_race_disadvantage(rd, race_config)
    expect_identical(recoded, rep(TRUE, nrow(rd)))
  })

  it('returns NA when none of the race boxes were checked', {
    recoded <- recode_response_data$define_race_disadvantage(rd_base, race_config)
    expect_identical(recoded, rep(NA, nrow(rd_base)))
  })

  it('treats FALSE-only rows the same as NA-only rows', {
    rd <- rd_base %>%
      dplyr::mutate(race_example1_disadv = FALSE)
    recoded <- recode_response_data$define_race_disadvantage(rd, race_config)
    expect_identical(recoded, rep(NA, nrow(rd)))
  })

  it('treats rows with FALSE and TRUE as the TRUE box being checked and FALSE not being checked', {
    rd <- rd_base[1:2, ] %>%
      dplyr::mutate(
        race_example1_disadv = c(TRUE, FALSE),
        race_example1_adv = c(FALSE, TRUE)
      )
    recoded <- recode_response_data$define_race_disadvantage(rd, race_config)
    names(recoded) <- NULL
    expect_identical(recoded, c(TRUE, FALSE))
  })

})


describe('define_high_fin_stress', {

  items <- data.frame(
    question_code = c("food_insecure", "fin_insecure_" %+% 1:4),
    financial_stress = TRUE,
    stringsAsFactors = FALSE
  )

  it('classifies as high stress when food but not fin insecurity is reported', {
    rd <- data.frame(
      food_insecure = c('often', 'sometimes', 'never'),
      fin_insecure_1 = c(NA, NA, NA),
      fin_insecure_2 = c(NA, NA, NA),
      fin_insecure_3 = c(NA, NA, NA),
      fin_insecure_4 = c(NA, NA, NA),
      stringsAsFactors = FALSE
    )
    expect_identical(
      recode_response_data$define_high_fin_stress(rd, items),
      c(TRUE, TRUE, FALSE)
    )
  })

  it('classifies as high stress when fin but not food insecurity is reported', {
    rd <- data.frame(
      food_insecure = rep('never', 4),
      fin_insecure_1 = c(TRUE, NA, NA, NA),
      fin_insecure_2 = c(NA, TRUE, NA, NA),
      fin_insecure_3 = c(NA, NA, TRUE, NA),
      fin_insecure_4 = c(NA, NA, NA, TRUE),
      stringsAsFactors = FALSE
    )
    expect_identical(
      recode_response_data$define_high_fin_stress(rd, items),
      c(TRUE, TRUE, TRUE, TRUE)
    )
  })

  it('classifies as high stress when fin AND food insecurity are reported', {
    rd <- data.frame(
      food_insecure = rep('often', 4),
      fin_insecure_1 = c(TRUE, NA, NA, NA),
      fin_insecure_2 = c(NA, TRUE, NA, NA),
      fin_insecure_3 = c(NA, NA, TRUE, NA),
      fin_insecure_4 = c(NA, NA, NA, TRUE),
      stringsAsFactors = FALSE
    )
    # write the test
    expect_identical(
      recode_response_data$define_high_fin_stress(rd, items),
      c(TRUE, TRUE, TRUE, TRUE)
    )
  })


  it('classifies as low stress when low food and fin stress were reported', {
    rd <- data.frame(
      food_insecure = rep('never', 4),
      fin_insecure_1 = NA,
      fin_insecure_2 = NA,
      fin_insecure_3 = NA,
      fin_insecure_4 = NA,
      stringsAsFactors = FALSE
    )
    expect_false(any(recode_response_data$define_high_fin_stress(rd, items)))
  })

  it('handles FALSE values appropriately', {
    rd <- data.frame(
      food_insecure = rep('never', 4),
      fin_insecure_1 = c(TRUE, NA, NA, NA),
      fin_insecure_2 = c(NA, FALSE, NA, NA),
      fin_insecure_3 = c(NA, NA, TRUE, NA),
      fin_insecure_4 = c(NA, NA, NA, TRUE),
      stringsAsFactors = FALSE
    )
    expect_identical(
      recode_response_data$define_high_fin_stress(rd, items),
      c(TRUE, FALSE, TRUE, TRUE)
    )
  })

  it('returns NA when nothing was reported', {
    rd <- data.frame(
      food_insecure = c(NA, NA),
      fin_insecure_1 = c(NA, NA),
      fin_insecure_2 = c(NA, NA),
      fin_insecure_3 = c(NA, NA),
      fin_insecure_4 = c(NA, NA),
      stringsAsFactors = FALSE
    )
    expect_identical(
      recode_response_data$define_high_fin_stress(rd, items),
      c(NA, NA)
    )
  })

  it('complains about unexpected food_insecure values', {
    rd <- data.frame(
      food_insecure = c("angry", "birds"),
      fin_insecure_1 = c(NA, NA),
      fin_insecure_2 = c(NA, NA),
      fin_insecure_3 = c(NA, NA),
      fin_insecure_4 = c(NA, NA),
      stringsAsFactors = FALSE
    )
    expect_error(recode_response_data$define_high_fin_stress(rd, items))
  })

  it('complains about non-boolean fin_insecure values', {
    rd <- data.frame(
      food_insecure = rep('never', 4),
      fin_insecure_1 = c("slow", "as", "molasses", NA),
      fin_insecure_2 = c(NA, FALSE, NA, NA),
      fin_insecure_3 = c(NA, NA, TRUE, NA),
      fin_insecure_4 = c(NA, NA, NA, TRUE),
      stringsAsFactors = FALSE
    )
    expect_error(recode_response_data$define_high_fin_stress(rd, items))
  })

  it('ignores with non-fin-stress rows in the items spec', {
    rd <- data.frame(
      food_insecure = rep('often', 4),
      fin_insecure_1 = c(TRUE, NA, NA, NA),
      fin_insecure_2 = c(NA, TRUE, NA, NA),
      fin_insecure_3 = c(NA, NA, TRUE, NA),
      fin_insecure_4 = c(NA, NA, NA, TRUE),
      stringsAsFactors = FALSE
    )
    items <- data.frame(
      question_code = c("food_insecure", "fin_insecure_" %+% 1:4, "other_item"),
      financial_stress = c(rep(TRUE, 5), FALSE),
      stringsAsFactors = FALSE
    )
    recoded <- recode_response_data$define_high_fin_stress(rd, items)
    expect_identical(recoded, rep(TRUE, 4))
  })

  it('handles old values (1,2,3) mixed with new values c("often", "sometimes", "never")', {
    rd <- data.frame(
      food_insecure = c('often', 'sometimes', 'never', '1', '2', '3'),
      fin_insecure_1 = NA,
      fin_insecure_2 = NA,
      fin_insecure_3 = NA,
      fin_insecure_4 = NA,
      stringsAsFactors = FALSE
    )
    recoded <- recode_response_data$define_high_fin_stress(rd, items)
    expect_equal(recoded, c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
  })

})


describe('compute_scale_composites', {

  items_elevate20 <- read.csv("tests/testthat/test_recode_response_data/copilot_items_sample.csv", stringsAsFactors = FALSE) %>%
    dplyr::filter(elevate20)
  subsets_elevate20 <- read.csv("tests/testthat/test_recode_response_data/copilot_subset_config_sample.csv", stringsAsFactors = FALSE) %>%
    dplyr::filter(elevate20)
  response_data_wdem <- read.csv(
    "tests/testthat/test_recode_response_data/sample_responses_elevate20.csv",
    stringsAsFactors = FALSE
  )
  response_data_recoded <- recode_response_data$recode_response_data(
    response_data_wdem,
    items_elevate20,
    subsets_elevate20
  )

  it('produces one column for each composite with the elevate20 specs', {
    expected_composites <- items_elevate20$question_code[items_elevate20$is_composite]
    found_composites <- response_data_recoded %>%
      dplyr::select(dplyr::one_of(expected_composites)) %>%
      names()
    expect_equal(sort(expected_composites), sort(found_composites))
  })

  it('computes the right values for a sample composite', {
    # I'm just checking them all to make sure I got the config right...
    compare_to_manual <- function(response_data, composite_inputs, composite_col){
      sample_response <- response_data$ResponseID[sample(1:nrow(response_data), 1)]
      sample_composite_inputs <- response_data_wdem %>%
        dplyr::filter(ResponseID %in% sample_response) %>%
        dplyr::select(dplyr::one_of(composite_inputs))
      manual_composite <- rowMeans(sample_composite_inputs)
      computed_composite <- response_data %>%
        dplyr::filter(ResponseID %in% sample_response) %>%
        dplyr::pull(composite_col)
      expect_equal(manual_composite, computed_composite)
    }
    compare_to_manual(
      response_data_recoded,
      c("cid_proud", "cid_learned", "cid_others"),
      "affirming_cultural_identity"
    )
    compare_to_manual(
      response_data_recoded,
      c("fg1_2", "fg2_2", "fg3_2"),
      "feedback_for_growth"
    )
    compare_to_manual(
      response_data_recoded,
      c("mw1_2", "mw2_2", "mw3_2"),
      "meaningful_work"
    )
    compare_to_manual(
      response_data_recoded,
      c("voice_choice", "voice_idea", "voice_suggestions"),
      "student_voice"
    )
    compare_to_manual(
      response_data_recoded,
      c("c_belonging_classmates", "c_belonging_teacher", "c_belonging_thoughts"),
      "classroom_belonging"
    )
    compare_to_manual(
      response_data_recoded,
      c("tc1_2", "tc2_2", "tc4_2"),
      "teacher_caring"
    )
  })

})


describe('validate_participant_tbl', {

  it('stops when required columns are missing', {

    participant_tbl <- data.frame(
      participant.uid = replicate(20, perts_ids$create_uid("Participant")),
      participant.stripped_student_id = 1:20
    )

    required_columns <- c("participant.uid", "participant.stripped_student_id")
    expect_error(recode_response_data$validate_participant_tbl(select(
      participant_tbl, -participant.uid
    )))
    expect_error(recode_response_data$validate_participant_tbl(select(
      participant_tbl, -participant.stripped_student_id
    )))

  })

  it('stops when participant_tbl contains no data', {
    participant_tbl_no_data <- data.frame(matrix(ncol = 2, nrow = 0)) %>%
      stats::setNames(c("participant.uid", "participant.stripped_student_id"))
    expect_error(recode_response_data$validate_participant_tbl(participant_tbl_no_data),
                 regexp = "contains no data")
  })

  it('stops when participant.uid field does not contain valid participant ids', {
    participant_tbl_correct_ids <- data.frame(
      participant.uid = replicate(20, perts_ids$create_uid("Participant")),
      participant.stripped_student_id = 1:20
    )
    participant_tbl_non_ids <- data.frame(
      participant.uid = 1:20,
      participant.stripped_student_id = 1:20
    )
    participant_tbl_wrong_ids <- data.frame(
      participant.uid = replicate(20, perts_ids$create_uid("Classroom")),
      participant.stripped_student_id = 1:20
    )
    # should throw no error for the correct ids
    recode_response_data$validate_participant_tbl(participant_tbl_correct_ids)
    # but should throw an error for non-PERTS-ids
    expect_error(recode_response_data$validate_participant_tbl(participant_tbl_non_ids),
                 regexp = "values were found in the participant.uid field that were not PERTS ids")
    # and for the wrong kind of PERTS ids
    expect_error(recode_response_data$validate_participant_tbl(participant_tbl_wrong_ids),
                 regexp = "the participant.uid field contains ids that are not participant_ids")
  })

})


describe('get_logins_for_participants', {

  it('returns a vector of the same length as the input vector when overlap is perfect', {

    participant_tbl <- data.frame(
      participant.uid = replicate(20, perts_ids$create_uid("Participant")),
      participant.stripped_student_id = 1:20
    )
    participant_ids <- participant_tbl$participant.uid
    hashed_logins <- recode_response_data$get_logins_for_participants(
      participant_ids,
      participant_tbl
    )

    expect_equal(length(hashed_logins), length(participant_ids))

  })

  it('returns a vector of the same length as the input vector when rostered ids dont appear in the data', {
    participant_tbl <- data.frame(
      participant.uid = replicate(20, perts_ids$create_uid("Participant")),
      participant.stripped_student_id = 1:20
    )
    participant_ids <- participant_tbl$participant.uid[1:10]
    hashed_logins <- recode_response_data$get_logins_for_participants(
      participant_ids,
      participant_tbl
    )

    expect_equal(length(hashed_logins), length(participant_ids))

  })


  it('returns a vector of the same length as the input vector when participant_ids are not found on the roster', {
    # these values should be blank in the output
    participant_tbl <- data.frame(
      participant.uid = replicate(20, perts_ids$create_uid("Participant")),
      participant.stripped_student_id = 1:20
    )
    new_participant <- perts_ids$create_uid("Participant")
    participant_ids <- c(participant_tbl$participant.uid, new_participant)
    hashed_logins <- recode_response_data$get_logins_for_participants(
      participant_ids,
      participant_tbl
    )
    expect_equal(length(hashed_logins), length(participant_ids))
    expect_equal(hashed_logins[21], as.character(NA))
  })

  it('returns values that are hashes instead of the original values', {
    participant_tbl <- data.frame(
      participant.uid = replicate(20, perts_ids$create_uid("Participant")),
      participant.stripped_student_id = 1:20
    )
    new_participant <- perts_ids$create_uid("Participant")
    participant_ids <- c(participant_tbl$participant.uid, new_participant)
    hashed_logins <- recode_response_data$get_logins_for_participants(
      participant_ids,
      participant_tbl
    )
    sample_hash <- hashed_logins[1]
    # should be a character vector of length-64 elements
    expect_equal(nchar(sample_hash), 64)

  })

})

describe('resolve_conflicting_demographics', {

  it('returns the existing value when values DONT conflict', {

    logins <- c(1, 2, 3, 3, 4, 5)
    gender_cat <- c("Boy/Man", "Girl/Woman", "Girl/Woman", "Girl/Woman",
                    "Boy/Man", "Boy/Man")
    login_hashes <- recode_response_data$salt_n_hash(logins, "salt")

    # prove that the third and fourth login hash value are duplicated
    expect_equal(login_hashes[3], login_hashes[4])
    # and that the third and fourth login hash value are identical
    expect_equal(gender_cat[3], gender_cat[4])

    unconflicted_demog <- recode_response_data$resolve_conflicting_demographics(
      login_hashes,
      gender_cat
    )
    expect_equal(unconflicted_demog, gender_cat)

  })

  it('returns the shared non-blank value when the only conflict is a blank', {
    logins <- c(1, 2, 3, 3)
    gender_cat <- c("Boy/Man", "Girl/Woman", NA, "Girl/Woman")
    login_hashes <- recode_response_data$salt_n_hash(logins, "salt")

    # prove that the third and fourth login hash value are duplicated
    expect_equal(login_hashes[3], login_hashes[4])
    # prove that the fourth value is non-blank
    expect_false(is.na(gender_cat[4]))
    # and that the third value is blank
    expect_true(is.na(gender_cat[3]))

    # the blank value should be filled in with "Girl/Woman"
    expected_unconflicted_demog <- c("Boy/Man", "Girl/Woman", "Girl/Woman", "Girl/Woman")

    unconflicted_demog <- recode_response_data$resolve_conflicting_demographics(
      login_hashes,
      gender_cat
    )
    expect_equal(unconflicted_demog, expected_unconflicted_demog)

  })

  it('returns NA when two non-blank values conflict', {

  })

})