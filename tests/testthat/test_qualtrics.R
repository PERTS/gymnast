context("qualtrics.R")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the gymnast directory
# * Run this command:
#   Rscript -e "testthat::auto_test('R', 'tests/testthat')"
#
# The test runner will watch your code files and re-run your tests when you
# change something. Get all those yummy green checks!

if (grepl("tests/testthat$", getwd())) {
  setwd("../..")  # root dir of gymnast repo
}

library(testthat)

bootstrap <- modules::use("R/bootstrap.R")
bootstrap$install_module_imports()

q <- import_module("qualtrics")

test_that("creates fake responses", {
  n_per_class <- 10
  classrooms_df <- data.frame(
    uid = c('Classroom_A', 'Classroom_B', 'Classroom_C'),
    code = c('foo', 'bar', 'baz'),
    num_students = rep(n_per_class, 3)
  )

  qs <- q$create_service('fake api key')
  fake_df <- qs$fake_responses(
    classrooms_df,
    start_date = Sys.Date(),
    end_date = Sys.Date() + 5
  )

  # +1 for the double header
  expect_equal(nrow(fake_df), (nrow(classrooms_df) * n_per_class) + 1)

  # Just a sample of the meta data we expect.
  expected_cols <- c('V1', 'participant_id', 'token', 'survey_id',
                     'code')
  expect_true(all(expected_cols %in% names(fake_df)))

  # Double header should have the right values.
  expect_equal(fake_df[1, "V1"], list("ResponseID"))
})
