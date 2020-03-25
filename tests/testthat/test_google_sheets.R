context("Google Sheets")
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

modules::use("R/bootstrap.R")$install_module_imports()
gs <- import_module("google_sheets")

# Put your credentials here in order to run integration tests.
testing_credentials <- list('foo')

# !! Integration tests are commented out because they take real actions in
# the cloud and should not be run willy-nilly.

# test_that("INTEGRATION: can create a sheet", {
#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_id <- gs_conn$create()
#   expect_equal(typeof(sheet_id), "character")
#   expect_equal(length(sheet_id), 1)

#   gs_conn$delete(sheet_id)  # clean up
# })

# test_that("INTEGRATION: can share a sheet", {
#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_id <- gs_conn$create()

#   Sys.sleep(1)  # seems not available for sharing for a tick, so wait

#   gs_conn$share(sheet_id, 'chris@perts.net')
#   permissions <- gs_conn$list_collaborators(sheet_id)

#   expect_equal(length(permissions), 2)  # rserve (owner) + sharee

#   gs_conn$delete(sheet_id)  # clean up
# })

# test_that("INTEGRATION: can read example data", {
#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_id <- gs_conn$create()
#   result <- gs_conn$read(sheet_id, 'Sheet1!A1:A2')

#   # Expect values to be empty because we created a blank sheet.
#   expect_equal(is.null(result), TRUE)

#   gs_conn$delete(sheet_id)  # clean up
# })

# test_that("INTEGRATION: writes to A1 by default", {
#   example_df <- mtcars

#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_id <- gs_conn$create("RServe overwrite test")
#   result <- gs_conn$overwrite(sheet_id, example_df)

#   expect_equal(result$updatedRows, nrow(example_df) + 1)  # +1 for col headers
#   expect_equal(result$updatedColumns, ncol(example_df))  # no rownames

#   gs_conn$delete(sheet_id)  # clean up
# })

# test_that("INTEGRATION: can write to arbitrary ranges in sheet", {
#   example_df <- mtcars

#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_id <- gs_conn$create("RServe overwrite test")
#   result <- gs_conn$overwrite(sheet_id, example_df, range_begin = 'B4')

#   # N.B. this isn't a very good test since we don't confirm the _position_
#   # of the data written. CAM notes he's tested this by manually sharing and
#   # viewing the sheet, 2019-03-04.
#   expect_equal(result$updatedRows, nrow(example_df) + 1)  # +1 for col headers
#   expect_equal(result$updatedColumns, ncol(example_df))  # no rownames

#   gs_conn$delete(sheet_id)  # clean up
# })

# test_that("INTEGRATION: fails if sheet doesn't exist", {
#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   expect_error(
#     gs_conn$read('sheet dne', 'Sheet1!A1:A2'),
#     "^Error"
#   )
# })

# test_that("INTEGRATION: lists all files", {
#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_name <- "RServe list test"
#   sheet_id <- gs_conn$create(sheet_name)
#   sheet_ids <- gs_conn$list_sheets()

#   # Freshly created sheet is in returned list, named correctly.
#   expect_equal(sheet_id %in% sheet_ids, TRUE)
#   expect_equal(names(sheet_ids[sheet_ids == sheet_id]), sheet_name)

#   gs_conn$delete(sheet_id)  # clean up
# })

# test_that("INTEGRATION: deletes a sheet", {
#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_id <- gs_conn$create("Delete Test")
#   gs_conn$delete(sheet_id)
#   sheet_ids <- gs_conn$list_sheets()

#   # Deleted sheet is not in returned list.
#   expect_equal(sheet_id %in% sheet_ids, FALSE)
# })

# test_that("INTEGRATION: can append to table in a sheet", {
#   example_df <- data.frame(a = c(1, 2), b = c(4, 5))

#   gs_conn <- gs$open(
#     testing_credentials,
#     credential_type = 'service_account_key'
#   )
#   sheet_id <- gs_conn$create("RServe append test")
#   gs_conn$overwrite(sheet_id, example_df)

#   # Now that the sheet has data, append some.
#   append_df <- data.frame(a = 3, b = 6)
#   result <- gs_conn$append(sheet_id, append_df)

#   result <- gs_conn$read(sheet_id, 'Sheet1!A1:B4')

#   # Watch out for data.frame() converting characters to factors!
#   expected <- data.frame(
#     a = c('1', '2', '3'),
#     b = c('4', '5', '6'),
#     stringsAsFactors = FALSE
#   )
#   expect_equal(result, expected)

#   gs_conn$delete(sheet_id)  # clean up
# })

test_that("UNIT: data_range_with_header, A1, with tab/sheet", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_equal(
    gs$data_range_with_header(df, range_begin = 'Sheet1!A1'),
    'Sheet1!A1:B3'
  )
})

test_that("UNIT: data_range_with_header, A1, without tab/sheet", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_equal(
    gs$data_range_with_header(df, range_begin = 'A1'),
    'A1:B3'
  )
})

test_that("UNIT: data_range_with_header, custom begin", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_equal(
    gs$data_range_with_header(df, range_begin = 'B3'),
    'B3:C5'
  )
})

test_that("UNIT: data_range_with_header, lowercase", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_equal(
    gs$data_range_with_header(df, range_begin = 'a1'),
    'A1:B3'
  )
})

test_that("UNIT: data_range_raw", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_equal(
    gs$data_range_raw(df, range_begin = 'B2'),
    'B2:C3'
  )
})

test_that("UNIT: data_range_skip_header", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_equal(
    gs$data_range_skip_header(df, range_begin = 'B2'),
    'B3:C4'
  )
})

test_that("UNIT: data_range_with_header fails given a range", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_error(
    gs$data_range_with_header(df, 'Sheet1!A1:A2'),
    "range_begin"
  )
})
