context("util.R")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the gymnast directory
# * Run this command:
#   Rscript -e "testthat::test_file('tests/testthat/test_util.R')"
#
# This will run all the tests in this file once. Run the command again after
# changes. Sadly, testthat::auto_test() was broken some time at or before
# version 3.0.4.

if (grepl("tests/testthat$", getwd())) {
  setwd("../..") # root dir of gymnast repo
}

util <- import_module("util")

# expect_true(all(expected_cols %in% names(fake_df)))

describe("str_percent", {
  it("normal case", {
    expect_identical(util$str_percent(1, 2), "50%")
  })

  it("returns blank value if zero denominator", {
    expect_identical(util$str_percent(1, 0), "N/A")
  })

  it("uses custom blank value", {
    expect_identical(util$str_percent(1, 0, blank_value = "-"), "-")
  })

  it("rounds to the ones place", {
    expect_identical(util$str_percent(1, 3), "33%")
  })

  it("handles values > 100%", {
    expect_identical(util$str_percent(100, 1), "10000%")
  })

  it("handles negative values", {
    expect_identical(util$str_percent(-1, 2), "-50%")
  })

  it("handles non-unitary vectors", {
    numerator <- c(-1, 0, 1)
    denominator <- c(100, 100, 0)
    expect_identical(
      util$str_percent(numerator, denominator),
      c("-1%", "0%", "N/A")
    )
  })

  it("can be used with dfs and pipes", {
    d <- data.frame(a = c(1, 2, 3), b = c(2, 3, 4)) %>%
      dplyr::mutate(pct_ab = util$str_percent(a, b))

    expect_identical(d$pct_ab, c("50%", "67%", "75%"))
  })
})

describe("is_interger", {
  it("returns false for characters", {
    char_str <- "a"
    expect_false(util$is_integer(char_str))

  })
  it("returns true for intergers", {
    zero <- 0
    hun <- 100
    neg <- -30

    expect_true(util$is_integer(zero))
    expect_true(util$is_integer(hun))
    expect_true(util$is_integer(neg))
  })

  it("returns false for numeric non-intergers", {
    ni <- 0.1
    frac <- 3 / 5
    neg_ni <- -30.1

    expect_false(util$is_integer(ni))
    expect_false(util$is_integer(frac))
    expect_false(util$is_integer(neg_ni))
  })
})

describe("is_valid_percent", {
  it("returns true for properly formated data", {
    expect_true(util$is_valid_percent("99%"))
    expect_true(util$is_valid_percent("-3%"))
    expect_true(util$is_valid_percent("—"))
    expect_true(util$is_valid_percent("0%"))
  })

  it("handels values beyond beyond 99% to - 99%, (up to 9999%)", {

    expect_true(util$is_valid_percent("199%"))
    expect_true(util$is_valid_percent("9999%"))
    expect_true(util$is_valid_percent("-300%"))
    expect_true(util$is_valid_percent("1000%"))

  })

  it("returns error for numeric strings", {
    expect_false(util$is_valid_percent(99))
    expect_false(util$is_valid_percent(-30))
    expect_false(util$is_valid_percent(0))
    expect_false(util$is_valid_percent("01%"))
  })

  it('does not return a pct sign when asked not to',{
    expect_equal(util$str_percent(1, 2, use_percent_sign = FALSE), '50')
  })
})

describe('datetime_to_iso_string', {
  it('returns current time by default', {
    datetime_str <- util$datetime_to_iso_string()
    expect_true(grepl(
      '^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$',
      datetime_str
    ))

    # There will be some tiny difference between the result and the comparison
    # time so just test that it's within a tolerance.
    datetime <- strptime(datetime_str, "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
    seconds_difference <- difftime(Sys.time(), datetime, units = "secs")
    expect_true(seconds_difference < 1)
  })

  it('returns given datetime, formatted', {
    # Starting with a timezone-agnostic value here to prove that the function
    # does actually use UTC.

    datetime = as.POSIXct(1127056501, origin = "1970-01-01")

    # On my system this prints:
    # > [1] "2005-09-18 08:15:01 PDT"

    expect_identical(
      util$datetime_to_iso_string(datetime),
      '2005-09-18T15:15:01Z'
    )
  })
})
