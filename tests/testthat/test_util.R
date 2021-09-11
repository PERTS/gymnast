context("util.R")
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
  it("returns error for characters", {
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

  })

  it("returns error for numeric strings", {
    expect_false(util$is_valid_percent(99))
    expect_false(util$is_valid_percent(-30))
    expect_false(util$is_valid_percent(0))
  })
})
