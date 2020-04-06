context("json_utils.R")
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

modules::use("R/bootstrap.R")$install_module_imports()
json_utils <- import_module("json_utils")

  # expect_true(all(expected_cols %in% names(fake_df)))

describe("parse_string_array", {
  it("normal case", {
    expect_identical(
      json_utils$parse_string_array('["foo", "bar"]'),
      c("foo", "bar")
    )
  })

  it("empty array", {
    expect_identical(
      json_utils$parse_string_array('[]'),
      as.character(vector())
    )
  })

  it("mixed types", {
    # N.B. this is asymmetrical with parse_numeric_array. In that case, values
    # that don't match the type are coerced to NA it warns. In
    # this case everything just because a character. This is based on what R
    # already does, e.g. c(5, "foo") is coerced to be identical to c("5", "foo"),
    # and never c(5, NA) without further intervention.
    expect_identical(
      # Some of these are strings, so jsonlite should output a character,
      # i.e. this is fine.
      json_utils$parse_string_array('["foo", 5]'),
      c("foo", "5")
    )
  })

  it("bad type", {
    expect_warning(
      expect_identical(
        json_utils$parse_string_array('[5, 6]'),
        c("5", "6")
      )
    )
  })
})

describe("parse_numeric_array", {
  it("normal case", {
    expect_identical(
      json_utils$parse_numeric_array('[5, 6]'),
      c(5, 6)
    )
  })

  it("empty array", {
    expect_identical(
      json_utils$parse_numeric_array('[]'),
      as.numeric(vector())
    )
  })

  it("mixed types", {
    expect_warning(
      expect_identical(
        # Some of these are strings, so jsonlite should output a character,
        # i.e. this is fine.
        json_utils$parse_numeric_array('["foo", 5]'),
        c(NA, 5)
      )
    )
  })

  it("bad type", {
    expect_warning(
      expect_identical(
        json_utils$parse_numeric_array('["foo", "bar"]'),
        as.numeric(c(NA, NA))
      )
    )
  })
})

describe("expand_vector_column_", {
  it("with character, varying lengths", {
    base_df <- data.frame(a = c(1, 2, 3))
    nested_col <- list(
      c("foo", "bar", "baz"),
      as.character(vector()),
      c("hi", "bye")
    )

    nested_df <- base_df
    nested_df$j <- nested_col

    expected <- dplyr::tibble(
      a = c(    1,     1,     1,  2,    3,     3),
      j = c("foo", "bar", "baz", NA, "hi", "bye")
    )

    expect_equal(json_utils$expand_vector_column_(nested_df, "j"), expected)
  })

  it("with numeric, varying lengths", {
    base_df <- data.frame(a = c(1, 2, 3))
    nested_col <- list(
      c(4, 5, 6),
      as.numeric(vector()),
      c(10.1, -5e5)
    )

    nested_df <- base_df
    nested_df$j <- nested_col

    expected <- dplyr::tibble(
      a = c(1, 1, 1,  2,    3,    3),
      j = c(4, 5, 6, NA, 10.1, -5e5)
    )

    expect_equal(json_utils$expand_vector_column_(nested_df, "j"), expected)
  })

  it("with zero-row data frames", {
    df <- dplyr::tibble(a = numeric(0), j = character(0))
    expected <- dplyr::tibble(a = numeric(0), j = character(0))

    expect_equal(json_utils$expand_vector_column_(df, "j"), expected)
  })

  it("empty array", {
    expect_identical(
      json_utils$parse_numeric_array('[]'),
      as.numeric(vector())
    )
  })

  it("mixed types", {
    expect_warning(
      expect_identical(
        # Some of these are strings, so jsonlite should output a character,
        # i.e. this is fine.
        json_utils$parse_numeric_array('["foo", 5]'),
        c(NA, 5)
      )
    )
  })

  it("bad type", {
    expect_warning(
      expect_identical(
        json_utils$parse_numeric_array('["foo", "bar"]'),
        as.numeric(c(NA, NA))
      )
    )
  })
})

describe("expand_string_array_column", {
  it("normal case", {
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        '["foo", "bar", "baz"]',
        '[]',
        '["hi", "bye"]'
      )
    )

    actual <- json_utils$expand_string_array_column(df, j)

    expected <- data.frame(
      a = c(    1,     1,     1,  2,    3,     3),
      j = c("foo", "bar", "baz", NA, "hi", "bye"),
      stringsAsFactors = FALSE
    )

    expect_equal(actual, expected)
  })
})

describe("expand_numeric_array_column", {
  it("normal case", {
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        '[4, 5, 6]',
        '[]',
        '[10.1, -5e5]'
      )
    )

    actual <- json_utils$expand_numeric_array_column(df, j)

    expected <- dplyr::tibble(
      a = c(1, 1, 1,  2,    3,    3),
      j = c(4, 5, 6, NA, 10.1, -5e5)
    )

    expect_equal(actual, expected)
  })
})

describe("widen_object_column", {
  it("default case", {
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        '{"foo": "bar", "baz": "qux"}',
        '{"foo": "bar"}',
        '{"hi": "bye"}'
      )
    )

    actual <- json_utils$widen_object_column(df, j)

    expected <- dplyr::tibble(
      a = c(1, 2, 3),
      foo = c('bar', 'bar', NA),
      baz = c('qux', NA, NA),
      hi = c(NA, NA, 'bye')
    )

    expect_equal(actual, expected)
  })

  it("handles a 1-row data frame", {
    df <- dplyr::tibble(
      a = 1,
      j = '{"foo": "bar", "baz": "qux"}'
    )

    actual <- json_utils$widen_object_column(df, j)

    expected <- dplyr::tibble(
      a = 1,
      foo = 'bar',
      baz = 'qux'
    )

    expect_equal(actual, expected)
  })

  it("handles types predictably", {
    # This is all done by jsonlite::fromJSON and rbind, but this test
    # documents the behavior.
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        '{"chr": "foo", "int": 1, "dbl": 0.1, "bool": true}',
        '{"chr": "bar", "int": 2, "dbl": -0.1, "bool": false}',
        '{"chr": 1}'
      )
    )

    actual <- json_utils$widen_object_column(df, j)

    expected <- dplyr::tibble(
      a = c(1, 2, 3),
      # Mixed types become character.
      chr = c('foo', 'bar', '1'),
      # All-integers are integer.
      int = as.integer(c(1, 2, NA)),
      # Non-integer numerics become double.
      dbl = as.double(c(0.1, -0.1, NA)),
      # Booleans become logical
      bool = c(TRUE, FALSE, NA)
    )

    expect_equal(actual, expected)
  })

  it("non-flat object", {
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        # this array will be converted to a length-1 character
        '{"foo": [1,2,3], "baz": "qux"}',
        # this nested object will be converted to NA
        '{"foo": {"is": "nested"}}',
        '{"hi": "bye"}'
      )
    )

    expect_warning({
      actual <- json_utils$widen_object_column(df, j)
    })

    expected <- dplyr::tibble(
      a = c(1, 2, 3),
      foo = c('[1,2,3]', NA, NA),
      baz = c('qux', NA, NA),
      hi = c(NA, NA, 'bye')
    )

    expect_equal(actual, expected)
  })

  it("unknown column", {
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        '{"foo": "bar", "baz": "qux"}',
        '{"foo": "bar"}',
        '{"hi": "bye"}'
      )
    )

    expect_error(
      json_utils$widen_object_column(df, doesnotexist),
      'json_utils\\$widen_object_column'
    )
  })

  it("handles types predictably", {
    # This is all done by jsonlite::fromJSON and rbind, but this test
    # documents the behavior.
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        '{"chr": "foo", "int": 1, "dbl": 0.1, "bool": true}',
        '{"chr": "bar", "int": 2, "dbl": -0.1, "bool": false}',
        '{"chr": 1}'
      )
    )

    actual <- json_utils$widen_object_column(df, j)

    expected <- dplyr::tibble(
      a = c(1, 2, 3),
      # Mixed types become character.
      chr = c('foo', 'bar', '1'),
      # All-integers are integer.
      int = as.integer(c(1, 2, NA)),
      # Non-integer numerics become double.
      dbl = as.double(c(0.1, -0.1, NA)),
      # Booleans become logical
      bool = c(TRUE, FALSE, NA)
    )

    expect_equal(actual, expected)
  })

  it("non-flat object", {
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        # this array will be converted to a length-1 character
        '{"foo": [1,2,3], "baz": "qux"}',
        # this nested object will be converted to NA
        '{"foo": {"is": "nested"}}',
        '{"hi": "bye"}'
      )
    )

    expect_warning({
      actual <- json_utils$widen_object_column(df, j)
    })

    expected <- dplyr::tibble(
      a = c(1, 2, 3),
      foo = c('[1,2,3]', NA, NA),
      baz = c('qux', NA, NA),
      hi = c(NA, NA, 'bye')
    )

    expect_equal(actual, expected)
  })

  it("unknown column", {
    df <- dplyr::tibble(
      a = c(1, 2, 3),
      j = c(
        '{"foo": "bar", "baz": "qux"}',
        '{"foo": "bar"}',
        '{"hi": "bye"}'
      )
    )

    expect_error(
      json_utils$widen_object_column(df, doesnotexist),
      'json_utils\\$widen_object_column'
    )
  })

  it("returns data frame if there are zero rows", {
    df <- dplyr::tibble(a = vector(), j = vector())
    expect_equal(json_utils$widen_object_column(df, j), df)
  })

  it("errors if data would be overwritten", {
    df <- dplyr::tibble(
      a = c(1, 2),
      b = c(1, 2),
      j = c('{"a": "foo", "b": "bar"}', '{"a": "baz", "b": "qux"}')
    )
    expect_error(
      json_utils$widen_object_column(df, j),
      'json_utils\\$widen_object_column'
    )
  })
})
