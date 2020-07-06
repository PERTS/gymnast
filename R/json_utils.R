modules::import('dplyr', `%>%`)

util <- import_module("util")

parse_string_array <- function (json_array) {
  # Args:
  #   * json_array - unitary character vector, JSON structure should be an
  #     array of strings, e.g. '["foo", "bar"]'
  # Returns: character vector of variable length
  #
  # Why not just use jsonlite::fromJSON? Because for empty
  # arrays it returns a list instead of a vector. Adds type
  # checking.
  #
  # Example:
  #   > parse_string_array('["foo", "bar"]')
  #   [1] "foo" "bar"
  #   > parse_string_array('[]')
  #   character(0)
  #   > parse_string_array('[5, 6]')
  #   Error: Can't convert JSON data to type character...
  if (json_array %in% '[]') {
    return(as.character(vector()))
  }
  result <- jsonlite::fromJSON(json_array)
  if (!identical(result, as.character(result))) {
    warning(paste0(
      "parse_string_array() recieved data that didn't look like strings: ",
      json_array
    ))
  }
  return(as.character(result))
}

parse_numeric_array <- function (json_array) {
  # Args:
  #   * json_array - unitary character vector, JSON structure should be an
  #     array of numbers, e.g. '[-99.9, 6.022e23]'
  # Returns: numeric vector of variable length
  #
  # See comments above.
  if (json_array %in% '[]') {
    return(as.numeric(vector()))
  }
  result <- jsonlite::fromJSON(json_array)
  if (!is.numeric(result)) {
    warning(paste0(
      "parse_numeric_array() recieved data that didn't look like numbers: ",
      json_array
    ))
  }
  return(as.numeric(result))
}

expand_vector_column_ <- function (df, column_name, as_type = NULL) {
  # Take a data frame that has a column where the elements have length > 1
  # and multiply the rows for each element in the vector.
  #
  # Input:
  #    a                      j
  #  1 1 c("foo", "bar", "baz")
  #  2 2           character(0)
  #  3 3         c("hi", "bye")
  #
  # Output:
  #   a   j
  # 1 1 foo
  # 2 1 bar
  # 3 1 baz
  # 2 2 NA
  # 4 3 hi
  # 5 3 by

  if (nrow(df) == 0) {
    if (!is.null(as_type)) {
      df[[column_name]] <- as_type(df[[column_name]])
    }

    return(df)
  }

  if (dplyr::is.grouped_df(df)) {
    df <- dplyr::ungroup(df)
  }

  vector_column <- df[[column_name]]
  if (is.null(vector_column)) {
    stop(paste0("Column '", column_name, "' not found."))
  }

  dfs_to_bind <- list()
  for (r in 1:nrow(df)) {
    current_vector <- vector_column[[r]]

    if (length(current_vector) %in% 0) {
      expanded <- df[r, ]
      expanded[[column_name]] <- NA
    } else {
      # Expand the current row of the data frame to repeat to the same dimension
      # as the vector.
      expanded <- df %>% dplyr::slice(rep(r, each = length(current_vector)))

      # Append the vector (which was a cell) as a _column_, now that the
      # dimensions are correct.
      expanded[[column_name]] <- current_vector
    }

    # Data frames ARE lists, so just concatenating them to our collection
    # `dfs_to_bind` would actually concatenate the _columns_ of the data frame
    # to the list. Wrapping it in list() forces R to treat the whole df as a
    # discrete object.
    dfs_to_bind <- c(dfs_to_bind, list(expanded))
  }

  if (length(dfs_to_bind) %in% 0) {
    bound <- df
  } else if (length(dfs_to_bind) %in% 1) {
    bound <- dfs_to_bind[[1]]
  } else {
    bound <- do.call(rbind, dfs_to_bind)
  }

  if (!is.null(as_type)) {
    bound[[column_name]] <- as_type(bound[[column_name]])
  }

  return(tibble::as.tibble(bound))
}

expand_string_array_column <- function (df, column_name) {
  # Accept NSE column name.
  col_expr <- substitute(column_name)

  # Parse each json array as a vector. N.B. this results in a
  # data frame where each cell in this column can have length
  # greater than 1, which is "untidy".
  parsed_df <- df
  parsed_df[[col_expr]] <- sapply(
    df[[col_expr]],
    parse_string_array,
    # Don't use the default behavior of sapply:
    # "should the result be simplified to a vector, matrix or higher dimensional array if possible?"
    # No.
    simplify = FALSE
  )

  # Expand/melt this untidy state into a longer form table,
  # tidy once again. Make sure to convert the nse argument
  # (an "expression") into a string for easier processing
  # downstream.
  return(expand_vector_column_(parsed_df, deparse(col_expr), as.character))
}

expand_numeric_array_column <- function (df, column_name) {
  # Accept NSE column name.
  col_expr <- substitute(column_name)

  # Parse each json array as a vector. N.B. this results in a
  # data frame where each cell in this column can have length
  # greater than 1, which is "untidy".
  parsed_df <- df
  parsed_df[[col_expr]] <- sapply(df[[col_expr]], parse_numeric_array)

  # Expand/melt this untidy state into a longer form table,
  # tidy once again. Make sure to convert the nse argument
  # (an "expression") into a string for easier processing
  # downstream.
  return(expand_vector_column_(parsed_df, deparse(col_expr), as.numeric))
}

widen_object_column <- function(df, column_name) {
  # Apply to any data frame that has a serialized JSON object column,
  # e.g. the saturn `response` table's `answers` field (note that triton's
  # response table is different and more complex).
  # Returns a similar dataframe, with that column widened into multiple
  # columns. Missing values are filled in with NA (see util$rbind_union).
  #
  # Input dataframe:
  #
  # | id |     json     |
  # |----|--------------|
  # |  1 | '{"foo": 1}' |
  # |  2 | '{"bar": 2}' |
  #
  # Output dataframe:
  #
  # | id | foo | bar |
  # |----|-----|-----|
  # |  1 | 1   | NA  |
  # |  2 | NA  | 2   |
  #
  # Example:
  #
  #   wide_responses <- widen_object_column(saturn_response_tbl, answers)

  # Accept NSE column name.
  col_expr <- substitute(column_name)

  if (nrow(df) %in% 0) {
    return(df)
  }

  if (!as.character(col_expr) %in% names(df)) {
    stop(paste0(
      "json_utils$widen_object_column: column ",
      as.character(col_expr),
      " not found in data frame."
    ))
  }

  # Each row be added to this and rbound together.
  dfs_to_bind <- list()
  binding_index <- 1

  widen <- function (row) {
    # `row` is a character vector with named elements.

    # Make a 1-row temporary data frame for every element EXCEPT the one with
    # JSON data. We'll expand that one and assign new columns to the df.
    col_keep <- !names(row) %in% as.character(col_expr)
    df_args <- as.list(row[col_keep])
    df_args$stringsAsFactors = FALSE
    row_df <- do.call(data.frame, df_args)

    # Parse the serialized field-level data into a list.
    tryCatch(
      {
        parsed_object <- jsonlite::fromJSON(
          row[[col_expr]],
          # jsonlite may think that some structures, like
          # '[{"a": 1, "b": 2}, {"a": 3, "b": 4}]'
          # should be returned as data frames; if we happe to have one of these,
          # don't apply any magic to it, so it's not confused with a
          # straightforward list.
          simplifyDataFrame = FALSE
        )
      },
      error = function (e) {
        print(e)
        print(methods::is(row[[col_expr]]))
        print(row[[col_expr]])
        print(row)
        return(list())
      }
    )

    if (is.null(parsed_object)) {
      parsed_object <- list()
    }

    if (!typeof(parsed_object) %in% 'list') {
      stop(paste0(
        "json_utils$widen_object_column: object didn't parse to a list. Type: ",
        typeof(parsed_object),
        " JSON string: ",
        row[[col_expr]]
      ))
    }

    existing_cols <- names(row)[col_keep]
    dupe_cols <- existing_cols[names(parsed_object) %in% existing_cols]
    if (length(dupe_cols) > 0) {
      stop(paste0(
        "json_utils$widen_object_column: widening would overwrite columns: ",
        paste(dupe_cols, collapse = ", "),
        ". Store them elsewhere first."
      ))
    }

    # For each field in the list...
    for (col in names(parsed_object)) {
      # Make sure the value is primitive. Otherwise keep it as a JSON string.
      value <- parsed_object[[col]]
      if (length(value) > 1) {
        value <- as.character(jsonlite::toJSON(value))
      } else if (!is.character(value) && !is.numeric(value) && !is.logical(value)) {
        warning(paste0(
          "Replacing unrecognized value with NA. Is: ",
          paste(methods::is(value), collapse = ' '),
          ". Length: ",
          length(value),
          ". Value: ",
          value
        ))
        value <- NA
      }

      # Create a new column for this field.
      row_df[[col]] <- value
    }

    # Now that all the fields are unpacked, bind this row to the full data frame.
    dfs_to_bind[[binding_index]] <<- row_df
    binding_index <<- binding_index + 1
  }

  # This has the side effect of populating dfs_to_bind.
  apply(df, 1, widen)

  if (length(dfs_to_bind) > 1) {
    wide <- util$rbind_union(dfs_to_bind)
  } else {
    wide <- dfs_to_bind[[1]]
  }

  # Numeric types have been lost. I'd love to be able to dynamically make the
  # output column types match in the input ones, but I don't know how to do that,
  # short of writing out a bunch of if statements to matched each type with its
  # "as" function. So just check the numeric ones.
  for (col in names(df)) {
    if (is.numeric(df[[col]]) && col %in% names(wide)) {
      wide[[col]] <- as.numeric(wide[[col]])
    }
  }

  return(tibble::as.tibble(wide))
}
