##########################################################################
###
###     util_dfc.R
###     Dan Greene, 12/21/17
###
###     The goal of the util_dfc ("data frame compare") module is to make it easy to see
###     where two data frames differ for debugging and further analysis.
###
###     Depends on util.R and util_data_summaries.R
###

dfc.setdiff_plus <- function(vec1, vec2) {
  # compare unique vector elements without regard to order and after automatically discarding any duplicates.
  # return a list with three components -
  ## shared_elements: the shared unique elements,
  ## only_in_first: the unique elements in vec1 and not vec2,
  ## only_in_second: the unique elements in vec2 and not vec1.

  # First, warn if there are duplicates in either vector. They will be discarded in the setdiff.
  # Warn if there are duplicate IDs in either df!
  if(any(duplicated(vec1))) {
    warning("Warning - first argument passed to dfc.setdiff_plus has duplicate values that will be ignored in the set-diff:")
    message(unique(vec1[duplicated(vec1)]))
  }
  if(any(duplicated(vec2))) {
    warning("Warning - second argument passed to dfc.setdiff_plus has duplicate values that will be ignored in the set-diff:")
    message(unique(vec2[duplicated(vec2)]))
  }

  return(list(shared_elements = intersect(vec1, vec2),
              only_in_first = setdiff(vec1, vec2),
              only_in_second = setdiff(vec2, vec1)))
}


dfc.get_concatenated_ids <- function(df, id_cols) {
  # Helper function for getting a vector of IDs (not necessarily unique) from a DF.
  # If there are multiple ID columns, they need to be concatenated with "---".
  if(length(id_cols) == 1) {
    return(as.character(df[, id_cols]))
  } else {
    return(apply(df[, id_cols], 1, paste, collapse = "---"))
  }
}

















dfc.compare_df_values <- function(df1, df2, id_col, verbose = FALSE) {
  # Function for comparing exact values of two data frames that have identical columns and row identifiers.
  # INPUTS: two data frames and a string naming a shared ID column.
  # OUTPUT: if the match is perfect, return nothing. Otherwise, return a list containing two elements:
  #    row_summary: summary table of percent-matched across row identifiers.
  #    col_summary: summary table of percent-matched across col identifiers.
  # STEPS:
  #    check that dimensions are identical
  #    check that column names are identical and ordered the same
  #    check that both DFs have the id column named in the arguments
  #    check that both DFs have the same exact set of values in the ID column, ordered the same way
  #    warn if there are duplicates in the ID column for either, because they make precise row sorting impossible
  #    compare the DFs value-by-value, getting a new DF of trues and falses for matches and non-matches
  #    report summary tables of percent-matched by row ID and by col name.


  # check that dimensions are identical
  if(!identical(dim(df1), dim(df2))) {
    stop("Cannot compare DF values - DFs have different dimensions.")
  }

  # check that column names are identical and ordered the same
  if(!identical(names(df1), names(df2))) {
    stop("Cannot compare DF values - DFs have different column names, or the same columns in a different order.")
  }

  # check that both DFs have the id column named in the arguments
  if(!id_col %in% names(df1) | !id_col %in% names(df2)) {
    stop("Cannot compare DF values - the ID column named in the arguments is not present in at least one DF.")
  }

  # check that both DFs have the same exact set of values in the ID column, ordered the same way
  if(!identical(df1[, id_col], df2[, id_col]) {
    stop("Cannot compare DF values - DFs have different ID column values, or the same values in a different order.")
  }

  # warn if there are duplicates in the ID column for either, because they make precise row sorting impossible
  if(any(duplicated(df1[, id_col])) | any(duplicated(df2[, id_col]))) {
    warning("Warning - duplicate ID values found in at least one data frame. Rows might not be sorted the same way, even if row IDs are.")
  }

  ##### ALL CHECKS PASSED - BEGIN VALUE-BY-VALUE COMPARISON #####

  # Cast NAs to the string "---NA---" so that "---NA---" == "---NA---" returns TRUE for all comparisons.
  df1[is.na(df1)] <- "---NA---"
  df2[is.na(df2)] <- "---NA---"

  # Compare DF values
  dfdiff <- as.data.frame(df1 == df2)
  if(all(dfdiff == TRUE)) {
    message("Perfect match!")
    return()
  } else {
    message("Match was not perfect.")
  }

  # Compare DF values by COLUMN and report results on the first few column variables.
  dfdiff_sum_cols <- ds.summarize_by_column(dfdiff, func_list = list("pct_unmatched" = function(x) {1 - mean(x)},
                                                                     "num_unmatched" = function(x) {length(x) - sum(x)}))
  if(verbose) {util.html_table(head(dfdiff_sum_cols))}

  # Compare DF values by ROW IDENTIFIER and report results on the first few identifiers.
  dfdiff_sum_rows <- dfdiff
  dfdiff_sum_rows$pct_unmatched <- apply(dfdiff_sum_rows, 1, function(x) {1 - mean(x)})
  dfdiff_sum_rows$num_unmatched <- apply(dfdiff_sum_rows[, !names(dfdiff_sum_rows) %in% "pct_unmatched"],
                                         1,
                                         function(x) {length(x) - sum(x)})
  dfdiff_sum_rows <- dfdiff_sum_rows[, c(id_col, "pct_unmatched", "num_unmatched")]

  if(verbose) {util.html_table(head(dfdiff_sum_rows))}

  # Return full summary dfs for user.
  return(list(row_summary = dfdiff_sum_rows,
              col_summary = dfdiff_sum_cols))

}















dfc.compare_dfs <- function(df1, df2, id_cols = c()) {
  # Big wrapper function for giving an overall picture of how two DFs differ:
  ## Check nrow and ncol
  ## Compare colnames
  ## Compare identifiers
  ## Subset to shared columns, sort columns and rows (using id_cols), and report which values match exactly in the subset.

  ##### BASIC SANITY-CHECKS AND COMPARISONS

  # Setup
  df1_name <- deparse(substitute(df1))
  df2_name <- deparse(substitute(df2))

  # Are they data frames?
  if(!is.data.frame(df1) | !is.data.frame(df2)) {
    stop("Error - at least one of the first two arguments for dfc.compare_dfs is not a data frame.")
  }

  # Compare dimensions
  if(identical(dim(df1), dim(df2))) {
    message("Data frames have the same dimensions.")
  } else {
    message("Data frames have different dimensions!")
    message("Data frame 1: " %+% paste(dim(df1), collapse = " x "))
    message("Data frame 2: " %+% paste(dim(df2), collapse = " x "))
  }



  ##### COMPARING AND FILTERING COLUMNS

  # Compare column names
  message("COMPARING COLUMN NAMES: ")
  col_name_compare_list <- dfc.setdiff_plus(names(df1), names(df2))
  col_names_not_perfect_match <- ifelse(length(c(col_name_compare_list$only_in_first, col_name_compare_list$only_in_second)) > 0,
                                        T, F)
  # If the column names don't match up perfectly...
  if(col_names_not_perfect_match) {

    # Report the column names that didn't match
    message("Column names did not match perfectly.")
    message("Columns only in first data frame: " %+%
                     paste(col_name_compare_list$only_in_first, collapse = ", "))
    message("Columns only in second data frame: " %+%
                     paste(col_name_compare_list$only_in_second, collapse = ", "))

    # If there are NO matching columns, exit!
    if(length(col_name_compare_list$shared_elements) == 0) {
      message("Data frames share no common column names! Comparison is impossible. Exiting.")
      return()
    }

    # otherwise, subset to shared columns
    message("Only comparing shared columns going forward.")
    df1 <- df1[, col_name_compare_list$shared_elements]
    df2 <- df2[, col_name_compare_list$shared_elements]

  } else {
    message("Column names matched perfectly!")
  }


  # Sort columns for apples-to-apples comparison.
  df1 <- df1[, sort(names(df1))]
  df2 <- df2[, sort(names(df2))]


  ##### COMPARING AND FILTERING ROWS (IF THERE ARE IDENTIFIERS)

  if(length(id_cols) > 0) {

    # Sanity-check if id_cols are in both DFs
    if(any(!id_cols %in% names(df1)) | any(!id_cols %in% names(df2))) {
      stop("Error - not all id columns provided are in both data frames.")
    }

    # Setup - create ID column for each data frame, condensing across multiple columns if necessary
    df1$concat_id <- dfc.get_concatenated_ids(df1, id_cols)
    df2$concat_id <- dfc.get_concatenated_ids(df2, id_cols)

    # Determine whether there are any duplicates in either data frame, and report that
    dup_IDs <- FALSE
    if(any(duplicated(df1$concat_id))) {
      dup_IDs <- TRUE
      message("WARNING - the first data frame has duplicate ID rows (as defined by the id_cols parameter). This makes precise row-matching impossible.")
    }
    if(any(duplicated(df2$concat_id))) {
      dup_IDs <- TRUE
      message("WARNING - the second data frame has duplicate ID rows (as defined by the id_cols parameter). This makes precise row-matching impossible.")
    }

    # Report on the overlap between unique IDs across data frames
    unique_id_compare_list <- dfc.compare_vecs(unique(df1$concat_id), unique(df2$concat_id))
    message("COMPARING IDENTIFIERS: ")
    message(length(unique_id_compare_list$shared_elements) %+% " shared unique identifiers, such as: " %+%
                     paste(head(unique_id_compare_list$shared_elements), collapse = ", "))
    message(length(unique_id_compare_list$only_in_first) %+% " unique identifiers only in first data frame.")
    if(length(unique_id_compare_list$only_in_first) > 0) {
      message("...such as: " %+% paste(head(unique_id_compare_list$only_in_first), collapse = ", "))}
    message(length(unique_id_compare_list$only_in_second) %+% " unique identifiers only in second data frame.")
    if(length(unique_id_compare_list$only_in_second) > 0) {
      message("...such as: " %+% paste(head(unique_id_compare_list$only_in_second), collapse = ", "))}

    # Use the overlap to filter both data frames to rows with shared IDs, if possible
    if(length(unique_id_compare_list$shared_elements) == 0) {
      message("No shared IDs across data frames, so no way to compare data frame values. Ending function.")
      return()
    }
    df1 <- df1[df1$concat_id %in% unique_id_compare_list$shared_elements, ]
    df2 <- df2[df2$concat_id %in% unique_id_compare_list$shared_elements, ]
    message("Both data frames filtered to remove any rows with non-shared IDs. Any duplicate IDs within a data frame are preserved.")

    # Sort both DFs by ID, and warn user if duplicates make perfect sorting impossible
    df1 <- arrange(df1, concat_id)
    df2 <- arrange(df2, concat_id)
    message("Rows of both data frames sorted by ID column(s).")
    if(dup_IDs) {
      message("WARNING - ID columns do not uniquely identify rows in at least one data frame, so the rows of the two data frames cannot be guaranteed to match up.")
    }
  } else {
    message("WARNING - no ID columns were defined in the function call, so no way to filter or sort rows for comparison.")
  }


  ##### INDIVIDUAL-VALUE COMPARISON

  # Sanity-check that the dimensions of the two DFs are the same
  if(!all(dim(df1) == dim(df2))){
    message("WARNING - data frames do not have same dimensions after attempting to filter to shared rows and columns. No way to compare values. Stopping function.")
    return()
  }

  # Cast NAs to the string "---NA---" so that "---NA---" == "---NA---" returns TRUE for all comparisons.
  df1[is.na(df1)] <- "---NA---"
  df2[is.na(df2)] <- "---NA---"

  # Compare DF values
  dfdiff <- as.data.frame(df1 == df2)
  if(all(dfdiff == TRUE)) {
    message("After subsetting and sorting on shared columns (by name) and rows (by ID, if possible), the data frames perfectly match.")
  } else {
    message("After subsetting and sorting on shared columns (by name) and rows (by ID, if possible), the data frames do not perfectly match.")
  }

  # Compare DF values by COLUMN and report results on the first few column variables.
  dfdiff_sum_cols <- ds.summarize_by_column(dfdiff, func_list = list("pct_unmatched" = function(x) {1 - mean(x)},
                                                                "num_unmatched" = function(x) {length(x) - sum(x)}))
  util.html_table(head(dfdiff_sum_cols))

  # Compare DF values by ROW IDENTIFIER (if it exists) and report results on the first few identifiers.
  # If no identifier given, just use row numbers.
  dfdiff_sum_rows <- dfdiff
  dfdiff_sum_rows$pct_unmatched <- apply(dfdiff_sum_rows, 1, function(x) {1 - mean(x)})
  dfdiff_sum_rows$num_unmatched <- apply(dfdiff_sum_rows[, !names(dfdiff_sum_rows) %in% "pct_unmatched"],
                                         1,
                                         function(x) {length(x) - sum(x)})
  if(length(id_cols) > 0) {
    dfdiff_sum_rows <- dfdiff_sum_rows[, c("concat_id", "pct_unmatched", "num_unmatched")]
  } else {
    dfdiff_sum_rows$row_number <- 1:nrow(dfdiff_sum_rows)
    dfdiff_sum_rows <- dfdiff_sum_rows[, c("row_number", "pct_unmatched", "num_unmatched")]
  }

  util.html_table(head(dfdiff_sum_rows))

  # Return full summary dfs for user.
  message("Returning full summary tables.")
  return(list(row_summary = dfdiff_sum_rows,
              col_summary = dfdiff_sum_cols))

}

