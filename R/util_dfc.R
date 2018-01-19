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

dfc.compare_vecs <- function(vec1, vec2) {
  # compare vector elements without regard to order and without checking for duplicates.
  # return a list with three components -
  ## shared_elements: the shared elements,
  ## only_in_first: the elements in vec1 and not vec2,
  ## only_in_second: the elements in vec2 and not vec1.
  return(list(shared_elements = intersect(vec1, vec2),
              only_in_first = setdiff(vec1, vec2),
              only_in_second = setdiff(vec2, vec1)))
}


dfc.get_concatenated_ids <- function(df, id_cols) {
  # Helper function for getting a vector of IDs (not necessarily unique) from a DF.
  # If there are multiple ID columns, they need to be concatenated with "__".
  if(length(id_cols) == 1) {
    return(as.character(df[, id_cols]))
  } else {
    return(apply(df[, id_cols], 1, paste, collapse = "__"))
  }
}


dfc.compare_colnames <- function(df1, df2) {
  # compare the column names of two data frames using dfc.compare_vecs.
  # return a list with three components -
  ## shared_elements: the shared names,
  ## only_in_first: the names in df1 and not df2,
  ## only_in_second: the names in df2 and not df1

  # Sanity checks
  if(!is.data.frame(df1) | !is.data.frame(df2)) {
    stop("Error - at least one of the first two arguments for dfc.compare_colnames is not a data frame.")
  }

  return(dfc.compare_vecs(names(df1), names(df2)))
}




dfc.compare_identifiers <- function(df1, df2, id_cols, id_cols_uniquely_identify_rows = FALSE) {
  # Compare two dfs on a set of one or more commonly-shared columns that identify rows.
  # If rows are identified by multiple columns (e.g. team x student x week), include all column names in id_cols.
  # Return a list with shared IDs, IDs in df1 only, and IDs in df2 only.
  #
  # If you expect id_cols to UNIQUELY identify each row in both dfs, set id_cols_uniquely_identify_rows to TRUE.
  # Otherwise, duplicate ID values within each data frame will get ignored.
  # Example use case: you want to compare a student-x-week df with a student df to see if they have the same students.
  # The commonly-shared id_col is student ID.
  # Since student ID doesn't uniquely identify rows in the first df, id_cols_uniquely_identify_rows = FALSE.

  # Sanity checks
  if(!is.data.frame(df1) | !is.data.frame(df2)) {
    stop("Error - at least one of the first two arguments for dfc.compare_unique_identifiers is not a data frame.")
  }
  if(any(is.na(id_cols)) | length(id_cols) == 0) {
    stop("Error - there are NAs in the id_cols argument, and/or no id_cols were provided.")
  }
  if(any(!id_cols %in% names(df1)) | any(!id_cols %in% names(df2))) {
    stop("Error - not all id columns provided are in both data frames.")
  }

  # Extract the IDs from df1 and df2 in character format.
  # If IDs are defined by multiple columns, use them all with "__" as a separator.
  # Note: this section treats NAs as the character string "NA" in an ID.
  df1_ids <- dfc.get_concatenated_ids(df1, id_cols)
  df2_ids <- dfc.get_concatenated_ids(df2, id_cols)

  # If the IDs aren't expected to uniquely identify rows, then cut duplicates from them.
  if(!id_cols_uniquely_identify_rows) {
    df1_ids <- df1_ids[!duplicated(df1_ids)]
    df2_ids <- df2_ids[!duplicated(df2_ids)]
  }

  # Warn if the IDs still have duplicates in either df!
  # This would mean that the user expected no duplicates but actually had them.
  if(any(duplicated(df1_ids))) {
    warning("Warning - first data frame passed to dfc.compare_unique_identifiers has unexpected duplicate ID values:")
    message(unique(df1_ids[duplicated(df1_ids)]))
  }
  if(any(duplicated(df2_ids))) {
    warning("Warning - second data frame passed to dfc.compare_unique_identifiers has unexpected duplicate ID values:")
    message(unique(df2_ids[duplicated(df2_ids)]))
  }

  # use dfc.compare_vecs on id_cols of each df
  return(dfc.compare_vecs(df1_ids, df2_ids))
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
  col_name_compare_list <- dfc.compare_colnames(df1, df2)
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

  # Cast NAs to the string "__NA__" so that "__NA__" == "__NA__" returns TRUE for all comparisons.
  df1[is.na(df1)] <- "__NA__"
  df2[is.na(df2)] <- "__NA__"

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

