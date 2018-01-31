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
    message("Warning - first argument passed to dfc.setdiff_plus has duplicate values that will be ignored in the set-diff.")
  }
  if(any(duplicated(vec2))) {
    message("Warning - second argument passed to dfc.setdiff_plus has duplicate values that will be ignored in the set-diff.")
  }

  return(list(shared_elements = intersect(vec1, vec2),
              only_in_first = setdiff(vec1, vec2),
              only_in_second = setdiff(vec2, vec1)))
}


dfc.get_concatenated_ids <- function(df, id_cols) {
  # Helper function for getting a vector of IDs (not necessarily unique) from a DF.
  # If there are multiple ID columns, they need to be concatenated with "___".
  if(length(id_cols) == 1) {
    return(as.character(df[, id_cols]))
  } else {
    # use util.to_character do avoid some nasty behavior with apply(), which 
    # converts numeric types to varchars when passing to paste, so that white spaces 
    # are appended to numbers with fewer than the max digits (e.g., if a column
    # has both "9" and "10", the apply function turns them to " 9" and "10". 
    # util.to_character prevents this.)
    return(apply(util.to_character(df[, id_cols]), 1, paste, collapse = "___"))
  }
}


dfc.compare_df_values <- function(df1, df2, id_col, verbose = FALSE) {
  # Function for comparing exact values of two data frames that have identical columns and row identifiers.
  # INPUTS: two data frames and a string naming a shared ID column. "Verbose" prints heads of summary tables.
  # OUTPUT: A list containing three elements:
  #    diff_df: a DF the same dimensions as the input DF of TRUEs and FALSEs for value comparisons.
  #    row_summary: summary table of percent-unmatched across row identifiers.
  #    col_summary: summary table of percent-unmatched across col identifiers.
  # STEPS:
  #    check that dimensions are identical
  #    check that column names are identical and ordered the same
  #    check that both DFs have the id column named in the arguments
  #    check that both DFs have the same exact set of values in the ID column, ordered the same way
  #    warn if there are duplicates in the ID column for either, because they make precise row sorting impossible
  #    compare the DFs value-by-value, getting a new DF of trues and falses for matches and non-matches
  #    report diff DF and summary tables of percent-unmatched by row ID and by col name.


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
  if(!identical(df1[, id_col], df2[, id_col])) {
    stop("Cannot compare DF values - DFs have different ID column values, or the same values in a different order.")
  }

  # warn if there are duplicates in the ID column for either, because they make precise row sorting impossible
  if(any(duplicated(df1[, id_col])) | any(duplicated(df2[, id_col]))) {
    message("Warning - duplicate ID values found in at least one data frame. Rows might not be sorted the same way, even if row IDs are.")
  }

  ##### ALL CHECKS PASSED - BEGIN VALUE-BY-VALUE COMPARISON #####

  # Cast NAs to the string "___NA___" so that "___NA___" == "___NA___" returns TRUE for all comparisons.
  df1[is.na(df1)] <- "___NA___"
  df2[is.na(df2)] <- "___NA___"

  # Compare DF values
  dfdiff <- as.data.frame(df1 == df2)
  if(all(dfdiff == TRUE)) {
    message("Perfect match!")
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
  dfdiff_sum_rows <- dfdiff_sum_rows[, c("pct_unmatched", "num_unmatched")]
  # need to attach the original ids too.
  dfdiff_sum_rows[, id_col] <- df1[, id_col]
  dfdiff_sum_rows <- dfdiff_sum_rows[, c(id_col, "pct_unmatched", "num_unmatched")]

  if(verbose) {util.html_table(head(dfdiff_sum_rows))}

  # Return diff DF and full summary dfs for user.
  return(list(diff_df = dfdiff,
              row_summary = dfdiff_sum_rows,
              col_summary = dfdiff_sum_cols))

}

