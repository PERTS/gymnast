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
  # Compare unique vector elements without regard to order and after
  # automatically discarding any duplicates.
  #
  # Args: two vectors to compare
  #
  # Returns: list with three components:
  # * shared_elements: the shared unique elements,
  # * only_in_first: the unique elements in vec1 and not vec2,
  # * only_in_second: the unique elements in vec2 and not vec1.
  #
  # Example:
  #   dfc.setdiff_plus(c(1,2), c(2, 3))


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
    return(as.character(df[[id_cols]]))
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
  df1 <- util.apply_columns(df1, function(x) ifelse(is.na(x), "___NA___", x))
  df2 <- util.apply_columns(df2, function(x) ifelse(is.na(x), "___NA___", x))

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

  # get a side-by-side comparison of any unmatched values
  unmatched_summary <- dfdiff_sum_cols[dfdiff_sum_cols$num_unmatched > 0, ]
  unmatched_cols <- unmatched_summary$variable_name

  if(length(unmatched_cols) > 0){
    side_by_side_df <- data.frame(matrix(nrow = nrow(dfdiff_sum_rows), ncol = 0))
    # we know they're all sorted the same way, so we can build the data.frame just by concatenating columns
    side_by_side_df[[id_col]] <- dfdiff_sum_rows[[id_col]]
    for(col in unmatched_cols){
      side_by_side_df[[col %+% "_matches"]] <- dfdiff[[col]]
      side_by_side_df[[col %+% "_df1"]] <- df1[[col]]
      side_by_side_df[[col %+% "_df2"]] <- df2[[col]]
    }
  } else{
    side_by_side_df <- NA
  }

  if(verbose) {util.html_table(head(dfdiff_sum_rows))}

  # Return diff DF and full summary dfs for user.
  return(list(diff_df = dfdiff,
              row_summary = dfdiff_sum_rows,
              col_summary = dfdiff_sum_cols,
              side_by_side_df = side_by_side_df))

}


dfc.strip_for_comparison <- function(DF){
  # formats a DF to eliminate frequent causes of unimportant mismatches, including
  # differently-formatted blanks, rounding errors, and special characters
  clean_blanks <- function(DF){
    DF %>%
      util.apply_columns(function(x) ifelse(util.is_blank(x), NA, x)) %>%
      util.apply_columns(function(x) if(all(is.na(x))) return(rep(NA, length(x))) else return(x))
  }

  DF %>%
    clean_blanks %>%
    util.apply_columns(., util.as_numeric_if_number) %>%
    util.round_df(., 5) %>%
    util.apply_columns(util.strip_non_ascii) %>%
    util.apply_columns(util.strip_special_characters)
}

compare_to_previous <- function(current_df, previous_df, index_cols, compare_values = FALSE){
  # compare_values is FALSE by default because it is computationally intensive
  # returns a list containing:
  # (1) a colname comparison;
  # (2) an index comparison;
  # (3) IFF all column names, indices, and dims match, a values summary

  current_df <- as.data.frame(current_df)
  previous_df <- as.data.frame(previous_df)

  # compare dimensions
  dim_comparison <- list(
    nrows_comparison = list(current = nrow(current_df), previous = nrow(previous_df)),
    ncols_comparison = list(current = ncol(current_df), previous = ncol(previous_df))
  )
  nrows_match <- dim_comparison$nrows_comparison$current == dim_comparison$nrows_comparison$previous
  ncols_match <- dim_comparison$ncols_comparison$current == dim_comparison$ncols_comparison$previous

  dim_comparison$nrows_comparison$match <- nrows_match
  dim_comparison$ncols_comparison$match <- ncols_match

  # compare column names
  colname_comparison <- dfc.setdiff_plus(names(current_df), names(previous_df))
  colnames_match <- length(colname_comparison$only_in_first) == 0 & length(colname_comparison$only_in_second) == 0

  # compare indices
  current_df$index <- dfc.get_concatenated_ids(current_df, index_cols)
  previous_df$index <- dfc.get_concatenated_ids(previous_df, index_cols)
  index_comparison <- dfc.setdiff_plus(current_df$index, previous_df$index)
  indices_match <- length(index_comparison$only_in_first) == 0 & length(index_comparison$only_in_second) == 0

  # if everything else matches, and a values comparison was requested, go ahead and do a values comparison
  if(nrows_match & ncols_match & colnames_match & indices_match & compare_values){
    current_df <- current_df[order(current_df$index), ]
    previous_df <- previous_df[order(previous_df$index), ]
    values_comparison <- dfc.compare_df_values(current_df, previous_df, "index")
    values_comparison$values_match <- all(values_comparison$col_summary$num_unmatched == 0)
  } else{
    values_comparison <- NA
    side_by_side_df <- NA
  }
  comparison <- list(
    "colname_comparison" = colname_comparison,
    "index_comparison" = index_comparison,
    "dim_comparison" = dim_comparison,
    "values_comparison" = values_comparison
  )
  return(comparison)
}

compare_to_previous_summary <- function(comparison_to_previous){
  # takes an object returned by compare_to_previous and summarizes deviations
  # into readable df format (returns a data.frame)
  n_cols_unmatched <- sum(
    length(comparison_to_previous$colname_comparison$only_in_first),
    length(comparison_to_previous$colname_comparison$only_in_second)
  )
  n_indices_only_in_current <- length(comparison_to_previous$index_comparison$only_in_first)
  n_indices_only_in_previous <- length(comparison_to_previous$index_comparison$only_in_second)
  nrows_diff <- comparison_to_previous$dim_comparison$nrows_comparison$current -
    comparison_to_previous$dim_comparison$nrows_comparison$previous
  ncols_diff <- comparison_to_previous$dim_comparison$ncols_comparison$current -
    comparison_to_previous$dim_comparison$ncols_comparison$previous

  values_match <- NA
  if(length(comparison_to_previous$values_comparison) > 1){
    values_match <- all(comparison_to_previous$values_comparison$col_summary$num_unmatched == 0)
  }
  return(
    data.frame(
      n_cols_unmatched,
      n_indices_only_in_current,
      n_indices_only_in_previous,
      nrows_diff,
      ncols_diff,
      values_match
    )
  )
}

dfc.get_granular_mismatches <- function(values_comparison, id_cols = "index"){
  # takes a values_comparison object (returned by dfc.compare_values)
  # and returns a data.frame with one row per value difference in the data.frames
  # that were compared. Variable names and df index values appear in the rows of this df.
  # id_cols — when working with the output of dfc.compare_values, the id_col will usually
  # be "index," a composite identifier produced with dfc.concatenate_ids. However,
  # the user can specify their own index columns in case they've modified the output of dfc.compare_values,
  # e.g., to de-concatenate the index columns or add new index columns whose job it is to handle duplicates.

  sbs_df <- values_comparison$side_by_side_df

  # make sure all id_cols appear in values_comparison$side_by_side_df
  if(!all(id_cols %in% names(sbs_df))){
    stop("in get_granular_mismatches, the id_cols you specified do not all appear in values_comparison$side_by_side_df. (" %+%
           paste0(id_cols, collapse = ", ") %+% ")")
  }

  # check for duplicated combinations of id_cols in values_comparison
  if(any(duplicated(sbs_df[id_cols]))){
    stop("cannot get granular mismatches because duplicated id_col values were found. Please handle duplicates and try again.")
  }

  col_summary <- values_comparison$col_summary
  vars <- col_summary$variable_name[col_summary$num_unmatched > 0]
  suffixes <- c("_df1", "_df2", "_matches")
  var_names <- expand.grid(vars, suffixes) %>%
    setNames(c("variable", "suffix")) %>%
    mutate(colname = variable %+% suffix)

  # restrict to rows with at least one mismatch (to save computation time)
  sbs_df$at_least_one_mismatch <- values_comparison$row_summary$num_unmatched > 0
  sbs_df <- sbs_df[sbs_df$at_least_one_mismatch, ]

  # to look at granular mismatches, restrict to columns with unmatched values,
  # handle duplicate indices by marking each instance
  # melt, and cast
  granular_mismatches <- sbs_df[c(var_names$colname, id_cols)] %>%
    melt(., id.vars = id_cols) %>%
    rename(colname = variable) %>%
    # merge in var_names to separate variables from suffixes
    merge(., var_names, by = "colname") %>%
    # clean up the suffix column
    mutate(suffix = gsub("^_", "", suffix)) %>%
    # cast
    dcast(paste0(id_cols, collapse = " + ") %+% " + variable ~ suffix") %>%
    mutate(matches = as.logical(matches)) %>%
    filter(!matches)
  return(granular_mismatches)
}