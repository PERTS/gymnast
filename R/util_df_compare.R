##########################################################################
###
###     util_dfc.R
###     Dan Greene, 12/21/17
###
###     The goal of the util_dfc ("data frame compare") module is to make it easy to see
###     where two data frames differ for debugging and further analysis.
###
###     Depends on util.R
###


# REMOVE SECTION BEFORE FINALIZING
setwd("~/Sites/gymnast/R")
source("util.R")



util_dfc.compare_vecs <- function(vec1, vec2) {
  # compare vector elements without regard to order.
  # return a list with three components -
  ## shared_elements: the shared elements,
  ## only_in_first: the elements in vec1 and not vec2,
  ## only_in_second: the elements in vec2 and not vec1.
  return(list(shared_elements = intersect(vec1, vec2),
              only_in_first = setdiff(vec1, vec2),
              only_in_second = setdiff(vec2, vec1)))
}



util_dfc.compare_colnames <- function(df1, df2) {
  # compare the column names of two data frames using util_dfc.compare_vecs.
  # return a list with three components -
  ## shared_elements: the shared names,
  ## only_in_first: the names in df1 and not df2,
  ## only_in_second: the names in df2 and not df1

  # Sanity checks
  if(!is.data.frame(df1) | !is.data.frame(df2)) {
    stop("Error - at least one of the first two arguments for util_dfc.compare_colnames is not a data frame.")
  }

  return(util_dfc.compare_vecs(names(df1), names(df2)))
}




util_dfc.compare_identifiers <- function(df1, df2, id_cols, id_cols_uniquely_identify_rows = FALSE) {
  # Compare two dfs on a set of one or more commonly-shared columns that identify rows.
  # If rows are identified by multiple columns (e.g. team x student x week), include all column names in id_cols.
  #
  # If you expect id_cols to UNIQUELY identify rows in both dfs, set id_cols_uniquely_identify_rows to TRUE.
  # Otherwise, duplicate ID values within each data frame will get ignored.
  # Example use case: you want to compare a student-x-week df with a student df to see if they have the same students.
  # The commonly-shared id_col is student ID.
  # Since student ID doesn't uniquely identify rows in the first df, id_cols_uniquely_identify_rows = FALSE.

  # Sanity checks
  if(!is.data.frame(df1) | !is.data.frame(df2)) {
    stop("Error - at least one of the first two arguments for util_dfc.compare_unique_identifiers is not a data frame.")
  }
  if(is.na(id_cols) | length(id_cols) == 0) {
    stop("Error - id_cols argument is NA or empty.")
  }
  if(any(!id_cols %in% names(df1)) | any(!id_cols %in% names(df2))) {
    stop("Error - not all id columns provided are in both data frames.")
  }

  # if id_cols is length 1, set id_col as col name in each df
  # else, set id_col as col name in each df as a concatenation of id_cols with "__" in between.
  # Note: this treats NAs as the character string "NA" in an identifier.
  if(length(id_cols) == 1) {
    df1_ids <- df1[, id_cols] %>% as.character()
    df2_ids <- df2[, id_cols] %>% as.character()
  } else {
    df1_ids <- df1[, id_cols] %>% apply(., 1, paste, collapse = "__")
    df2_ids <- df2[, id_cols] %>% apply(., 1, paste, collapse = "__")
  }

  # If id_cols aren't expected to uniquely identify rows, then cut duplicates from df1_ids and df2_ids.
  if(!id_cols_uniquely_identify_rows) {
    df1_ids <- df1_ids[!duplicated(df1_ids)]
    df2_ids <- df2_ids[!duplicated(df2_ids)]
  }

  # warn if id_col has duplicates in either df!
  # This means that id_col doesn't uniquely identify rows as expected.
  if(any(duplicated(df1_ids))) {
    warning("Warning - first data frame passed to util_dfc.compare_unique_identifiers has unexpected duplicate ID values:")
    util.print_pre(unique(df1_ids[duplicated(df1_ids)]))
  }
  if(any(duplicated(df2_ids))) {
    warning("Warning - second data frame passed to util_dfc.compare_unique_identifiers has unexpected duplicate ID values:")
    util.print_pre(unique(df2_ids[duplicated(df2_ids)]))
  }

  # use util_dfc.compare_vecs on id_cols of each df
  return(util_dfc.compare_vecs(df1_ids, df2_ids))
}









# Big wrapper function for giving an overall picture of how two DFs differ:
## Check nrow and ncol
## Compare colnames
## Compare identifiers
## Subset to shared columns, sort columns and rows (using id_cols), and report which values match exactly in the subset.
# NOTE: need to subset to shared columns, or assume that the user has already done this. What's conceptually clear?
# consider checking by column first, then by row.
# Any way to see the data without getting bogged down by big dfs?
util_dfc.compare_dfs <- function(df1, df2, id_cols = c(), id_cols_uniquely_identify_rows = FALSE) {

  # Setup
  df1_name <- deparse(substitute(df1))
  df2_name <- deparse(substitute(df2))

  # Sanity checks
  if(!is.data.frame(df1) | !is.data.frame(df2)) {
    stop("Error - at least one of the first two arguments for util_dfc.compare_dfs is not a data frame.")
  }

  # Compare basic nrow and ncol
  diff_nrow <- FALSE
  util.print_pre("COMPARING ROW COUNTS: ")
  if(nrow(df1) == nrow(df2)) {
    util.print_pre(df1_name %+% " and " %+% df2_name %+% " both have " %+% nrow(df1) %+% " rows.")
  } else {
    diff_nrow <- TRUE
    util.print_pre("Warning - " df1_name %+% " has " %+% nrow(df1) %+% " rows, while " %+% df2_name %+%
                     " has " %+% nrow(df2) %+% " rows.")
  }

  # other stuff here

}

