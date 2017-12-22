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


# helper function:
# compare vector elements without regard to order.
# return a list with two components - the elements in vec1 and not vec2, and those in vec2 and not vec1.
util_dfc.compare_vecs <- function(vec1, vec2) {

}

# compare the column names of two data frames.
# return a list of information about how the name-sets differ:
## whether they match perfectly
## colnames in df1 and not df2
## colnames in df2 and not df1
util_dfc.compare_colnames <- function(df1, df2) {
# use util_dfc.compare_vecs on colnames.
}


# compare two dfs on a set of one or more columns that should uniquely
# identify each row. optionally remove duplicates too if you just want to
# check that the same identifiers are present.
util_dfc.compare_identifiers <- function(df1, df2, id_cols, remove_dups = FALSE) {
  # check id_cols to see if it's empty and if it's in both dfs
  # if id_cols is length 1, set id_col as col name in each df
    # else, set id_col as col name in each df as a concatenation of id_cols with "__" in between.
    # treat NAs as strings saying "NA".
  # if remove_dups, remove duplicates in each id_col
  # warn if id_col has duplicates in either df!
  # use util_dfc.compare_vecs on id_cols of each df.
}

