###############################################################
###
###     util_qualtrics_cleaning.R
###     Used to load and process Qualtrics CSV data files.
###
###     The main function call is:
###     clean_dataframe <- qc.clean_qualtrics(raw_qualtrics_csv)
###     
###     Depends on util.R already being loaded.
###
###############################################################

# util.R should have already been loaded in by the calling script.
# source("util.R")

STANDARD_SECOND_ROW_QUALTRICS_COLUMNS <- c(
  "ResponseID"
  ,"ResponseSet"
  ,"Name"
  ,"ExternalDataReference"
  ,"EmailAddress"
  ,"IPAddress"
  ,"Status"
  ,"StartDate"
  ,"EndDate"
  ,"Finished")

##########################
# Name variables appropriately

  qc.insert_hidden_column_names <- function(qdf_char,
    extract_column_name=qc.extract_delimited, ...){
    # Qualtrics uses two header rows, one for variables named in Qualtrics, and
    # one for the question text itself. We have a practice of "hiding" column
    # names in the second header row (in the question text) by wrapping them
    # in hidden <div>s. This function scans the first row of the data.frame for
    # these hidden values and fixes all the column names.

    # Inputs:
    # - all-character Qualtrics dataset with first row of data containing
    # the extra Qualtrics headers
    # - a string-identifying function (e.g., qc.extract_delimited)
    # - arbitrary args to be passed to the string-identifying function (...)

    # Output: Qualtrics dataset with all variables named
    # appropriately.

    question_text <- as.character(qdf_char[1, ])

    # start with the names we already have
    best_column_names <- names(qdf_char)

    # Find the hidden column names by applying the string-identifying function
    # to the question text.
    hidden_column_names <- sapply(question_text, extract_column_name, ...)

    # Wherever hidden column names appear, replace default labels with hidden
    # column names
    valid <- !is.na(hidden_column_names)
    best_column_names[valid] <- hidden_column_names[valid]

    # add any necessary _TEXT suffixes
    best_column_names <- qc.add_TEXT_suffixes(qdf_char, best_column_names)

    # throw a warning if the function being used yields column names that are
    # not unique
    if(any(duplicated(best_column_names))){
      warning("Your function for pulling column names from the first row " %+%
        "resulted in duplicate column names.")
    }

    # add the new column names to the data.frame
    names(qdf_char) <- best_column_names
    return(qdf_char)
  }


  qc.extract_delimited <- function(x,delimiter = "__pdd__"){
    # Input:
    # - string vector from first row of raw Qualtrics dataset containing second
    # Qualtrics header
    # - delimiter to demarcate the column name, e.g., "__pdd__toi_1__pdd__"
    # becomes "toi_1"
    # x is a string vector

    # define a regular expression based on the delimiter:
    regex <- paste0(delimiter,".+",delimiter)

    # now extract the delimited string based on the delimiter:
    # note that str_extract returns NA if no match is found.
    extracted_strings <- str_extract(x,regex)
    delimited_strings <- gsub(delimiter,"",extracted_strings)
    return(delimited_strings)
  }

  qc.handle_known_Qualtrics_columns <- function(
    qdf_char,
    known=STANDARD_SECOND_ROW_QUALTRICS_COLUMNS){
    # Insert known Qualtrics columns that always appear in row 1 instead of in
    # the header for some unknown reason.
    # insert known columns that appear in row 1
    is_known <- qdf_char[1,] %in% known
    names(qdf_char)[is_known] <- as.character(qdf_char[1,is_known])
    return(qdf_char)
  }

  qc.remove_unnamed_columns <- function(qdf_rn){
    # Remove unnamed columns (optional)
    # this function removed default-named Qualtrics columns (e.g., V1, Q3)
    # it can be really useful to do this, but should be optional.
    known_regex <- "^Q[[:digit:]]+_?[[:digit:]]*$|^V[[:digit:]]+$"
    unnamed_columns <- grepl(known_regex, names(qdf_rn))
    return(qdf_rn[, !unnamed_columns])
  }

  qc.add_TEXT_suffixes <- function(qdf_char, column_names){
    # Qualtrics has an infrequent question type where you can input an open-
    # ended response to a multiple-choice question (e.g., "something else
    # (write here)"). Qualtrics creates TWO columns for such questions, one for
    # the multiple-choice response, and one for the open-ended response.
    # Qualtrics distinguishes them in the column names by adding the suffix
    # "_TEXT". But it doesn't differentiate the question text in the first row.
    # We need the _TEXT suffix, otherwise qc.extract_hidden_column_names will
    # produce duplicate columns. So, add the TEXT suffix back.

    # find the _TEXT columns in the original variable names
    TEXT_suffix_columns <- grepl("_TEXT$", names(qdf_char))

    # add "_TEXT" to column_names wherever this suffix appears in the original
    # variable names
    column_names[TEXT_suffix_columns] <- column_names[TEXT_suffix_columns] %+%
      "_TEXT"

    return(column_names)
  }

  # a wrapper function for all the above column-naming procedures:
  qc.rename_columns <- function( qdf_char,
                                 extract_column_name=qc.extract_delimited,
                                 remove_unnamed_columns=FALSE,
                                 ...){

  # First, insert delimited column names (these should always override any
  # existing column names)
  qdf_rn <- qc.insert_hidden_column_names(qdf_char, extract_column_name, ...)

  # Now, insert known Qualtrics column names
  qdf_rn <- qc.handle_known_Qualtrics_columns(qdf_rn)

  # Optionally, remove unnamed columns
  if(remove_unnamed_columns){
    qdf_rn <- qc.remove_unnamed_columns(qdf_rn)
  }

  # Return a renamed Qualtrics data.frame
  return(qdf_rn)
}


##########################
# Wrap it all up

qc.clean_qualtrics <- function(
  qdf,
  remove_unnamed_columns=FALSE,
  extract_column_name=qc.extract_delimited,
  ...){
  # Input: raw Qualtrics dataset, hidden column retrieving function, and
  # Output: clean Qualtrics dataset
  # takes a Qualtrics data.frame, a boolean indicating whether unnamed columns
  # should be removed, a function (qc.extract_delimited, by default), and
  # optional arguments to be passed to a user-created function fun
  qdf_char <- util.to_character(qdf)
  qdf_unicode <- util.to_acsii(qdf_char)
  qdf_rn <- qc.rename_columns(qdf_unicode,
    remove_unnamed_columns,
    extract_column_name=extract_column_name,
    ...)
  qdf_row_removed <- qdf_rn[-1,]
  qdf_clean <- util.as_numeric_if_number(qdf_row_removed)
  return(qdf_clean)
}

qc.rbind_inprogress <- function(inprogress_qdf, clean_qdf){
  # Takes an in-progress Qualtrics dataset (inprogress_qdf), cleans it,
  # replaces its column names with those of the cleand Qualtrics dataset
  # (clean_qdf), and rbinds it to the bottom of cleaned_qdf. Note that if you
  # set remove_unnamed_columns to TRUE when running qc.clean_qualtrics, both
  # setNames and rbind will throw errors, because both depend on each
  # data.frame having the same number of columns.

  # note that i have to use nested functions in the line below because if I
  # pipe something to suppressWarnings, it throws an error.

  suppressWarnings(qc.clean_qualtrics(inprogress_qdf)) %>%
    setNames(., names(clean_qdf)) %>%
    rbind(clean_qdf, .)
}
