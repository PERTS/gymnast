########################################################################################################
########################## Qualtrics Cleaning Functions ################################################

require(stringr)

KNOWN_QUALTRICS_COLUMNS <- c("ResponseID"
                             ,"ResponseSet"
                             ,"Name"
                             ,"ExternalDataReference"
                             ,"EmailAddress"
                             ,"IPAddress"
                             ,"Status"
                             ,"StartDate"
                             ,"EndDate"
                             ,"Finished")

# Warning if util.R has not been loaded
if(!any(grepl("^util\\.", objects()))){
  # util.R is required for util_qualtrics_cleaning.R
  # raise exception if util.R objects missing from workspace
  stop("util.R is required for util_qualtrics_cleaning.R.
       Run source util.R before source util_qualtrics_cleaning.R")
}

##########################
# Name variables appropriately

  # Input: all-character Qualtrics dataset, string-identifying function (e.g.,
  # pq.extract_delimited) Output: Qualtrics dataset with all variables named
  # appropriately.

  pq.insert_hidden_column_names <- function(qdf_char,
    extract_column_name=pq.extract_delimited, ...){

    question_text <- as.character(qdf_char[1,])
    q_labels <- names(qdf_char)

    # Find the hidden column names by applying the string-identifying function
    # to the question text.
    hidden_column_names <- sapply(question_text,extract_column_name,...)

    # start with the default labels
    best_column_names <- q_labels

    # Wherever hidden column names appear, replace default labels with hidden
    # column names
    valid <- !is.na(hidden_column_names)
    best_column_names[valid] <- hidden_column_names[valid]

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

  pq.extract_delimited <- function(x,delimiter="__pdd__"){
    # x is a string vector

    # define a regular expression based on the delimiter:
    regex <- paste0(delimiter,".+",delimiter)

    # now extract the delimited string based on the delimiter:
    # note that str_extract returns NA if no match is found.
    extracted_strings <- str_extract(x,regex)
    delimited_strings <- gsub(delimiter,"",extracted_strings)
    return(delimited_strings)
  }

  # Insert known Qualtrics columns that always appear in row 1 instead of in
  # the header for some unknown reason.
  pq.handle_known_Qualtrics_columns <- function(qdf_char,
    known_qualtrics_columns=KNOWN_QUALTRICS_COLUMNS){
    # insert known columns that appear in row 1
    is_known_qualtrics_column <- qdf_char[1,] %in% known_qualtrics_columns
    names(qdf_char)[is_known_qualtrics_column] <- as.character(qdf_char[1,is_known_qualtrics_column])
    return(qdf_char)
  }

  # Remove unnamed columns (optional)
  pq.remove_unnamed_columns <- function(qdf_rn){
      # this function removed default-named Qualtrics columns (e.g., V1, Q3)
      # it can be really useful to do this, but should be optional.
      known_regex <- "^Q[[:digit:]]+_?[[:digit:]]*$|^V[[:digit:]]+$"
      unnamed_columns <- grepl(known_regex, names(qdf_rn))
      qdf_rn <- qdf_rn[, !unnamed_columns]
      return(qdf_rn)
    }

  # a wrapper function for all the above column-naming procedures:
  pq.rename_columns <- function( qdf_char,
                                 extract_column_name=pq.extract_delimited,
                                 remove_unnamed_columns=FALSE,
                                 ...){

  # First, insert delimited column names (these should always override any
  # existing column names)
  qdf_rn <- pq.insert_hidden_column_names(qdf_char, extract_column_name, ...)

  # Now, insert known Qualtrics column names
  qdf_rn <- pq.handle_known_Qualtrics_columns(qdf_rn)

  # Optionally, remove unnamed columns
  if(remove_unnamed_columns){
    qdf_rn <- pq.remove_unnamed_columns(qdf_rn)
  }

  # Return a renamed Qualtrics data.frame
  return(qdf_rn)
}


##########################
# Wrap it all up

# Input: raw Qualtrics dataset, hidden column retrieving function, and
# Output: clean Qualtrics dataset

pq.clean_qualtrics <- function(qdf
                               ,remove_unnamed_columns=FALSE
                               ,extract_column_name=pq.extract_delimited
                               ,...){
  # takes a Qualtrics data.frame, a boolean indicating whether unnamed columns
  # should be removed, a function (pq.extract_delimited, by default), and
  # optional arguments to be passed to a user-created function fun
  qdf_char <- util.to_character(qdf)
  qdf_unicode <- util.to_unicode(qdf_char)
  qdf_rn <- pq.rename_columns(qdf_unicode,
    remove_unnamed_columns,
    extract_column_name=extract_column_name,
    ...)
  qdf_row_removed <- qdf_rn[-1,]
  qdf_clean <- util.as_numeric_if_number(qdf_row_removed)
  return(qdf_clean)
}

