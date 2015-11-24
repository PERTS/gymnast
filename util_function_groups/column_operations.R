############################### rename_columns function  #################################################

rename_columns <- function(data, mapping) {
  # This function takes a data set and a mapping (which can be a list or data frame)
  # with columns "old_name" and "new_name". It uses util.recode to turn the old names
  # into the new names, making sure not to break any R name rules.
  
  # Patterns for forbidden names
  whitespace_pattern <- "\\s"
  starting_w_digit_pattern <- "^\\.?[0-9]"
  naughty_words <- c("if", "else", "repeat", "while", "function", "for",
                     "in", "next", "break", "TRUE", "FALSE", "NULL", "Inf",
                     "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", NA)
  
  # if the new names in the mapping match any evil patterns, then complain
  if( any(mapping$new_name %in% naughty_words) |
      any(grepl(whitespace_pattern, mapping$new_name)) |
      any(grepl(starting_w_digit_pattern, mapping$new_name))) {
    stop("Invalid new names are present among: ",paste(mapping$new_name, collapse =" "))
  }
  
  # if old and new names are not the same length, then complain
  if(length(mapping$old_name) != length(mapping$new_name)) {
    stop("Error! Old names and new names are not the same length in your mapping for renaming.")
  }
  
  names(data) <- util.recode(names(data),
                             mapping$old_name,
                             mapping$new_name)
  
  # if new column names in the final data have duplicates, then complain
  if (any(duplicated(names(data)))) {
    stop("Error! Duplicated names in your renamed data. Review your mapping for renaming.")
  }
  
  return(data)
}

################################ testing for rename_columns  ################################################

######################## TEST 1: handling forbidden names


test_forbidden_names <- function() {
  
  my_mapping <- list(
    old_name = c("foo", "bar", "baz","blerp"),
    new_name = c("newfoo", "newbar", "newbaz", NA)
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    bar = c(6,7,8,9,10),
    baz = c(11,12,13,14,15),
    blerp = c(16,17,18,19,20)
  )
  
  success <- FALSE
  
  tryCatch(
    expr = {
      rename_columns(d,my_mapping)
    },
    error = function(e) {
      success <<- TRUE
    }
  )
  return(success)
}

test_forbidden_names()

######################## TEST 2: handling extra data columns that aren't in mapping

test_extra_data_columns <- function() {
  
  my_mapping <- list(
    old_name = c("foo"),
    new_name = c("newfoo")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    blerp = c(16,17,18,19,20)
  )
  
  expected_output <- data.frame(
    newfoo = c(1,2,3,4,5),
    blerp = c(16,17,18,19,20)
  )
  
  new_d <- rename_columns(d,my_mapping)
  
  return(identical(new_d, expected_output))
  
}

test_extra_data_columns()

######################## TEST 3: extra mapping columns aren't present in data

test_extra_mapping_columns <- function() {
  
  my_mapping <- list(
    old_name = c("foo","bar"),
    new_name = c("newfoo","newbar")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5)
  )
  
  expected_output <- data.frame(
    newfoo = c(1,2,3,4,5)
  )
  
  new_d <- rename_columns(d,my_mapping)
  
  return(identical(new_d, expected_output))
  
}

test_extra_mapping_columns()

######################## TEST 4: duplicates in names of final data

test_new_name_duplicates <- function() {
  my_mapping <- list(
    old_name = c("foo","bar","baz"),
    new_name = c("newfoo","newbar","newbar")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    bar = c(6,7,8,9,10),
    baz = c(11,12,13,14,15)
  )
  
  function_correctly_stopped <- FALSE
  tryCatch(
    expr = {
      rename_columns(d,my_mapping)
    },
    error = function(e) {
      function_correctly_stopped <<- TRUE
    }
  )
  return(function_correctly_stopped)
  
}

test_new_name_duplicates()

######################## TEST 5: old_name and new_name are different lengths

test_mapping_same_length <- function() {
  my_mapping <- list(
    old_name = c("foo","bar","baz"),
    new_name = c("newfoo","newbar")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    bar = c(6,7,8,9,10)
  )
  
  function_correctly_stopped <- FALSE
  tryCatch(
    expr = {
      rename_columns(d,my_mapping)
    },
    error = function(e) {
      function_correctly_stopped <<- TRUE
    }
  )
  return(function_correctly_stopped)
  
}

test_mapping_same_length()

######################## TEST 6: basic performance

test_basic_performance <- function() {
  
  my_mapping <- list(
    old_name = c("foo","bar","baz"),
    new_name = c("newfoo","newbar","newbaz")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    bar = c(6,7,8,9,10),
    baz = c("a","b","c","d","e")
  )
  
  expected_output <- data.frame(
    newfoo = c(1,2,3,4,5),
    newbar = c(6,7,8,9,10),
    newbaz = c("a","b","c","d","e")
  )
  
  new_d <- rename_columns(d,my_mapping)
  
  return(identical(new_d, expected_output))
  
}

test_basic_performance()

################################################################################

util.validate_columns <- function(df, column_validation){
    
    # define custom validation functions for each column attribute
    
    validate_datatype <- function(column_attributes, x){
        exp_type <- column_attributes$datatype
        actual_type <- class(x)
        if(exp_type != actual_type){
            util.warn(column_attributes$column 
                      %+% " data class is " %+% actual_type 
                      %+% ". Should be " %+% exp_type 
                      %+% ".")
        }
    }
    
    validate_accepted_values <- function(column_attributes, x){
        if(!all(x %in% column_attributes$accepted_values)){
            util.warn(column_attributes$column %+% " has unaccepted values.")
        }
    }
    
    validate_accepted_range <- function(column_attributes, x){
        if(!is.numeric(x)){
            util.warn(column_attributes$column %+% " is not numeric, but " %+%
                          "you set min and max values for it.")
        }
        x_min <- min(column_attributes$accepted_range)
        x_max <- max(column_attributes$accepted_range)
        if(!all(x >= x_min & x <= x_max, na.rm = TRUE)){
            util.warn(column_attributes$column %+%
                          " has out of range values.")
        }
    }
    
    validate_blanks_allowed <- function(column_attributes, x){
        # if blanks are not allowed and there are any blanks, throw warning
        if(!column_attributes$blanks_allowed & any(util.is_blank(x))){
            util.warn(column_attributes$column %+% " has blank values, when none are allowed.")
        }
    }
    
    validate_required <- function(column_attributes, df_names){
        # make sure the "column_required" attribute exists
        if(exists("column_attributes$column_required")){
            # if it does, throw a warning if the column is required but it doesn't exist
            if(column_attributes$column_required & !column_attributes$column %in% df_names){
                util.warn("Required column " %+% column %+% " does not appear in the data.")
            }
        }
    }
    
    for(column in names(column_validation)){
        
        # pick out the attributes of the column in question
        column_attributes <- column_validation[[column]]
        
        # add the column name itself as an attribute of the column
        column_attributes$column <- column
        
        # check whether the column exists
        if(column %in% names(df)){
            values <- df[,column]
            # If it does, run the appropriate checks
            for(attribute in names(column_attributes)){
                switch(attribute,
                       blanks_allowed = validate_blanks_allowed(column_attributes,values),
                       accepted_values = validate_accepted_values(column_attributes,values),
                       accepted_range = validate_accepted_range(column_attributes,values),
                       datatype = validate_datatype(column_attributes,values)
                )
            }
        }
        validate_required(column_attributes, names(df))
    }
}

################################ Unit testing for validate_columns ####################################

# @todo - turn into unit tests

# d <- read.csv("~/Documents/ctc_intervention.csv", stringsAsFactors = FALSE) %>%
#     qc.clean_qualtrics() %>% # clean the qualtrics
#     # add the test columns
#     mutate(
#         blanks_in_wrong_place = sample(c(1:7, NA), nrow(.), replace = TRUE),
#         out_of_range = sample(c(1:8, NA), nrow(.), replace = TRUE),
#         blanks_not_among_accepted_values = sample(c("Value1", "Value2", NA),
#                                                   nrow(.), replace = TRUE)
#     )
# 
# column_validation <- list(
#     # some archetype columns
#     "nonexistant_required" = list(
#         "column_required" = TRUE
#     ),
#     "nonexistant_not_required" = list(
#         "column_required" = FALSE
#     ),
#     "blanks_in_wrong_place" = list(
#         "blanks_allowed" = FALSE
#     ),
#     "out_of_range" = list(
#         "accepted_range" = c(1, 7)
#     ),
#     "blanks_not_among_accepted_values" = list(
#         "accepted_values" = c("Value1", "Value2")
#     ),
#     # some real columns from Qualtrics:
#     "race" = list(
#         "datatype" = "character",
#         "accepted_values" = c("White", "Black", "Latino", "Asian", "Other", "Unknown"),
#         "column_required" = FALSE,
#         "blanks_allowed" = TRUE
#     ),
#     "ddowell" = list(
#         "datatype" = "numeric",
#         "accepted_range" = c(1, 7),
#         "blanks_allowed" = TRUE
#     ),
#     # an open response field
#     "dlrn_open" = list(
#         "datatype" = "character",
#         "blanks_allowed" = FALSE
#     )
# )
