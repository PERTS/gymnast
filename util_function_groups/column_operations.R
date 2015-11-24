################################################################################

source("~/Sites/gymnast/util.R", chdir = FALSE)
source("~/Sites/gymnast/util_qualtrics_cleaning.R", chdir = FALSE)

validate_columns <- function(df, column_validation){
    
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
        print("Validated datatype")
    }
    
    validate_accepted_values <- function(column_attributes, x){
        if(!all(x %in% column_attributes$accepted_values)){
            util.warn(column_attributes$column %+% " has non-accepted values.")
        }
        print("Validated accepted values")
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
        print("Validated accepted range")
    }
    
    validate_blanks_allowed <- function(column_attributes, x){
        # if blanks are not allowed and there are any blanks, throw warning
        if(!column_attributes$blanks_allowed & any(util.is_blank(x))){
            util.warn(column_attributes$column %+% " has blank values, when none are allowed.")
        }
        print("Validated blanks allowed")
    }
    
    validate_required <- function(column_attributes, df_names){
        # make sure the "column_required" attribute exists
        if(exists("column_attributes$column_required")){
            # if it does, throw a warning if the column is required but it doesn't exist
            if(column_attributes$column_required & !column_attributes$column %in% df_names){
                util.warn("Required column " %+% column %+% " does not appear in the data.")
            }
        }
        print("Validated required")
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

################################ Unit testing ####################################

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

column_validation <- list(
    # some archetype columns
    "nonexistant_required" = list(
        "column_required" = TRUE
    ),
    "nonexistant_not_required" = list(
        "column_required" = FALSE
    ),
    "blanks_in_wrong_place" = list(
        "blanks_allowed" = FALSE
    ),
    "out_of_range" = list(
        "accepted_range" = c(1, 7)
    ),
    "blanks_not_among_accepted_values" = list(
        "accepted_values" = c("Value1", "Value2")
    ),
    # some real columns from Qualtrics:
    "race" = list(
        "datatype" = "character",
        "accepted_values" = c("White", "Black", "Latino", "Asian", "Other", "Unknown"),
        "column_required" = FALSE,
        "blanks_allowed" = TRUE
    ),
    "ddowell" = list(
        "datatype" = "numeric",
        "accepted_range" = c(1, 7),
        "blanks_allowed" = TRUE
    ),
    # an open response field
    "dlrn_open" = list(
        "datatype" = "character",
        "blanks_allowed" = FALSE
    )
)
