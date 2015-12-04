###############################################################
###
###     util.R
###     The heart of gymnast. Load before other files.
###     It is broken up into multiple functional sections.
###
###############################################################


###############################################################
### 
###     Package Installation
###
###############################################################

gymnast_install <- function () {
    # Load (or install) all packages used by gymnast.
    dependencies <- c(
        "digest","dplyr","formatR","ggplot2",
        "grid","Hmisc","knitr","lme4",
        "lmerTest","psych","reshape2","scales" ,  
        "stargazer","stringr","xtable"
    )
    for(lib_name in dependencies){
        message(paste0("Loading... ",lib_name))
        # Require returns FALSE if packages failed
        if( !require(lib_name, character.only = TRUE) ){
            install.packages(lib_name)
            # library raises error if installation failed
            library(lib_name, character.only = TRUE)
        }
    }
}

gymnast_install()


###############################################################
### 
###     Extended Base
###     Improves basic functionality of R.
###
###############################################################


util.tidy <- function(source = "clipboard") {
    # cleans up coding style; by default from the clipboard
    # 1. copy ugly code to clipboard
    # 2. call util.tidy() in console
    # 3. paste outputted code to editor
    tidy_source(source=source, arrow = TRUE, width.cutoff = 78)
}


# Syntactic sugar for string concatenation, Example:
# > "hello" %+% "world"
# [1] "helloworld"
"%+%" <- function (x, y) { paste(x, y, sep="") }


util.apply_columns <- function(df, fun, ...){
    # returns a data.frame with fun applied to every column.
    # ellipsis args are passed to fun, applied to all columns.
    
    # stringsAsFactors=FALSE prevents factorizing characters.
    # check.names=FALSE avoids adding extra characters to colnames
    data.frame(
        lapply(df, fun, ...), 
        stringsAsFactors = FALSE, 
        check.names=FALSE
    )
} 

###############################################################
### 
###     String Cleaning
###     It's for updating strings.
###
###############################################################

util.strip_special_characters <- function(x){
    # only retain numbers, (latin) letters, and spaces
    letters_numbers_spaces_whitelist <- "[^0-9A-Za-z ]"
    gsub(letters_numbers_spaces_whitelist,"",x)
}


util.to_lowercase_and_numbers <- function(x){
    x %>%
        tolower() %>%
        util.strip_special_characters() %>%
        gsub(" ", "", .) # remove all spaces
}

# This is a unit test that should be re-written.
# if(! util.to_lowercase_and_numbers(" D*  i") %in% "di"){
#     stop("util.to_lowercase_and_numbers failed test.")
# }


util.trim <- function(x){
    # trim leading/trailing spaces and tabs
    gsub("(^[\t ]+)|([\t ]+$)", "", x)
}


util.strip_non_acsii <- function(x){
    # deletes non-ASCII characters, e.g., ø, ñ, etc.
    # e.g., Ekstrøm becomes Ekstrm
    iconv(x, "latin1", "ASCII", sub="")
}


###############################################################
### 
###     Data Types
###     Checking and coercing data types.
###
###############################################################


util.is_blank <- function(x){
    # true if tab, space, empty space, NA, NaN
    is.na(x) | grepl("^[ \t]*$", x)
}


util.to_character <- function(x){
    # like base::as.character()
    # but accepts (and returns) data frames or vectors
    if(class(x) %in% "data.frame"){
        util.apply_columns(x, as.character)
    }
    else{ as.character(x) }
}


util.to_acsii <- function(x){
    if(class(x) %in% "data.frame"){
        util.apply_columns(x, util.strip_non_acsii)
    }
    else{ as.util.strip_non_acsii(x) }
}

util.is_vector_of_numbers <- function(x){
    # Are all elements of x numbers?
    # (regardless of whether x is numeric)
    # Character vectors sometimes need to be changed to numerics,
    # e.g., when all columns set to character types by default.
    
    # find numeric values
    numeric_values <- grepl("^[[:digit:]]*\\.*[[:digit:]]+$",x)
    
    # find the blank values
    blank_values <- util.is_blank(x)
    
    # Return TRUE if all values are either numeric or blank
    # Return FALSE otherwise
    if(all(numeric_values | blank_values)){
        return(TRUE)
    }
    else{
        return(FALSE)
    }
}

util.as_numeric_if_number <- function(x){
    # run as.numeric if the x is made up of numbers
    # runs independently on each column if x is data.frame
    if(class(x) %in% "data.frame"){
        return(util.apply_columns(x, util.as_numeric_if_number))
    }
    if(util.is_vector_of_numbers(x)){
        x <- as.numeric(x)
    }
    return(x)
}


###############################################################
### 
###     Messages
###     Prettier messages that print to consolte or HTML.
###
###############################################################

util.print_pre <- function(x){
    # prints to html as it would to console (preformatted html)
    if(interactive()) return(x) 
    capture.output(print(x)) %>%
        paste(collapse="\n") %>%
        paste("<pre>",.,"</pre>") %>%
        cat()
}

util.warn <- function(message) {
    if (interactive()) {
        warning(message)
    } else {
        paste0("<div class='warning'>", message, "</div>") %>%
            cat()
    }
}

util.caution <- function(message) {
    if (interactive()) {
        message(message)
    } else {
        paste0("<div class='caution'>", message, "</div>") %>%
            cat()
    }
}

util.passed <- function(message) {
    if (interactive()) {
        cat(message)
    } else {
        paste0("<div class='pass'>", message, "</div>") %>%
            cat()
    }
} 

###############################################################
### 
###     Table Printing
###     Print tables to HTML (or console) reasonably.
###
###############################################################


util.html_table_from_model <- function(model){
    accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
    if( ! any(class(x) %in% accepted_models ) ){
        util.warn("Unaccepted model supplied!")
    }
    if( ! interactive() ){
        stargazer(
            model, 
            type="html", 
            star.cutoffs = c(.05, .01, .001),
            notes        = "", 
            notes.label = "1 star p<.05; 2 stars p<.01; 3 stars p<.001",
            notes.append = FALSE,
            single.row=TRUE
        )
    }
    else{
        util.print_pre(summary(model))
    }
}

util.html_table_data_frame <- function(x){
    # "grouped_df", "tbl_df" are dplyr type data.frames
    # ungroup to make them printable like a data.frame
    if(any(class(x) %in% c("grouped_df", "tbl_df"))){
        x <- data.frame(ungroup(x))
    }
    
    if( ! interactive() ){
        print(xtable(x), 
              type="html",
              html.table.attributes = 
                  getOption("xtable.html.table.attributes", 
                            "border=0, class='xtable'")
        )  
    }else{
        return(x)
    }
}

util.html_table_psych_alphas <- function(x){
    # psych::alpha object, turn key data into data.frame
    if(! all(class(x) %in% c("psych","alpha"))){
        util.warn("Not a psych::alpha object!")
    } 
    # extract the alpha coefficients for printing
    x <- x$total
    util.html_table_data_frame(x)
}

util.html_table <- function(x, ...) {
    accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
    accepted_psych_objects <- c("psych","alpha")
    accepted_dfs <- c("grouped_df", "tbl_df","data.frame","table")
    
    if( any(class(x) %in% accepted_models ) ){
        util.html_table_from_model(x)
    }
    
    if( all( class(x) %in% accepted_psych_objects ) ){
        util.html_table_psych_alphas(x)
    }
    
    if( any(class(x) %in% accepted_dfs ) ){
        util.html_table_data_frame(x)
    }
}


###############################################################
### 
###     Math
###     Functions that involve mathematical manipulation.
###
###############################################################



util.z_score <- function(x){
    # calculate the z-score of vector or for each vector in a data.frame
    if(class(x) %in% "data.frame"){
        util.apply_columns(x,util.z_score)
    }
    else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
}

util.row_means <- function(x){
    # like base::rowMeans(x, na.rm=TRUE) 
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowMeans(x, na.rm = TRUE))
}

util.row_sums <- function(x){
    # like base::rowSums(x, na.rm=TRUE) 
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowSums(x, na.rm = TRUE))
}

util.round_df <- function(DF, digits=2){
    # round all numeric columns    
    round_if_number <- function(x, digits=digits){
        if(util.is_vector_of_numbers(x)){
            x <- round(x, digits)
        }
        return(x)
    }
    
    util.apply_columns( DF,
                        round_if_number,
                        digits=digits)
}



###############################################################
### 
###     Reshaping
###     Manipulating the structure of data.
###
###############################################################


# Data Binding
util.rbind_many <- function(dfs, keep="intersection"){
    # keep "intersection" or "union" of columns
    # what columns should remain in the r-bound df?
    columns <- names(dfs[[1]])
    for(i in 2:length(dfs)){
        if(keep == "intersection"){
            columns <- columns[ columns %in% names(dfs[[i]])]
        }else{ # keep == "union"
            columns <- unique(c( columns, names(dfs[[i]]) ))
        }
    }
    if(length(columns) < 2){
        stop("2+ columns must match for result to be a data.frame.")
    }
    for(i in 1:length(dfs)){
        # fill out missing columns
        dfs[[i]][, columns[! columns %in% names(dfs[[i]]) ]] <- NA
        # remove extraneous columns
        dfs[[i]] <- dfs[[i]][, columns]
    }
    do.call(rbind,dfs)
}

# rbind list of dfs, keep intersecting column names
util.rbind_intersection <- function(dfs){
    util.rbind_many(dfs, keep="intersection")
}

# rbind list of dfs, keep union of column names
util.rbind_union <- function(dfs){
    util.rbind_many(dfs, keep="union")
}

# demo of util.rbind_many
# x <- data.frame( b=c(1,2,3), a=c(1,2,3), z=c(1,2,3) )
# y <- data.frame( b=c(4,5,6), a=c(4,5,6), d=c(4,5,6))
# z <- data.frame( b=c(7,8,9), a=c(7,8,9), e=c(7,8,9))
# dfs <- list(x,y,z)
# util.rbind_union(dfs)
# util.rbind_intersection(dfs)



###############################################################
### 
###     Value Replacement
###     Smartly replacing data values with other values.
###
###############################################################


util.recode <- function(vector, originals, replacements){
    # replace appearances of "originals" with "replacements"
    for(i in 1:length(originals)){
        vector[vector %in% originals[i] ] <- replacements[i]
    }
    vector
}

# vector <- rep(c("a","b","fefe","C","c"),100000)
# x <- c("a","b","c")
# y <- c("A","B","C")
# system.time(recode <- util.recode(vector,x,y))

util.reverse_likert <- function(v, scale_levels) {
    # Require that responses are numeric so that we can do arithmetic
    reversed_data <- as.numeric(v)
    # Change 6's to 1's and 2's to 5's, etc.
    return(scale_levels - reversed_data + 1)
}



###############################################################
###
###     Package Installation
###
###############################################################

gymnast_install <- function () {
    # Load (or install) all packages used by gymnast.
    dependencies <- c(
        "digest","dplyr","formatR","ggplot2",
        "grid","Hmisc","knitr","lme4",
        "lmerTest","psych","reshape2","scales" ,
        "stargazer","stringr","xtable"
    )
    for(lib_name in dependencies){
        message(paste0("Loading... ",lib_name))
        # Require returns FALSE if packages failed
        if( !require(lib_name, character.only = TRUE) ){
            install.packages(lib_name)
            # library raises error if installation failed
            library(lib_name, character.only = TRUE)
        }
    }
}

gymnast_install()


###############################################################
###
###     Extended Base
###     Improves basic functionality of R.
###
###############################################################

util.tidy <- function(source = "clipboard") {
    # cleans up coding style; by default from the clipboard
    # 1. copy ugly code to clipboard
    # 2. call util.tidy() in console
    # 3. paste outputted code to editor
    tidy_source(source=source, arrow = TRUE, width.cutoff = 78)
}


# Syntactic sugar for string concatenation, Example:
# > "hello" %+% "world"
# [1] "helloworld"
"%+%" <- function (x, y) { paste(x, y, sep="") }


util.apply_columns <- function(df, fun, ...){
    # returns a data.frame with fun applied to every column.
    # ellipsis args are passed to fun, applied to all columns.

    # stringsAsFactors=FALSE prevents factorizing characters.
    # check.names=FALSE avoids adding extra characters to colnames
    data.frame(
        lapply(df, fun, ...),
        stringsAsFactors = FALSE,
        check.names=FALSE
    )
}


###############################################################
###
###     String Cleaning
###     It's for updating strings.
###
###############################################################

util.strip_special_characters <- function(x){
    # only retain numbers, (latin) letters, and spaces
    letters_numbers_spaces_whitelist <- "[^0-9A-Za-z ]"
    gsub(letters_numbers_spaces_whitelist,"",x)
}

util.to_lowercase_and_numbers <- function(x){
    x %>%
        tolower() %>%
        util.strip_special_characters() %>%
        gsub(" ", "", .) # remove all spaces
}

# This is a unit test that should be re-written.
# if(! util.to_lowercase_and_numbers(" D*  i") %in% "di"){
#     stop("util.to_lowercase_and_numbers failed test.")
# }

util.trim <- function(x){
    # trim leading/trailing spaces and tabs
    gsub("(^[\t ]+)|([\t ]+$)", "", x)
}

util.strip_non_acsii <- function(x){
    # deletes non-ASCII characters, e.g., ø, ñ, etc.
    # e.g., Ekstrøm becomes Ekstrm
    iconv(x, "latin1", "ASCII", sub="")
}



###############################################################
###
###     Data Types
###     Checking and coercing data types.
###
###############################################################


util.is_blank <- function(x){
    # true if tab, space, empty space, NA, NaN
    is.na(x) | grepl("^[ \t]*$", x)
}

util.to_character <- function(x){
    # like base::as.character()
    # but accepts (and returns) data frames or vectors
    if(class(x) %in% "data.frame"){
        util.apply_columns(x, as.character)
    }
    else{ as.character(x) }
}

util.to_acsii <- function(x){
    if(class(x) %in% "data.frame"){
        util.apply_columns(x, util.strip_non_acsii)
    }
    else{ as.util.strip_non_acsii(x) }
}

util.is_vector_of_numbers <- function(x){
    # Are all elements of x numbers?
    # (regardless of whether x is numeric)
    # Character vectors sometimes need to be changed to numerics,
    # e.g., when all columns set to character types by default.

    # find numeric values
    numeric_values <- grepl("^[[:digit:]]*\\.*[[:digit:]]+$",x)

    # find the blank values
    blank_values <- util.is_blank(x)

    # Return TRUE if all values are either numeric or blank
    # Return FALSE otherwise
    if(all(numeric_values | blank_values)){
        return(TRUE)
    }
    else{
        return(FALSE)
    }
}

util.as_numeric_if_number <- function(x){
    # run as.numeric if the x is made up of numbers
    # runs independently on each column if x is data.frame
    if(class(x) %in% "data.frame"){
        return(util.apply_columns(x, util.as_numeric_if_number))
    }
    if(util.is_vector_of_numbers(x)){
        x <- as.numeric(x)
    }
    return(x)
}



###############################################################
###
###     Messages
###     Prettier messages that print to consolte or HTML.
###
###############################################################

util.print_pre <- function(x){
    # prints to html as it would to console (preformatted html)
    if(interactive()) return(x)
    capture.output(print(x)) %>%
        paste(collapse="\n") %>%
        paste("<pre>",.,"</pre>") %>%
        cat()
}

util.warn <- function(message) {
    if (interactive()) {
        warning(message)
    } else {
        paste0("<div class='warning'>", message, "</div>") %>%
            cat()
    }
}

util.caution <- function(message) {
    if (interactive()) {
        message(message)
    } else {
        paste0("<div class='caution'>", message, "</div>") %>%
            cat()
    }
}

util.passed <- function(message) {
    if (interactive()) {
        cat(message)
    } else {
        paste0("<div class='pass'>", message, "</div>") %>%
            cat()
    }
}

###############################################################
###
###     Table Printing
###     Print tables to HTML (or console) reasonably.
###
###############################################################


util.html_table_from_model <- function(model){
    accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
    if( ! any(class(x) %in% accepted_models ) ){
        util.warn("Unaccepted model supplied!")
    }
    if( ! interactive() ){
        stargazer(
            model,
            type="html",
            star.cutoffs = c(.05, .01, .001),
            notes        = "",
            notes.label = "1 star p<.05; 2 stars p<.01; 3 stars p<.001",
            notes.append = FALSE,
            single.row=TRUE
        )
    }
    else{
        util.print_pre(summary(model))
    }
}

util.html_table_data_frame <- function(x){
    # "grouped_df", "tbl_df" are dplyr type data.frames
    # ungroup to make them printable like a data.frame
    if(any(class(x) %in% c("grouped_df", "tbl_df"))){
        x <- data.frame(ungroup(x))
    }

    if( ! interactive() ){
        print(xtable(x),
              type="html",
              html.table.attributes =
                  getOption("xtable.html.table.attributes",
                            "border=0, class='xtable'")
        )
    }else{
        return(x)
    }
}

util.html_table_psych_alphas <- function(x){
    # psych::alpha object, turn key data into data.frame
    if(! all(class(x) %in% c("psych","alpha"))){
        util.warn("Not a psych::alpha object!")
    }
    # extract the alpha coefficients for printing
    x <- x$total
    util.html_table_data_frame(x)
}

util.html_table <- function(x, ...) {
    accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
    accepted_psych_objects <- c("psych","alpha")
    accepted_dfs <- c("grouped_df", "tbl_df","data.frame","table")

    if( any(class(x) %in% accepted_models ) ){
        util.html_table_from_model(x)
    }

    if( all( class(x) %in% accepted_psych_objects ) ){
        util.html_table_psych_alphas(x)
    }

    if( any(class(x) %in% accepted_dfs ) ){
        util.html_table_data_frame(x)
    }
}


###############################################################
###
###     Math
###     Functions that involve mathematical manipulation.
###
###############################################################



util.z_score <- function(x){
    # calculate the z-score of vector or for each vector in a data.frame
    if(class(x) %in% "data.frame"){
        util.apply_columns(x,util.z_score)
    }
    else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
}

util.row_means <- function(x){
    # like base::rowMeans(x, na.rm=TRUE)
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowMeans(x, na.rm = TRUE))
}

util.row_sums <- function(x){
    # like base::rowSums(x, na.rm=TRUE)
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowSums(x, na.rm = TRUE))
}

util.round_df <- function(DF, digits=2){
    # round all numeric columns
    round_if_number <- function(x, digits=digits){
        if(util.is_vector_of_numbers(x)){
            x <- round(x, digits)
        }
        return(x)
    }

    util.apply_columns( DF,
                        round_if_number,
                        digits=digits)
}



###############################################################
###
###     Reshaping
###     Manipulating the structure of data.
###
###############################################################


# Data Binding
util.rbind_many <- function(dfs, keep="intersection"){
    # keep "intersection" or "union" of columns
    # what columns should remain in the r-bound df?
    columns <- names(dfs[[1]])
    for(i in 2:length(dfs)){
        if(keep == "intersection"){
            columns <- columns[ columns %in% names(dfs[[i]])]
        }else{ # keep == "union"
            columns <- unique(c( columns, names(dfs[[i]]) ))
        }
    }
    if(length(columns) < 2){
        stop("2+ columns must match for result to be a data.frame.")
    }
    for(i in 1:length(dfs)){
        # fill out missing columns
        dfs[[i]][, columns[! columns %in% names(dfs[[i]]) ]] <- NA
        # remove extraneous columns
        dfs[[i]] <- dfs[[i]][, columns]
    }
    do.call(rbind,dfs)
}

# rbind list of dfs, keep intersecting column names
util.rbind_intersection <- function(dfs){
    util.rbind_many(dfs, keep="intersection")
}

# rbind list of dfs, keep union of column names
util.rbind_union <- function(dfs){
    util.rbind_many(dfs, keep="union")
}

# demo of util.rbind_many
# x <- data.frame( b=c(1,2,3), a=c(1,2,3), z=c(1,2,3) )
# y <- data.frame( b=c(4,5,6), a=c(4,5,6), d=c(4,5,6))
# z <- data.frame( b=c(7,8,9), a=c(7,8,9), e=c(7,8,9))
# dfs <- list(x,y,z)
# util.rbind_union(dfs)
# util.rbind_intersection(dfs)



###############################################################
###
###     Value Replacement
###     Smartly replacing data values with other values.
###
###############################################################


util.recode <- function(vector, originals, replacements){
    # replace appearances of "originals" with "replacements"
    for(i in 1:length(originals)){
        vector[vector %in% originals[i] ] <- replacements[i]
    }
    vector
}


util.reverse_likert <- function(v, scale_levels) {
    # Require that responses are numeric so that we can do arithmetic
    reversed_data <- as.numeric(v)
    # Change 6's to 1's and 2's to 5's, etc.
    return(scale_levels - reversed_data + 1)
}


###############################################################
###
###     Column Operations
###     Transforming values at the column level.
###
###############################################################

util.rename_columns <- function(data, mapping) {
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
  if(any(mapping$new_name %in% naughty_words) |
     any(grepl(whitespace_pattern, mapping$new_name)) |
     any(grepl(starting_w_digit_pattern, mapping$new_name))) {
    stop("Invalid new names are present among: ", paste(mapping$new_name, collapse =" "))
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

###### testing for rename_columns ######

### TEST 1: handling forbidden names

(function() {
  
  my_mapping <- list(
    old_name = c("foo", "bar", "baz", "blerp"),
    new_name = c("newfoo", "newbar", "newbaz", NA)
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    bar = c(6,7,8,9,10),
    baz = c(11,12,13,14,15),
    blerp = c(16,17,18,19,20)
  )
  
  success <- FALSE
  
  # @todo: abstract away this "assert error" tryCatch operation
  tryCatch(
    expr = {
      rename_columns(d, my_mapping)
    },
    error = function(e) {
      success <<- TRUE
    }
  )
  return(success)
})()


### TEST 2: handling extra data columns that aren't in mapping

(function() {
  
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
  
  new_d <- util.rename_columns(d, my_mapping)
  
  return(identical(new_d, expected_output))
  
})()


### TEST 3: extra mapping columns aren't present in data

(function() {
  
  my_mapping <- list(
    old_name = c("foo", "bar"),
    new_name = c("newfoo", "newbar")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5)
  )
  
  expected_output <- data.frame(
    newfoo = c(1,2,3,4,5)
  )
  
  new_d <- util.rename_columns(d, my_mapping)
  
  return(identical(new_d, expected_output))
  
})()

### TEST 4: duplicates in names of final data

(function() {
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
      util.rename_columns(d, my_mapping)
    },
    error = function(e) {
      function_correctly_stopped <<- TRUE
    }
  )
  return(function_correctly_stopped)
  
})()

### TEST 5: old_name and new_name are different lengths

(function() {
  my_mapping <- list(
    old_name = c("foo", "bar", "baz"),
    new_name = c("newfoo", "newbar")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    bar = c(6,7,8,9,10)
  )
  
  function_correctly_stopped <- FALSE
  tryCatch(
    expr = {
      util.rename_columns(d, my_mapping)
    },
    error = function(e) {
      function_correctly_stopped <<- TRUE
    }
  )
  return(function_correctly_stopped)
  
})()

### TEST 6: basic performance

(function() {
  
  my_mapping <- list(
    old_name = c("foo", "bar", "baz"),
    new_name = c("newfoo", "newbar", "newbaz")
  )
  
  d <- data.frame(
    foo = c(1,2,3,4,5),
    bar = c(6,7,8,9,10),
    baz = c("a", "b", "c", "d", "e")
  )
  
  expected_output <- data.frame(
    newfoo = c(1,2,3,4,5),
    newbar = c(6,7,8,9,10),
    newbaz = c("a", "b", "c", "d", "e")
  )
  
  new_d <- util.rename_columns(d, my_mapping)
  
  return(identical(new_d, expected_output))
  
})()

###### end of tests for util.rename_columns ######

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
  
  validate_accepted_values <- function(column_attributes, x) {
    if(!all(x %in% column_attributes$accepted_values)) {
      util.warn(column_attributes$column %+% " has unaccepted values.")
    }
  }
  
  validate_accepted_range <- function(column_attributes, x){
    if(!is.numeric(x)) {
      util.warn(column_attributes$column %+% " is not numeric, but " %+%
                  "you set min and max values for it.")
    }
    x_min <- min(column_attributes$accepted_range)
    x_max <- max(column_attributes$accepted_range)
    if(!all(x >= x_min & x <= x_max, na.rm = TRUE)) {
      util.warn(column_attributes$column %+%
                  " has out of range values.")
    }
  }
  
  validate_blanks_allowed <- function(column_attributes, x){
    # if blanks are not allowed and there are any blanks, throw warning
    if(!column_attributes$blanks_allowed & any(util.is_blank(x))) {
      util.warn(column_attributes$column %+% " has blank values, when none are allowed.")
    }
  }
  
  validate_required <- function(column_attributes, df_names){
    # make sure the "column_required" attribute exists
    if(exists("column_attributes$column_required")){
      # if it does, throw a warning if the column is required but it doesn't exist
      if(column_attributes$column_required & (!column_attributes$column %in% df_names)){
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

### Unit testing for validate_columns ###

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