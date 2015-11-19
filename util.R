# util.R

setwd("~/Sites/gymnast/")
source("install_gymnast_packages.R")

# cleans up coding style; by default from the clipboard
# 1. copy ugly code to clipboard
# 2. call util.tidy() in console
# 3. paste outputted code to editor
util.tidy <- function(source = "clipboard") {
    tidy_source(source=source, arrow = TRUE, width.cutoff = 78)
}

# Syntactic sugar for string concatenation, Example:
# > "hello" %+% "world"
# [1] "helloworld"
"%+%" <- function (x, y) { paste(x, y, sep="") }

#   STRING CLEANING
util.strip_special_characters <- function(x){ gsub("[^0-9A-Za-z]","",x) }

util.clean_str <- function(x){
  x %>%
    tolower() %>%
    util.strip_special_characters() %>%
    gsub("^ +| +$", "", .) %>%  # leading/trailing spaces
    gsub(" +", " ", .)          # extra space
}
util.clean_string <- util.clean_str
# util.clean_str(" D* ") == "d"


# DATA TYPE CONVERSION

# Apply any function to all columns in a data.frame, or to the elements of a vector.

    # Other common approaches return a matrix, or else fail to remove factors
    # (for example, apply() and do.call(cbind, lapply()) both have these problems)

util.apply_columns <- function(df, fun){
  # returns a data.frame with fun applied to every column. sets
  # stringsAsFactors = FALSE to prevent factorizing characters.
  # sets check.names = FALSE to avoid adding extra characters to column names
  # by default.
    data.frame(lapply(df, fun), stringsAsFactors = FALSE, check.names=FALSE)
}

# same base::as.character() but accepts (and returns) data frames or vectors
util.to_character <- function(x){
  if(class(x) %in% "data.frame"){
    util.apply_columns(x, as.character)
  }
  else{ as.character(x) }
}

# replace appearances of originals with replacements inside the vector
util.recode <- function(vector, originals, replacements){
  # we profiled the timing, and this loop is fast, even with 10M+
  for(i in 1:length(originals)){
    vector[vector %in% originals[i] ] <- replacements[i]
  }
  vector
}
# vector <- rep(c("a","b","fefe","C","c"),100000)
# x <- c("a","b","c")
# y <- c("A","B","C")
# system.time(util.recode(vector,x,y))


# Strip elements containing non-unicode characters

	# Note that any time you are dealing with a file that contains open-text fields, especially from
	# survey data, it is likely that there will be a couple of non-unicode characters, such as
	# accent marks, umlauts, characters with tildes, etc. These non-unicode characters cause other
	# string manipulation functions to throw errors.

	# Note to Dave: The disadvantage to this method is that it
	# returns NA for values that contain non-unicode characters. In general this is rare, so I
	# haven't worried about it. But it might be worth digging around for a better solution, e.g.,
	# a function that merely replaces the non-unicode CHARACTER with a capital "X" or something
	# like that. I'd be interested in your thoughts on whether it's worth the time to fix this.

util.to_unicode <- function(x){
  if(class(x) %in% "data.frame"){
    util.apply_columns(x, iconv)
  }
  else{ iconv(x) }
}

# Determine whether all elements of x are numbers (regardless of whether x is numeric)

util.is_number <- function(x){
  # Determines whether all elements of x are numbers, regardless of whether x
  # is type character or type numeric, by evaluating whether all elements of x
  # match a regular expression where there is at least one digit character and
  # no non-digit characters A decimal is optional. This is useful when dealing
  # with character vectors that may need to be changed to numerics, e.g., in
  # Qualtrics datasets where all columns are set to character types by default,
  # even if they are number columns.

  # find the numeric values:
  numeric_values <- grepl("^[[:digit:]]*\\.*[[:digit:]]+$",x)
  #numeric_values <- x[!is.na(as.numeric(x))] # can also do this but it throws
  #a bunch of warnings.

  # find the non-blank values
  blank_values <- util.is_blank(x)

  # Return TRUE if values are either numeric or blank, and original vector
  # otherwise
  if(all(numeric_values | blank_values)){
    return(TRUE)
  }
  else{
     return(FALSE)
  }

}

util.as_numeric_if_number <- function(x){
  # If x is a data.frame, call util.as_numeric_if_number on each column of x
  # and return a data.frame with numbers converted to numerics.  If x is a
  # vector, return a character or numeric vector depending on the contents of x
  if(class(x) %in% "data.frame"){
    return(util.apply_columns(x, util.as_numeric_if_number))
  }
  else{
    if(util.is_number(x)){
      return(as.numeric(x))
    } else{
      return(x)
    }
  }
}

# HTML PRINTING

# maybe we should remove this function
util.html_table <- function(df, ...) {
  if(any(class(df) %in% c("grouped_df", "tbl_df"))){
    df <- data.frame(ungroup(df))
  }
  if(all(class(df) %in% c("psych","alpha"))){
    # print just the alpha coefficients
    df <- df$total
  }
  if( ! interactive() ){
    if( any( class(df) %in% c("lmerMod","lm","aov","glm","glmerMod") ) ){
      stargazer(df, type="html",
                star.cutoffs = c(.05, .01, .001),
                notes        = "",
                notes.label = "1 star p<.05; 2 stars p<.01; 3 stars p<.001",
                notes.append = FALSE,
                single.row=TRUE
      )
    }
    else{
      print(xtable(df, ...),
            type="html",
            html.table.attributes =
              getOption("xtable.html.table.attributes", "border=0, class='xtable'"), ...)
    }
  }
  else{
    if( any( class(df) %in% c("lmerMod","lm","aov","glm","glmerMod") ) ){
      print(summary(df))
    }else{
      print(df)
    }
  }
}

# prints to html as it would to console (preformatted html)
util.print_pre <- function(x){
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
        paste0("<div class='warning'>", message, "</div>") %>% cat()
    }
}

util.caution <- function(message) {
    if (interactive()) {
        message(message)
    } else {
        paste0("<div class='caution'>", message, "</div>") %>% cat()
    }
}

util.passed <- function(message) {
    if (interactive()) {
        cat(message)
    } else {
        paste0("<div class='pass'>", message, "</div>") %>% cat()
    }
}


# Basic Data Transformation
util.z_score <- function(x){
  if(class(x) %in% "data.frame"){
    do.call(cbind,lapply(x, util.z_score))
  }
  else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
}

util.is_blank <- function(x){
  is.na(x) | grepl("^[ \t]*$", x)
}

# round all numeric columns
util.round_df <- function(DF, digits=2, show_caution=TRUE){
  vec_list <- lapply( DF, function(vec){
    if( is.numeric(vec) ){
      vec <- round(vec,digits)
    }
    else{
      if(show_caution){
        util.caution("Any characters will make all columns characters.")
      }
    }
    return( vec )
  })
  do.call( cbind, vec_list ) %>%
    as.data.frame()
}

util.reverse_likert <- function(v, scale_levels) {
  # Require that responses are numeric so that we can do arithmetic
  reversed_data <- as.numeric(v)
  # Change 6's to 1's and 2's to 5's, etc.
  return(scale_levels - reversed_data + 1)
}

# Data Binding
# keep "intersection" or "union" of columns
util.rbind_many <- function(dfs, keep="intersection"){
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


