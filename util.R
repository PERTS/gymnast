# util.R

setwd("~/Sites/gymnast/")
source("install_gymnast_packages.R")

# Apply any function to all columns in a data.frame
# or to the elements of a vector.
# Other common approaches return a matrix or fail to remove factors
# e.g., apply() and do.call(cbind, lapply()) retain factors

util.apply_columns <- function(df, fun){
  # returns a data.frame with fun applied to every column.
  # sets stringsAsFactors = FALSE to prevent factorizing characters.
  # sets check.names = FALSE to avoid adding extra characters to column names by default.
  data.frame(lapply(df, fun), stringsAsFactors = FALSE, check.names=FALSE)
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
# system.time(recode <- util.recode(vector,x,y))

util.reverse_likert <- function(v, scale_levels) {
  # Require that responses are numeric so that we can do arithmetic
  reversed_data <- as.numeric(v)
  # Change 6's to 1's and 2's to 5's, etc.
  return(scale_levels - reversed_data + 1)
}


source("util_code_conventions.R")
source("util_string_cleaning.R")
source("util_datatypes.R")
source("util_messages.R")
source("util_printing_tables.R")
source("util_math.R")
source("util_reshaping.R")


