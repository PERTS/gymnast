
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


util.apply_columns <- function(df, fun){
  # returns a data.frame with fun applied to every column.
  # stringsAsFactors = FALSE prevents factorizing characters.
  # check.names = FALSE avoids adding extra characters to colnames
  
  # Other apply approaches return a matrix or retain factors
  # e.g., apply() and do.call(cbind, lapply()) retain factors
  data.frame(
    lapply(df, fun), 
    stringsAsFactors = FALSE, 
    check.names=FALSE
  )
} 

