
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

