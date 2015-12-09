
# replace appearances of originals with replacements inside the vector
util.recode <- function(vector, originals, replacements){
  # check if any of the replacement values also appear in the originals.
  if(any(replacements %in% originals)){
    # if they do:
    # check whether any shared values appear earlier in the list of
    # replacements than in the originals. If they do,
    # this will cause desired values to be over-written. For example, if
    # originals == c("a","b") and replacements == c("b", "c"), then you
    # probably want the vector c("a", "b", "a") to be recoded as c("b", "c",
    # "b"). But what you will actually get is c("c", "c", "c"), because the
    # "a"s get replaced with "b"s before the b's get replaced with c's. So warn
    # if this is happening, otherwise it's super-annoying to track down!
    # find the shared values
    shared_values <- replacements[replacements %in% originals] %>% unique
    # find their positions
    first_shared_replacements <- which(replacements %in% shared_values) %>% min
    first_shared_originals <- which(originals %in% shared_values) %>% min
    # warn if any shared values appear earlier in replacements than originals
    if(first_shared_replacements < first_shared_originals){
        util.warn("In util.recode()," %+%
            "you have some values in originals that also " %+%
            "appear in replacements. Some of these shared values may " %+%
            "cause recoded values to be over-written. If you don't " %+%
            "want this to happen, please check your recoded output " %+%
            "to make sure no desired values were over-written. " %+%
            "If they were, you'll need to change the order " %+%
            "of the values in originals and replacements to prevent " %+%
            "the over-writing.")
    }
  }
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

