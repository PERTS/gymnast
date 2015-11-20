


util.is_blank <- function(x){
  is.na(x) | grepl("^[ \t]*$", x)
}

# same base::as.character() 
# but accepts (and returns) data frames or vectors
util.to_character <- function(x){
  if(class(x) %in% "data.frame"){
    util.apply_columns(x, as.character)
  }
  else{ as.character(x) }
}


# Are all elements of x numbers?
# (regardless of whether x is numeric)
util.is_vector_of_numbers <- function(x){
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

# run as.numeric if the x is made up of numbers
# runs independently on each column if x is data.frame
util.as_numeric_if_number <- function(x){
  if(class(x) %in% "data.frame"){
    return(util.apply_columns(x, util.as_numeric_if_number))
  }
  if(util.is_vector_of_numbers(x)){
    x <- as.numeric(x)
  } 
  return(x)
} 


