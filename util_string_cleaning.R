
# only retain numbers, (latin) letters, and spaces
util.strip_special_characters <- function(x){ 
  letters_numbers_spaces_whitelist <- "[^0-9A-Za-z ]"
  gsub(letters_numbers_spaces_whitelist,"",x) 
}

util.to_lowercase_and_numbers <- function(x){
  x %>%
    tolower() %>%
    util.strip_special_characters() %>%
    gsub("(^ +)|( +$)", "", .) %>%  # leading/trailing spaces
    gsub(" ", "", .)          # all spaces space
}

if(! util.to_lowercase_and_numbers(" D*  i") %in% "di"){
  stop("util.to_lowercase_and_numbers failed test.")
}


