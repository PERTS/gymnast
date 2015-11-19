

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

