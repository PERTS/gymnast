###############################################################
###
###     Messages
###     Prettier messages that print to console or HTML.
###
###############################################################

if (!"%>%" %in% ls()) {
  source('operators.R', local = TRUE)
}

util.print_pre <- function(x){
    # prints to html as it would to console (preformatted html)
    if(interactive()){
        print(x)
    } else{
        capture.output(print(x)) %>%
        paste(collapse="\n") %>%
        paste("<pre>",.,"</pre>") %>%
        cat()
    }
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
