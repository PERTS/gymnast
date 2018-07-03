if (!'ensure_packages' %in% ls()) {
  source('ensure_packages.R', local = TRUE)
}
ensure_packages('magrittr')

# Chain
`%>%` <- magrittr::"%>%"

# Concatentate

`%+%` <- function (x, y) { paste(x, y, sep="") }
