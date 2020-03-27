# to import: logging <- import_module("logging")
# to call:
#  logging$info("my message")
#  logging$info("The situation is foo", bar)

# this is a list of functions which sends various messages to stdout,
# which are in turn properly displayed in the google app engine
# Note: we can also send messages to stderr() if needed, but currently
# we are not planning to do it. If needed, just change stdout() to stderr()

# Logging output can also be controlled with the environment variable
# LOG_LEVEL:
# - 'NONE' nothing logged by this module
# - 'DEBUG' the default, debug- and anything higher-level are emitted.
# - 'INFO' lower levels (debug) are not emitted, info and higher are
# - ...etc

# Example to turn off all logs:
#
# Sys.setenv(LOG_LEVEL = 'NONE')

.levels <- c('DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL', 'NONE')

.create_logger <- function (level) {
  return(function (...) {
    # Skip writing anything if this logger's level is lower than the
    # threshold.
    log_level <- ifelse(
      Sys.getenv("LOG_LEVEL") %in% .levels,
      Sys.getenv("LOG_LEVEL"),
      'DEBUG'
    )
    this_level_index <- which(.levels %in% level)
    log_level_index <- which(.levels %in% log_level)
    if (this_level_index < log_level_index) {
      # write(paste0('ignoring ', level, ' log b/c env set to ', log_level), stdout())
      return()
    }

    items <- c(level, ...)
    # Paste each, possibly lengthy, argument to a length 1
    # character so we don't get a cross product situation.
    flat_items <- Map(paste, items)
    msg <- paste(flat_items, collapse = " ")
    write(msg, stdout())
  })
}

debug = .create_logger('DEBUG')
info = .create_logger('INFO')
warning = .create_logger('WARNING')
error = .create_logger('ERROR')
critical = .create_logger('CRITICAL')
