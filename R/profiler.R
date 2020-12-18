# Profiler: Allows light-weight profiling of code execution. Writes to the
# global environment.
#
# Based on github.com/PERTS/gae_server/simple_profiler.py
#
# Example use:
#
#   profiler <- import_module("profiler")
#   profiler$add_event("lets-a go")
#
#   # in other files or modules sharing the same global environment
#   profiler <- import_module("profiler")
#   profiler$add_event("doing another thing")
#
#   # to view all profile events
#   profiler$print()
#
# You can also name your profiler:
#
#   profiler <- import_module("profiler")
#   profiler$add_event("start my script", "custom_profiler")
#
#   # in other files or modules sharing the same global environment
#   profiler <- import_module("profiler")
#   profiler$add_event("doing more stuff", "custom_profiler")
#
#   # to all profile events with this name:
#   profiler$print("custom_profiler")

modules::import("stringr", "str_pad")

# Export everything that doesn't start with a dot, i.e. things starting with
# a dot are private to this module.
modules::export("^[^.]")

.DEFAULT_NAME <- "default"
.MAX_MESSAGE_LENGTH <- 30

.get_milliseconds <- function() round(as.numeric(Sys.time()) * 1000)
.get_name <- function(n) paste0(".profiler.", n)

add_event <- function(message, profile_name = .DEFAULT_NAME) {
  if (exists(.get_name(profile_name), envir = globalenv())) {
    profile <- get(.get_name(profile_name), envir = globalenv())
  } else {
    profile <- list()
  }

  event <- list(
    message = substr(message, 1, .MAX_MESSAGE_LENGTH),
    time = .get_milliseconds()
  )
  profile[[length(profile) + 1]] <- event

  assign(.get_name(profile_name), profile, envir = globalenv())
}

clear <- function(profile_name = .DEFAULT_NAME) {
  assign(.get_name(profile_name), NULL, envir = globalenv())
}

clear_all <- function() {
  all_profile_names <- grep(
    "^\\.profiler",
    ls(all.names = TRUE, envir = globalenv()),
    value = TRUE
  )
  for (name in all_profile_names) {
    assign(name, NULL, envir = globalenv())
  }
}

printable_report <- function(profile_name = .DEFAULT_NAME) {
  profile <- get(.get_name(profile_name), envir = globalenv())

  report <- c(
    "",
    paste0("Profile: ", profile_name),
    # Width of message column should match .MAX_MESSAGE_LENGTH
    "Message                        Run Time  Total time",
    "---------------------------------------------------"
  )

  rows <- list()
  i <- 1
  previous_time <- NULL
  net_time <- 0
  for (event in profile) {
    if (i > 1) {
      t <- event$time - previous_time
      net_time <- net_time + t
      rows[[i - 1]]$time <- t
    }
    previous_time <- event$time
    rows[[i]] <- list(message = event$message, time = 0, net_time = net_time)
    i <- i + 1
  }
  for (row in rows) {
    report <- c(report, paste(
      str_pad(row$message, .MAX_MESSAGE_LENGTH, "right"),
      str_pad(row$time, 8, "left"),
      str_pad(row$net_time, 10, "left")
    ))
  }
  report <- c(report, "")

  paste(report, collapse = "\n")
}

print <- function(profile_name = .DEFAULT_NAME) {
  cat(printable_report(profile_name))
}
