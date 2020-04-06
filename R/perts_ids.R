# See gae_server/gae_models/datastore_model.py

modules::import('stringr', 'str_split')

# Letters only, in StandingCamelCase
kind_pattern <- '([A-Z][a-z]*)+'

# Letters, numbers, hyphen.
id_pattern <- '[A-Za-z0-9\\-]+'

long_uid_part_pattern <- paste0('^(', kind_pattern, ')_(', id_pattern, ')$')
short_uid_part_pattern <- paste0('^', id_pattern, '$')

choose <- function (n) {
  function (v) v[n]
}

create_uid <- function (kind = NULL) {
  characters <- c(LETTERS, letters, "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  short_uid <- paste(sample(characters, 12, replace = TRUE), collapse = "")
  if (is.null(kind)) {
    return(short_uid)
  } else {
    if (!grepl(paste0('^', kind_pattern, '$'), kind)) {
      stop(paste0("Invalid kind: ", kind))
    }
    return(paste0(kind, "_", short_uid))
  }
}

get_kind <- function (uid) {
  # Vector-capable.
  #
  # Example:
  # > get_kind(c("Team_123", "User_456"))
  # [1] "Team" "User"
  if (!all(grepl('_', uid))) {
    stop("perts_ids$get_kind(): not a long uid, no underscore.")
  }
  unlist(Map(choose(1), str_split(uid, '_')))
}

get_short_uid <- function (short_or_long_uid) {
  # Expects unitary vector
  #
  # Example:
  # > get_short_uid("Team_123")
  # [1] "123"
  if (is_short_uid(short_or_long_uid)) {
    short_uid <- short_or_long_uid # already short
  } else if (is_long_uid(short_or_long_uid)) {
    parts <- str_split(short_or_long_uid, '\\.')[[1]]
    kind_id_pairs <- str_split(parts, '_')
    ids <- Map(choose(2), kind_id_pairs)
    short_uid <- paste0(ids, collapse = '.')
  } else {
    stop("perts_ids$get_short_uid(): invalid id")
  }
  return(short_uid)
}

get_long_uid <- function (kind, short_or_long_uid) {
  # Expects unitary vectors
  #
  # Example:
  # > get_long_uid("Team", "123")
  # [1] "Team_123"
  if (is_long_uid(short_or_long_uid)) {
    if (get_kind(short_or_long_uid) != kind) {
      stop("perts_ids$get_long_uid(): got a long uid with a different kind")
    }
    long_uid <- short_or_long_uid # already long
  } else if (is_short_uid(short_or_long_uid)) {
    parts <- str_split(short_or_long_uid, '\\.')[[1]]
    long_uid <- paste0(paste(kind, short_or_long_uid, sep = '_'), collapse = '.')
  } else {
    stop("perts_ids$get_long_uid(): invalid id")
  }
  return(long_uid)
}

is_long_uid <- function (short_or_long_uid) {
  # Expects unitary vector. Note that there is a specific regex both the kind
  # and id part must match, it's not just the underscore that matters.
  #
  # Example:
  # > is_long_uid("123")
  # [1] FALSE
  #
  # > is_long_uid("Team_123")
  # [1] TRUE
  #
  # > is_long_uid("abc_123") # doesn't start with a captial letter
  # [1] FALSE
  parts <- str_split(short_or_long_uid, '\\.')[[1]]
  all(grepl(long_uid_part_pattern, parts))
}

is_short_uid <- function (short_or_long_uid) {
  # Expects unitary vector. Note that there is a specific regex both the kind
  # and id part must match, it's not just the underscore that matters.
  #
  # Example:
  # > is_short_uid("123")
  # [1] TRUE
  #
  # > is_short_uid("Team_123")
  # [1] FALSE
  #
  # > is_short_uid("123%") # disallowed character
  # [1] FALSE
  parts <- str_split(short_or_long_uid, '\\.')[[1]]
  all(grepl(short_uid_part_pattern, parts))
}
