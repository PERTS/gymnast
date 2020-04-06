extract_groups <- function (pattern, subject) {
  # Get values of regex capturing groups from a string.
  #
  # Args:
  #   pattern - character, length 1, regular expression
  #   subject - character, legnth 1, in which to search for pattern
  #
  # Returns a list where the names are the substrings matching the pattern, and
  # the values are vectors of the values of each capturing group in that
  # substring.
  #
  # Examples:
  #
  # > regex$extract_groups('(\\d)(.)bc', '1abc123abc')
  # $`1abc`
  # [1] "1"  "a"
  #
  # $`3abc`
  # [1] "3"  "a"
  #
  # > regex$extract_groups('\\dabc', '1abc')
  # $`1abc`
  # character(0)

  if (length(pattern) != 1 || length(subject) != 1) {
    stop("extract_groups() requires both arguments to be length 1")
  }
  matches <- regmatches(subject, gregexpr(pattern, subject))
  group_matches <- sapply(
    matches[[1]],
    function(m) regmatches(m, regexec(pattern, m))
  )
  # Process each instance of the pattern matching within the subject.
  group_values <- Map(
    function (x) {
      # The first value is the pattern match itself, regardless of groups.
      wo_first <- x[-1]
      # The length of the return value should be match the number of groups. If
      # there are zero groups, this should have length zero.
      wo_na <- wo_first[!is.na(wo_first)]
      return(wo_na)
    },
    group_matches
  )

  return(group_values)
}
