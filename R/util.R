modules::import(
    'dplyr',
    `%>%`,
    'arrange',
    'distinct',
    'filter',
    'left_join',
    'mutate',
    'n',
    'pull'
)

logging <- import_module("logging")

`%+%` <- paste0

apply_columns <- function(df, fun, ...){
    # returns a data.frame with fun applied to every column.
    # ellipsis args are passed to fun, applied to all columns.

    # stringsAsFactors=FALSE prevents factorizing characters.
    # check.names=FALSE avoids adding extra characters to colnames
    data.frame(
        lapply(df, fun, ...),
        stringsAsFactors = FALSE,
        check.names=FALSE
    )
}

na_omit <- function(x){
  # returns all values of vector, matrix or data.frame x that are not NA,
  # but maintains the type of x (in contrast with stats::na.omit which adds a
  # bunch of unsolicited attributes to vectors) For 2-d objects, util.na_omit
  # returns zero-row objects rather than removing whole columns of all-NA
  # values (which na.omit does)
  if(is.atomic(x) & is.vector(x)){
    return(x[!is.na(x)])
  }
  if(is.matrix(x) | is.data.frame(x)){
    return(x[stats::complete.cases(x), ])
  }
  stop("util.na_omit is for vectors, matrices and data.frames only.")
}

# util.na_omit_test <- function(){
#   x_char <- c("1", "a", NA)
#   x_number <- c(1, 2, NA)
#   x_matrix <- matrix(nrow = 2, ncol = 2, data = c(NA, 1, 2, 3))
#   x_matrix_allb <- matrix(nrow = 3, ncol = 2, c(1,2,3,NA,NA,NA))
#   x_list <- list(a = c(NA, 1), b = c(2, 3))
#   x_df <- data.frame(a = c(NA, 1), b = c(2, 3))
#   x_df_allb <- data.frame(a = c(1, 2), b = c(NA, NA))
#   x_tibble <- dplyr::as_tibble(x_df)

#   assert <- stopifnot
#   assert(util.na_omit(x_char) == c("1", "a"))
#   assert(util.na_omit(x_number) == c(1, 2))
#   # I couldn't think of how to test that it throws an error for lists
#   assert(util.na_omit(x_matrix) == matrix(nrow = 1, ncol = 2, data = c(1, 3)))
#   # make sure a 2-d matrix has still been returned as well
#   assert(dim(util.na_omit(x_matrix)) == c(1,2))
#   # see what happens when matrix has an all-blank col
#   assert(util.na_omit(x_matrix_allb) == matrix(ncol = 2, nrow = 0))
#   assert(util.na_omit(x_df) == data.frame(a = 1, b = 3))
#   assert(util.na_omit(x_tibble) == dplyr::tibble(a = 1, b = 3))
#   # where there are all blank cols, the result should be a zero-row data.frame
#   # that preserves column names and number of columns
#   assert(nrow(util.na_omit(x_df_allb)) == 0)
#   assert(names(util.na_omit(x_df_allb)) == names(x_df_allb))
# }

###############################################################
###
###     Renaming
###
###############################################################


prefix_columns <- function (df, prefix, sep = '.') {
  # Add a prefix to all columns of a data frame.
  # Example:
  # prefixed <- prefix_columns(mtcars, "foo")
  # names(prefixed)  # [1] "foo.mpg"  "foo.cyl" ...
  names(df) <- gsub('(.*)', paste0(prefix, paste0(sep, '\\1')), names(df))
  return(df)
}

suffix_columns <- function (df, prefix, sep = '.') {
  # Add a suffix to all columns of a data frame.
  # Example:
  # suffixed <- suffix_columns(mtcars, "foo")
  # names(suffixed)  # [1] "mpg.foo"  "cyl.foo" ...
  names(df) <- gsub('(.*)', paste0(prefix, paste0('\\1', sep)), names(df))
  return(df)
}


###############################################################
###
###     Value Replacement
###     Smartly replacing data values with other values.
###
###############################################################


recode <- function(vector, originals, replacements){
    # replace appearances of "originals" with "replacements"
    if(length(originals) != length(replacements)){
        logging$warning("Original and replacement should have equal length.")
    }
    if(any(duplicated(originals))){
        logging$warning("Originals should not include duplicate values.")
    }

    new_vec <- vector
    for(v in originals){
        new_vec[vector == v] <- replacements[originals == v]
    }
    new_vec
}

# Someday we'll make this a real testthat test
# gymnast_test__util.recode <- function(){
#     # basic test
#     test_name <- "basic replacement"
#     vector <- c("a","b","fefe","c")
#     x <- c("a","b","c")
#     y <- c("A","B","C")
#     expectation <- c("A","B","fefe","C")
#     if( !identical(expectation,util.recode(vector,x,y)) ){
#         util.warn("recode fails: " %+% test_name)
#     }

#     test_name <- "multiple replacement includes originals"
#     vector <- c("a","b","fefe","c")
#     x <- c("a","b","c")
#     y <- c("b","c","d")
#     expectation <- c("b","c","fefe","d")
#     if( !identical(expectation,util.recode(vector,x,y))){
#         util.warn("recode fails: " %+% test_name)
#     }
# }


###############################################################
###
###     String Cleaning
###     It's for updating strings.
###
###############################################################


strip_special_characters <- function(x){
    # only retain numbers, (latin) letters, and spaces
    letters_numbers_spaces_whitelist <- "[^0-9A-Za-z ]"
    gsub(letters_numbers_spaces_whitelist,"",x)
}


to_lowercase_and_numbers <- function(x){
    x %>%
        tolower() %>%
        strip_special_characters() %>%
        gsub(" ", "", .) # remove all spaces
}

# This is a unit test that should be re-written.
# if(! util.to_lowercase_and_numbers(" D*  i") %in% "di"){
#     stop("util.to_lowercase_and_numbers failed test.")
# }


strip_non_ascii <- function(x){
    # deletes non-ASCII characters, e.g., ø, ñ, etc.
    # e.g., Ekstrøm becomes Ekstrm
    iconv(x, "latin1", "ASCII", sub="")
}


###############################################################
###
###     Reshaping
###     Manipulating the structure of data.
###
###############################################################


# Data Binding
rbind_many <- function(dfs, keep="intersection"){
    # keep "intersection" or "union" of columns
    # what columns should remain in the r-bound df?
    if (length(dfs) %in% 1) {
        return(dfs[[1]])
    }
    columns <- names(dfs[[1]])
    for(i in 2:length(dfs)){
        if(keep == "intersection"){
            columns <- columns[ columns %in% names(dfs[[i]])]
        }else{ # keep == "union"
            columns <- unique(c( columns, names(dfs[[i]]) ))
        }
    }
    if(length(columns) < 2){
        stop("2+ columns must match for result to be a data.frame.")
    }
    for(i in 1:length(dfs)){
        # fill out missing columns
        dfs[[i]][, columns[! columns %in% names(dfs[[i]]) ]] <- NA
        # remove extraneous columns
        dfs[[i]] <- dfs[[i]][, columns]
    }
    do.call(rbind,dfs)
}

# rbind list of dfs, keep intersecting column names
rbind_intersection <- function(dfs){
    rbind_many(dfs, keep="intersection")
}

# rbind list of dfs, keep union of column names
rbind_union <- function(dfs){
    rbind_many(dfs, keep="union")
}

# demo of rbind_many
# x <- data.frame( b=c(1,2,3), a=c(1,2,3), z=c(1,2,3) )
# y <- data.frame( b=c(4,5,6), a=c(4,5,6), d=c(4,5,6))
# z <- data.frame( b=c(7,8,9), a=c(7,8,9), e=c(7,8,9))
# dfs <- list(x,y,z)
# rbind_union(dfs)
# rbind_intersection(dfs)



###############################################################
###
###     Data Types
###     Checking and coercing data types.
###
###############################################################


is_blank <- function(x){
    # true if tab, space, empty space, NA, NaN
    is.na(x) | grepl("^[ \t]*$", x)
}

is_present <- function(x){
    ! is_blank(x)
}

is_vector_of_numbers <- function(x){
    # Are all elements of x numbers (permitting blanks)?
    # (regardless of whether x is numeric)
    # Character vectors sometimes need to be changed to numerics,
    # e.g., when all columns set to character types by default.

    # determine if x is logical first, because as.numeric will coerce logicals
    # to boolean 0,1 rather than to NA, meaning util.is_vector_of_numbers would return TRUE
    if(is.logical(x)) return(FALSE)

    # find numeric values
    x_as_numeric <- suppressWarnings(as.numeric(x))

    # anything that gets coerced to NA by as.numeric is not really a number.
    # but elements that were blank originally are ok.
    non_numeric <- is.na(x_as_numeric)
    originally_blank <- is_blank(x)

    # Return FALSE if there are any coerced blanks, because that means x
    # contained elements that could not be converted to numeric.
    # Otherwise return TRUE.
    if(any(non_numeric & !originally_blank)){
        return(FALSE)
    }
    else{
        return(TRUE)
    }
}

# util.is_vector_of_numbers_test <- function(){
#     x_char <- c("1", "a", "b", "1.1", "1e05", "a1", "1a")
#     x_number <- c("1", "1.1", "2", "-1.1",  ".1", "0.1", "00.1", "1.00", "1.1", "01")
#     x_scientific <- c("1e05")
#     x_with_blanks <- c(x_number, "", NA)
#     x_logical <- c(TRUE, FALSE)

#     assert <- stopifnot
#     assert(!util.is_vector_of_numbers(x_char))
#     assert(util.is_vector_of_numbers(x_number))
#     assert(util.is_vector_of_numbers(x_scientific))
#     assert(util.is_vector_of_numbers(x_with_blanks))
#     assert(!util.is_vector_of_numbers(x_logical))
# }

as_numeric_if_number <- function(x){
    # run as.numeric if the x is made up of numbers
    # runs independently on each column if x is data.frame
    if("data.frame" %in% class(x)){
        return(apply_columns(x, as_numeric_if_number))
    }
    if(is_vector_of_numbers(x)){
        x <- as.numeric(x)
    }
    return(x)
}


###############################################################
###
###     Formatting
###
###############################################################


clean_percent <- function(numerator, denominator, blank_value = 0) {
  raw_pct <- round(numerator / denominator * 100)
  is_blank <- is.na(raw_pct) | is.nan(raw_pct) | is.infinite(raw_pct)
  ifelse(is_blank, blank_value, raw_pct)
}

str_percent <- function(numerator, denominator, blank_value = 'N/A') {
  raw_pct <- clean_percent(numerator, denominator, blank_value = NA)
  ifelse(is.na(raw_pct), blank_value, paste0(as.character(raw_pct), '%'))
}


###############################################################
###
###     Files
###
###############################################################


list_all <- function (initial_path, max_depth = 2, type = 'all', current_depth = 0) {
    # Like base::list.files and base::list.dirs, but better:
    # * Can choose to list only files, or only directories (specify `type`).
    # * Rather than choosing between no recursion (not very useful) and full
    #   recursion (potentially very slow for a deep folder tree) you can set a
    #   max depth. Choose 0 for no recursion.
    # * Ignores hidden and system files, defined as anything starting with
    #   '.', '$', or '~'.
    #
    # Args:
    #   initial_path: atomic char, directory to scan for files.
    #   max_depth: atomic int, default 2, how many subfolders deep to scan for
    #     files. Zero means enter no subfolders.
    #   type: atomic char, default 'all', or choose 'dirs' or 'files'.
    #   current_depth: internal use only, do not specify.
    #
    # Returns: char of absolute file paths

    # Remove the trailing slash if it exists.
    len <- nchar(initial_path)
    if (substr(initial_path, len, len) == '/') {
        initial_path <- substr(initial_path, 1, len - 1)
    }

    # List everything within this path, both files and dirs.
    all_names <- list.files(initial_path, pattern = '^[^\\.\\$~]',
                            full.names = TRUE, recursive = FALSE)

    # file.info() returns a data frame, use it to separate files and dirs.
    info <- file.info(all_names)
    dirs <- all_names[info$isdir %in% TRUE]  # careful, isdir can be NA
    files <- all_names[info$isdir %in% FALSE]

    if (type == 'files') {
        out <- files
    } else if (type == 'dirs') {
        out <- dirs
    } else {
        out <- all_names
    }

    # If not at max depth, recurse into each found directory.
    if (current_depth < max_depth) {
        for (d in dirs) {
            out <- c(out, list_all(
                d, max_depth = max_depth, type = type,
                current_depth = current_depth + 1))
        }
    }

    return(out)
}

list_files <- function (initial_path, ...) {
    # Lists only files, not directories. See list_all().
    list_all(initial_path, type = 'files', ...)
}

list_dirs <- function (initial_path, ...) {
    # Lists only directories, not files. See list_all().
    list_all(initial_path, type = 'dirs', ...)
}

find_crypt_paths <- function (
    files_to_load,
    initial_path = NA,
    volume_patterns = NA,
    max_depth = 2
) {
    # Find the full paths of specified files within any mounted crypts.
    # Designed to work with read_csv_files().
    #
    # Example call:
    # > find_crypt_paths(list(school_a = 'School A/data.csv'))
    # $school_a
    # [1] "/Volumes/NO NAME 1/CC 10/School A/data.csv"
    #
    # Args:
    #   file_to_load: list or character, mapping of arbitrary labels to file
    #     names or partial file paths, as specific as necessary to find the
    #     file. If you give 'data.csv' and this function finds several of those
    #     within mounted crypts, it will stop. The solution is to be more
    #     specific, e.g. 'CC10-11/data.csv'
    #   initial_path: atomic char, default '/Volumes', the parent directory
    #     where crypt files are mounted (not applicable in Windows).
    #   volume_patterns: char, regexes that are expected to match volume
    #     names. Default matches volumes that start with "NO NAME" or
    #     "Untitled".
    #   max_depth: atomic int, default 2, how many subfolders deep to scan for
    #     files. Zero means enter no subfolders.
    #
    # Returns: List or character (according to type of file_to_load argument)
    # with provided labels to absolute file paths.

    if (.Platform$OS.type == 'unix') {
        # You may want to set this to '/media' if you're using linux.
        if (is.na(initial_path)) {
            initial_path <- '/Volumes'
        }

        if (is.na(volume_patterns)) {
            volume_patterns <- c('NO.NAME', 'Untitled')
        }

        pattern <- '(' %+% paste(volume_patterns, collapse = '|') %+% ')'

        all_volume_paths <- list.dirs(initial_path, recursive = FALSE)
        mount_paths <- all_volume_paths[grepl(
            pattern, ignore.case = TRUE, all_volume_paths)]

    } else if (.Platform$OS.type == 'windows') {
        initial_path <- NA
        # What drive letters exist and are not the operating system?
        all_drives <- paste0(letters, ':/')
        is_os <- sapply(all_drives, function (d) 'Windows' %in% list.files(d))
        mount_paths <- all_drives[file.exists(all_drives) & !is_os]
    }

    # Compile a list of files from each mount path.
    crypt_paths <- c()
    for (m in mount_paths) {
        crypt_paths <- c(list_all(m, max_depth = max_depth), crypt_paths)
    }

    # For each file to load, scan the list of known files for a match.
    if (is.list(files_to_load)) {
        found_paths <- list()
    } else if (is.character(files_to_load)) {
        found_paths <- character(0)
    } else {
        stop(paste0(
            "Bad type for `files_to_load`, got ",
            typeof(files_to_load)
        ))
    }
    for (x in sequence(length(files_to_load))) {
    # for (label in names(files_to_load)) {

        file_name <- files_to_load[x]

        # We only want to match the end of the path, whether it's a file or a
        # directory, so trim everything to the length of the file name before
        # checking for an exact match. Avoid regex because escaping any
        # regex special characters that may be in the user-provided file names
        # is hard with all the proliferating backslashes.
        p_len <- nchar(file_name)  # pattern length
        s_len <- nchar(crypt_paths)  # subject length
        crypt_path_endings <- substr(crypt_paths, s_len - p_len + 1, s_len)

        match <- crypt_paths[crypt_path_endings == file_name]
        if (length(match) > 1) {
            stop("Multiple matches found for " %+% file_name %+% ": " %+%
                 match %+% "\n")
        } else {
            if (is.list(files_to_load)) {
                label <- names(files_to_load)[x]
            } else if (is.character(files_to_load)) {
                label <- x
            }
            found_paths[[label]] <- match
        }
    }

    return(found_paths)
}


###############################################################
###
###     Math
###     Functions that involve mathematical manipulation.
###
###############################################################



z_score <- function(x){
    # calculate the z-score of vector or for each vector in a data.frame
    if("data.frame" %in% class(x)){
        apply_columns(x,util.z_score)
    }
    else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
}

row_means <- function(x, na.rm = TRUE){
    # like base::rowMeans(x, na.rm=TRUE)
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowMeans(x, na.rm = na.rm))
}

ordinal <- function(vec){
  # return a vector with the ordinals corresponding to vec values
  # e.g., 4,5,5,3,NA,8 returns 2,3,3,1,NA,4
  # The line commented out below should return true.
  # all( util.ordinal(c(4,5,5,3,NA,8)) == c(2,3,3,1,NA,4), na.rm=TRUE )
  if(all(is.na(vec))){ return(vec) }

  df <- data.frame(original = vec)
  vec_to_ord <- df %>%
    filter( ! is.na(original) ) %>%
    distinct() %>%
    arrange(original) %>%
    mutate(ordinal = 1:n())

  left_join(df, vec_to_ord, by="original") %>% pull(ordinal)
}



###############################################################
###
###     Dates and Times
###
###############################################################


find_day_of_week <- function (increment, day_abbr) {
  current_day <- Sys.Date()
  for (i in 1:7) {
    day_of_week <- current_day %>%
      lubridate::wday(., label = TRUE)
    if (day_of_week == day_abbr) {
      return(current_day)
    }
    current_day <- current_day + increment
  }
}

next_monday <- function() find_day_of_week(+1, "Mon")
last_monday <- function() find_day_of_week(-1, "Mon")
