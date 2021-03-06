###############################################################
###
###     util.R
###     The heart of gymnast. Load before other files.
###     It is broken up into multiple functional sections.
###
###############################################################


###############################################################
###
###     Extended Base
###     Improves basic functionality of R.
###
###############################################################

ensure_packages <- function (libs, prefer_type = "binary") {
  # Install `libs` if they aren't installed already.
  #
  # Args:
  #   libs - character, packages to maybe install
  #   prefer_type - character, default "binary", or "source", sets
  #     options(install.packages.check.source) which is an indirect way of
  #     controlling whether we use binary or source installation, since the
  #     `type` argument of install.packages() is platform-dependent and thus
  #     unreliable.
  if (prefer_type == "binary") {
    # Prefer binary packages, even if the source version is farther ahead.
    # This is default because it's so much faster to install a binary.
    options(install.packages.check.source = "no")
  } else if (prefer_type == "source") {
    options(install.packages.check.source = NULL)
  }

  installed <- utils::installed.packages()[,"Package"]
  to_install <- libs[!(libs %in% installed)]

  if (length(to_install)) {
    utils::install.packages(
      to_install,
      repos = 'http://cran.us.r-project.org'
      # type = 'binary'  # doesn't work for some platforms, e.g. linux
    )
  }
}

util.tidy <- function(source = "clipboard") {
    # cleans up coding style; by default from the clipboard
    # 1. copy ugly code to clipboard
    # 2. call util.tidy() in console
    # 3. paste outputted code to editor
    tidy_source(source=source, arrow = TRUE, width.cutoff = 78)
}


# Syntactic sugar for string concatenation, Example:
# > "hello" %+% "world"
# [1] "helloworld"
"%+%" <- function (x, y) { paste(x, y, sep="") }


util.apply_columns <- function(df, fun, ...){
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

util.duplicated_all <- function(x) {
  # Returns a boolean indicating whether each value is
  # duplicated anywhere in x. This differs from base::duplicated, which
  # returns FALSE for the first of a set of duplicated values.
  #
  # Example: util.duplicated(c(1, 2, 1)) returns c(T, F, T).i
  duplicated_downward <- duplicated(x)
  duplicated_upward <- duplicated(x, fromLast=TRUE)
  return (duplicated_downward | duplicated_upward)
}

util.na_omit <- function(x){
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

util.na_omit_test <- function(){
  x_char <- c("1", "a", NA)
  x_number <- c(1, 2, NA)
  x_matrix <- matrix(nrow = 2, ncol = 2, data = c(NA, 1, 2, 3))
  x_matrix_allb <- matrix(nrow = 3, ncol = 2, c(1,2,3,NA,NA,NA))
  x_list <- list(a = c(NA, 1), b = c(2, 3))
  x_df <- data.frame(a = c(NA, 1), b = c(2, 3))
  x_df_allb <- data.frame(a = c(1, 2), b = c(NA, NA))
  x_tibble <- dplyr::as_tibble(x_df)

  assert <- stopifnot
  assert(util.na_omit(x_char) == c("1", "a"))
  assert(util.na_omit(x_number) == c(1, 2))
  # I couldn't think of how to test that it throws an error for lists
  assert(util.na_omit(x_matrix) == matrix(nrow = 1, ncol = 2, data = c(1, 3)))
  # make sure a 2-d matrix has still been returned as well
  assert(dim(util.na_omit(x_matrix)) == c(1,2))
  # see what happens when matrix has an all-blank col
  assert(util.na_omit(x_matrix_allb) == matrix(ncol = 2, nrow = 0))
  assert(util.na_omit(x_df) == data.frame(a = 1, b = 3))
  assert(util.na_omit(x_tibble) == dplyr::tibble(a = 1, b = 3))
  # where there are all blank cols, the result should be a zero-row data.frame
  # that preserves column names and number of columns
  assert(nrow(util.na_omit(x_df_allb)) == 0)
  assert(names(util.na_omit(x_df_allb)) == names(x_df_allb))
}
util.na_omit_test()

###############################################################
###
###     String Cleaning
###     It's for updating strings.
###
###############################################################

util.strip_special_characters <- function(x){
    # only retain numbers, (latin) letters, and spaces
    letters_numbers_spaces_whitelist <- "[^0-9A-Za-z ]"
    gsub(letters_numbers_spaces_whitelist,"",x)
}


util.to_lowercase_and_numbers <- function(x){
    x %>%
        tolower() %>%
        util.strip_special_characters() %>%
        gsub(" ", "", .) # remove all spaces
}

# This is a unit test that should be re-written.
# if(! util.to_lowercase_and_numbers(" D*  i") %in% "di"){
#     stop("util.to_lowercase_and_numbers failed test.")
# }


util.trim <- function(x){
    # trim leading/trailing spaces and tabs
    gsub("(^[\t ]+)|([\t ]+$)", "", x)
}


util.strip_non_ascii <- function(x){
    # deletes non-ASCII characters, e.g., ø, ñ, etc.
    # e.g., Ekstrøm becomes Ekstrm
    iconv(x, "latin1", "ASCII", sub="")
}


###############################################################
###
###     Data Types
###     Checking and coercing data types.
###
###############################################################


util.is_blank <- function(x){
    # true if tab, space, empty space, NA, NaN
    is.na(x) | grepl("^[ \t]*$", x)
}

util.is_present <- function(x){
    ! util.is_blank(x)
}

util.to_character <- function(x){
    # like base::as.character()
    # but accepts (and returns) data frames or vectors
    if("data.frame" %in% class(x)){
        util.apply_columns(x, as.character)
    }
    else{ as.character(x) }
}


util.to_ascii <- function(x){
    if("data.frame" %in% class(x)){
        util.apply_columns(x, util.strip_non_ascii)
    }
    else{util.strip_non_ascii(x) }
}

util.is_vector_of_numbers <- function(x){
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
    originally_blank <- util.is_blank(x)

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


util.is_vector_of_numbers_test <- function(){
    x_char <- c("1", "a", "b", "1.1", "1e05", "a1", "1a")
    x_number <- c("1", "1.1", "2", "-1.1",  ".1", "0.1", "00.1", "1.00", "1.1", "01")
    x_scientific <- c("1e05")
    x_with_blanks <- c(x_number, "", NA)
    x_logical <- c(TRUE, FALSE)

    assert <- stopifnot
    assert(!util.is_vector_of_numbers(x_char))
    assert(util.is_vector_of_numbers(x_number))
    assert(util.is_vector_of_numbers(x_scientific))
    assert(util.is_vector_of_numbers(x_with_blanks))
    assert(!util.is_vector_of_numbers(x_logical))
}

util.is_vector_of_numbers_test()

util.as_numeric_if_number <- function(x){
    # run as.numeric if the x is made up of numbers
    # runs independently on each column if x is data.frame
    if("data.frame" %in% class(x)){
        return(util.apply_columns(x, util.as_numeric_if_number))
    }
    if(util.is_vector_of_numbers(x)){
        x <- as.numeric(x)
    }
    return(x)
}

###############################################################
###
###     Messages
###     Prettier messages that print to console or HTML.
###
###############################################################

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

###############################################################
###
###     Table Printing
###     Print tables to HTML (or console) reasonably.
###
###############################################################


util.html_table_from_model <- function(model, ...){
    accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
    if( ! any(class(model) %in% accepted_models ) ){
        util.warn("Unaccepted model supplied!")
    }
    if( ! interactive() ){
        stargazer(
            model,
            type="html",
            star.cutoffs = c(.05, .01, .001),
            notes        = "",
            notes.label = "1 star p<.05; 2 stars p<.01; 3 stars p<.001",
            notes.append = FALSE,
            single.row=TRUE,
            ...
        )
    }
    else{
        util.print_pre(summary(model))
    }
}

util.html_table_data_frame <- function(x, ...){
    # "grouped_df", "tbl_df" are dplyr type data.frames
    # ungroup to make them printable like a data.frame
    if(any(class(x) %in% c("grouped_df", "tbl_df"))){
        x <- data.frame(ungroup(x))
    }

    if( ! interactive() ){
        print(xtable(x),
              type="html",
              include.rownames = FALSE,
              html.table.attributes =
                  getOption("xtable.html.table.attributes",
                            "border=0, class='xtable'"),
              ...
        )
    }else{
        print(x)
    }
}

util.html_table_psych_alphas <- function(x, ...){
    # psych::alpha object, turn key data into data.frame
    if(! all(class(x) %in% c("psych","alpha"))){
        util.warn("Not a psych::alpha object!")
    }
    # extract the alpha coefficients for printing
    x <- x$total
    util.html_table_data_frame(x, ...)
}

util.html_table <- function(x, ...) {
    # Print a variety of things to rendered output as a nice table.
    # Accepts extra keyword arguments which are passed to xtable or stargazer,
    # as appropriate.
    # To capture rendered output in non-interactive mode as a character vector:
    # * Nothing is required in the case of models, just assign the returned
    #   value, e.g. `output <- util.html_table(lm.D9)`
    # * Set `print.results = FALSE` in other cases, e.g.
    #   `output <- util.html_table(my_data_frame, print.results = FALSE)`
    #   `output <- util.html_table(psych::alpha(r9), print.results = FALSE)`
    accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
    accepted_psych_objects <- c("psych","alpha")
    accepted_dfs <- c("grouped_df", "tbl_df","data.frame","table","matrix")

    if( any(class(x) %in% accepted_models ) ){
        util.html_table_from_model(x, ...)
    } else if( all( class(x) %in% accepted_psych_objects ) ){
        util.html_table_psych_alphas(x, ...)
    } else if( any(class(x) %in% accepted_dfs ) ){
        util.html_table_data_frame(x, ...)
    }
}



###############################################################
###
###     Math
###     Functions that involve mathematical manipulation.
###
###############################################################



util.z_score <- function(x){
    # calculate the z-score of vector or for each vector in a data.frame
    if("data.frame" %in% class(x)){
        util.apply_columns(x,util.z_score)
    }
    else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
}

util.row_means <- function(x, na.rm = TRUE){
    # like base::rowMeans(x, na.rm=TRUE)
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowMeans(x, na.rm = na.rm))
}

util.row_sums <- function(x, na.rm = TRUE){
    # like base::rowSums(x, na.rm=TRUE)
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowSums(x, na.rm = na.rm))
}

util.round_df <- function(DF, digits=2){
    # round all numeric columns
    round_if_number <- function(x, digits=digits){
        if(util.is_vector_of_numbers(x)){
            x <- round(x, digits)
        }
        return(x)
    }

    util.apply_columns( DF,
                        round_if_number,
                        digits=digits)
}


util.ordinal <- function(vec){
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
###     Reshaping
###     Manipulating the structure of data.
###
###############################################################


# Data Binding
util.rbind_many <- function(dfs, keep="intersection"){
    # keep "intersection" or "union" of columns
    # what columns should remain in the r-bound df?
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
util.rbind_intersection <- function(dfs){
    util.rbind_many(dfs, keep="intersection")
}

# rbind list of dfs, keep union of column names
util.rbind_union <- function(dfs){
    util.rbind_many(dfs, keep="union")
}

# demo of util.rbind_many
# x <- data.frame( b=c(1,2,3), a=c(1,2,3), z=c(1,2,3) )
# y <- data.frame( b=c(4,5,6), a=c(4,5,6), d=c(4,5,6))
# z <- data.frame( b=c(7,8,9), a=c(7,8,9), e=c(7,8,9))
# dfs <- list(x,y,z)
# util.rbind_union(dfs)
# util.rbind_intersection(dfs)



###############################################################
###
###     Value Replacement
###     Smartly replacing data values with other values.
###
###############################################################


util.recode <- function(vector, originals, replacements){
    # replace appearances of "originals" with "replacements"
    if(length(originals) != length(replacements)){
        "Original and replacement should have equal length" %>%
            util.warn()
    }
    if(any(duplicated(originals))){
        "Originals should not include duplicate values" %>%
            util.warn()
    }

    new_vec <- vector
    for(v in originals){
        new_vec[vector == v] <- replacements[originals == v]
    }
    new_vec
}

gymnast_test__util.recode <- function(){
    # basic test
    test_name <- "basic replacement"
    vector <- c("a","b","fefe","c")
    x <- c("a","b","c")
    y <- c("A","B","C")
    expectation <- c("A","B","fefe","C")
    if( !identical(expectation,util.recode(vector,x,y)) ){
        util.warn("recode fails: " %+% test_name)
    }

    test_name <- "multiple replacement includes originals"
    vector <- c("a","b","fefe","c")
    x <- c("a","b","c")
    y <- c("b","c","d")
    expectation <- c("b","c","fefe","d")
    if( !identical(expectation,util.recode(vector,x,y))){
        util.warn("recode fails: " %+% test_name)
    }
}

util.reverse_likert <- function(v, scale_levels) {
    # Require that responses are numeric so that we can do arithmetic
    reversed_data <- as.numeric(v)
    # Change 6's to 1's and 2's to 5's, etc.
    return(scale_levels - reversed_data + 1)
}

###############################################################
###
###     De-identification
###     Hash and de-identify variables and data.frames
###
###############################################################

util.hash_vector <- function(x, salt = NULL){
    # Hashes vector x with SHA-256, after optionally pasting salt

    # if salt is defined, then salt
    if(!is.null(salt)){
        x <- paste0(x, salt)
    }
    # hash
    lapply(x, function(x) digest(x, algo="sha256")) %>% unlist
}

###############################################################
###
###     Reading and writing files
###     Read/write files efficiently
###
###############################################################

util.read_csv_files <- function(path_list, ...) {
    # reads a list of .csv file paths and returns a list of data.frames
    # Args
    #   path_list: list that contains paths pointing to the desired .csv files
    #   ... optional arguments to be passed to read.csv (e.g., na.strings)
    #
    # Loops through the paths in path_list, reads them into R, and creates
    # a list of data.frames, e.g., for
    # path_list <- list("a" = "~Downloads/my_file.csv"), you would get
    # a one-element list named "a" containing the contents my_file.csv
    # as a data.frame

    found_files <- sapply(path_list, function(path) {
        length(path) > 0 && file.exists(path)
    })

    # Check for nonexistant files
    if(any(!found_files)){
        util.warn(
            "The following files were not found: " %+%
            paste0(
                names(found_files)[!found_files],
                collapse=", "
            )
        )
    } else{
        util.passed("All files present, successfully loaded " %+%
            length(found_files) %+%
            " files.")
    }
    # Read the new files into df_list()
    df_list <- list()
    for(file_name in names(path_list)){
      # only try to read in files that exist!
        if(file_name %in% names(found_files[found_files])){
            df_list[[file_name]] <- read.csv(
                path_list[[file_name]],
                stringsAsFactors=FALSE,
                ...
                )
        }
    }
    return(df_list)
}

util.assign_list_to_environment <- function(l, environment = .GlobalEnv){
    # assigns all elements of list l to environment
    #
    # Args
    #   l: list of objects to be assigned, with names(l) corresponding to desired
    # object names
    #   environment: environment you want the variables assigned to
    for(i in 1:length(l)){
        assign(names(l)[i], l[[i]], environment)
    }
}

util.list_all <- function (initial_path, max_depth = 2, type = 'all', current_depth = 0) {
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
            out <- c(out, util.list_all(
                d, max_depth = max_depth, type = type,
                current_depth = current_depth + 1))
        }
    }

    return(out)
}

util.list_files <- function (initial_path, ...) {
    # Lists only files, not directories. See util.list_all().
    util.list_all(initial_path, type = 'files', ...)
}

util.list_dirs <- function (initial_path, ...) {
    # Lists only directories, not files. See util.list_all().
    util.list_all(initial_path, type = 'dirs', ...)
}

util.find_crypt_paths <- function (files_to_load, initial_path = NA,
                                   volume_patterns = NA, max_depth = 2) {
    # Find the full paths of specified files within any mounted crypts.
    # Designed to work with util.read_csv_files().
    #
    # Example call:
    # > find_crypt_paths(list(school_a = 'School A/data.csv'))
    # $school_a
    # [1] "/Volumes/NO NAME 1/CC 10/School A/data.csv"
    #
    # Args:
    #   file_to_load: list, mapping of arbitrary labels to file names or
    #     partial file paths, as specific as necessary to find the file. If you
    #     give 'data.csv' and this function finds several of those within
    #     mounted crypts, it will stop. The solution is to be more specific,
    #     e.g. 'CC10-11/data.csv'
    #   initial_path: atomic char, default '/Volumes', the parent directory
    #     where crypt files are mounted (not applicable in Windows).
    #   volume_patterns: char, regexes that are expected to match volume
    #     names. Default matches volumes that start with "NO NAME" or
    #     "Untitled".
    #   max_depth: atomic int, default 2, how many subfolders deep to scan for
    #     files. Zero means enter no subfolders.
    #
    # Returns: List with provided labels to absolute file paths.

    # check the format of files_to_load
    if(!is.list(files_to_load)){
        stop("in util.find_crypt_paths, files_to_load must be a list.")
    }

    if(any(util.is_blank(names(files_to_load))) | is.null(names(files_to_load))){
        stop("in util.find_crypt_paths, all elements of the list files_to_load must be named (e.g., list(a = 'a',), not list('a')")
    }

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
        crypt_paths <- c(util.list_all(m, max_depth = max_depth), crypt_paths)
    }

    # For each file to load, scan the list of known files for a match.
    found_paths <- list()
    for (label in names(files_to_load)) {
        file_name <- files_to_load[[label]]

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
            found_paths[[label]] <- match
        }
    }

    return(found_paths)
}


###############################################################
###
###     Package Installation
###
###############################################################

gymnast_install <- function () {
    # Load (or install) all packages used by gymnast.
    dependencies <- c(
        "digest","dplyr","formatR","ggplot2",
        "grid","Hmisc","knitr","lme4",
        "lmerTest","psych","reshape2","scales" ,
        "stargazer","stringr","xtable"
    )

    ensure_packages(dependencies, prefer_type = "source")

    for(lib_name in dependencies){
        library(lib_name, character.only = TRUE)
    }
    # Do not uncomment without resolving issue #27!
    # resolve_name_conflicts()
  }

resolve_name_conflicts <- function () {
    # BAD (pending issue #27) b/c when sourcing gymnast namespace doesn't
    # exist!

    # When gymnast is installed as a package, rather than loaded as code from
    # github, some function names can conflict. Enforce our preferences here.

    util.assign_list_to_environment(list(
        # Use our string concatenation rather than the psych packages matrix
        # addition.
        # http://personality-project.org/r/psych/psych-manual.pdf
        # What the heck is a triple colon?
        # http://stackoverflow.com/questions/2165342/r-calling-a-function-from-a-namespace
        "%+%" = gymnast:::`%+%`
    ))
}

to_type_tbldf_test <- function(){
  test_df <- data.frame(a = c(1, 2), b = c('c','d'))
  test_tbldf <- test_df %>% group_by(a)
  test_vec <- c(1, 2)

  assert <- stopifnot

  # all the util.to_ functions should return dfs of the same dimensions as the originals,
  # for both dfs and tbl_df types (i.e., data.frame-like objects returned by dplyr operations)
  assert(identical(dim(test_df), dim(util.to_character(test_df))))
  assert(identical(dim(test_tbldf), dim(util.to_character(test_tbldf))))
  assert(identical(length(test_vec), length(util.to_character(test_vec))))

  assert(identical(dim(test_df), dim(util.to_ascii(test_df))))
  assert(identical(dim(test_tbldf), dim(util.to_ascii(test_tbldf))))
  assert(identical(length(test_vec), length(util.to_ascii(test_vec))))

  assert(identical(dim(test_df), dim(util.as_numeric_if_number(test_df))))
  assert(identical(dim(test_tbldf), dim(util.as_numeric_if_number(test_tbldf))))
  assert(identical(length(test_vec), length(util.as_numeric_if_number(test_vec))))
}

# to_type_tbldf_test()