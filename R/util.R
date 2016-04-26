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


util.strip_non_acsii <- function(x){
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


util.to_character <- function(x){
    # like base::as.character()
    # but accepts (and returns) data frames or vectors
    if(class(x) %in% "data.frame"){
        util.apply_columns(x, as.character)
    }
    else{ as.character(x) }
}


util.to_acsii <- function(x){
    if(class(x) %in% "data.frame"){
        util.apply_columns(x, util.strip_non_acsii)
    }
    else{ as.util.strip_non_acsii(x) }
}

util.is_vector_of_numbers <- function(x){
    # Are all elements of x numbers?
    # (regardless of whether x is numeric)
    # Character vectors sometimes need to be changed to numerics,
    # e.g., when all columns set to character types by default.

    # find numeric values
    numeric_values <- grepl("^-*[[:digit:]]*\\.*[[:digit:]]+$",x)

    # find the blank values
    blank_values <- util.is_blank(x)

    # Return TRUE if all values are either numeric or blank
    # Return FALSE otherwise
    if(all(numeric_values | blank_values)){
        return(TRUE)
    }
    else{
        return(FALSE)
    }
}

util.as_numeric_if_number <- function(x){
    # run as.numeric if the x is made up of numbers
    # runs independently on each column if x is data.frame
    if(class(x) %in% "data.frame"){
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
###     Prettier messages that print to consolte or HTML.
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


util.html_table_from_model <- function(model){
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
            single.row=TRUE
        )
    }
    else{
        util.print_pre(summary(model))
    }
}

util.html_table_data_frame <- function(x){
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
                            "border=0, class='xtable'")
        )
    }else{
        print(x)
    }
}

util.html_table_psych_alphas <- function(x){
    # psych::alpha object, turn key data into data.frame
    if(! all(class(x) %in% c("psych","alpha"))){
        util.warn("Not a psych::alpha object!")
    }
    # extract the alpha coefficients for printing
    x <- x$total
    util.html_table_data_frame(x)
}

util.html_table <- function(x, ...) {
    accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
    accepted_psych_objects <- c("psych","alpha")
    accepted_dfs <- c("grouped_df", "tbl_df","data.frame","table")

    if( any(class(x) %in% accepted_models ) ){
        util.html_table_from_model(x)
    } else if( all( class(x) %in% accepted_psych_objects ) ){
        util.html_table_psych_alphas(x)
    } else if( any(class(x) %in% accepted_dfs ) ){
        util.html_table_data_frame(x)
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
    if(class(x) %in% "data.frame"){
        util.apply_columns(x,util.z_score)
    }
    else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
}

util.row_means <- function(x){
    # like base::rowMeans(x, na.rm=TRUE)
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowMeans(x, na.rm = TRUE))
}

util.row_sums <- function(x){
    # like base::rowSums(x, na.rm=TRUE)
    # but it returns self if vector instead of issuing an error
    if(is.vector(x)) return(x)
    return(rowSums(x, na.rm = TRUE))
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

util.read_csv_files <- function(path_list, environment = .GlobalEnv, ...){
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

    found_files <- sapply(path_list, file.exists)

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
        crypt_paths <- c(util.list_all(m), crypt_paths)
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
    for(lib_name in dependencies){
        message(paste0("Loading... ",lib_name))
        # Require returns FALSE if packages failed
        if( !require(lib_name, character.only = TRUE) ){
            install.packages(lib_name)
            # library raises error if installation failed
            library(lib_name, character.only = TRUE)
        }
    }
}

gymnast_install()
