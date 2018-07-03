###############################################################
###
###     Reading and writing files
###     Read/write files efficiently
###
###############################################################

if (!"%+%" %in% ls()) {
  source('operators.R', local = TRUE)
}

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

    if(any(is.null(names(files_to_load)))) {
        stop(
          "In util.find_crypt_paths, all elements of the list " %+%
          "files_to_load must be named, e.g. list(a = 'a',), not list('a')."
        )
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
