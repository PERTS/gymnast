# big_pipe.R
#
# Process/download JSON exports from various PERTS Platforms.
#
# Uses data from your working directory by default. To download fresh data, get
# a copy of the credentials for your project from the password crypt, then
# call:
#
# tables <- big_pipe(project_id, project_credentials)

# packages: jsonlite, httr, dplyr

google_api <- import_module('google_api')

modules::import("dplyr", `%>%`)

`%+%` <- paste0

drop_unused_columns <- function (d) {
  unused <- c(
    'assc_school_list',
    'auth_id',
    'deleted',
    'is_archived',
    'is_test'
  )
  d[!names(d) %in% unused]
}

drop_unsafe_columns <- function (d) {
  unsafe <- c(
    'hashed_password'
  )
  d[!names(d) %in% unsafe]
}

drop_internal_columns <- function (d) {
  # Anything with a double underscore is App Engine internal.
  internal <- grep('^__', names(d), value = TRUE)
  d[!names(d) %in% internal]
}


# @todo: revoke example client credentials for mariposa
big_pipe <- function(project_id, project_credentials, download_pd = FALSE,
                     credential_type = 'client_id', tables = 'all') {
    # Args:
    #   project_id - character, length 1, Google Cloud project id, e.g.
    #     'neptuneplatform'
    #   project_credentials - list, R-representation of what Google provides,
    #     with extra fields 'buckets' listing which GCS buckets to look in.
    #   credential_type - character, length 1, one of:
    #     'client_id' default, for interactive/console use by analysts
    #     'service_account_key' for automated code like RServe
    #   tables - character, default 'all', names of the tables you'd like to
    #     download, in StandingCamel, e.g. c('Organization', 'ProjectCohort')
    #   download_pd - boolean, length 1, default FALSE, whether to download the
    #     very large Pd table on Yosemite-like projects.

    # Project credentials have the following form, and should never be stored
    # in a git repository so they remain secure.
    # project_credentials = list(
    #     buckets = c('mariposa-backup-daily-1', 'mariposa-backup-daily-2'),
    #     client_id = '1009908406552-bgfrlkl1g2t9q9encl8ofji0rt76u71l.apps.googleusercontent.com',
    #     client_secret = '-2bV-0G2MJt5Rt_2DGAgOj1J'
    # )

    bucket_url <- function(bucket) {
        paste0("https://www.googleapis.com/storage/v1/b/", bucket, "/o")
    }

    file_url <- function(bucket, file_name) {
        paste0(bucket_url(bucket), "/", file_name)
    }

    list_bucket <- function(bucket, token) {
        response <- httr::GET(bucket_url(bucket), httr::config(token = token))
        httr::stop_for_status(response)
        return(httr::content(response))
    }

    choose_newest_json_items <- function(contents_list, token) {
        newest_bucket <- NA
        newest_updated <- NA
        newest_items <- NA
        for (contents in contents_list) {
            # Filter items in the bucket for .json files. Need to do a little
            # wrangling to get just the file names out of the object meta data.
            item_names <- sapply(contents$items, function(i) i$name)

            # The Pd table is the slowest to download and often the least
            # useful. Provide an option for it to be skipped.
            if (download_pd) {
                regex <- ".json"
            } else {
                regex <- "^(?!Pd).+?\\.json"
            }
            json_items <- contents$items[grepl(regex, item_names, perl = TRUE)]

            if (length(json_items) > 0) {
                first_item <- json_items[[1]]
                updated <- as.Date(first_item$updated)
                if (is.na(newest_updated) || updated > newest_updated) {
                    newest_bucket <- first_item$bucket
                    newest_updated <- updated
                    newest_items <- json_items
                }
            }
        }
        if (is.na(newest_updated)) {
            stop("No json data files found.")
        } else {
            print(paste0("Choosing newest bucket: ", newest_bucket))
        }
        print(paste0("Preparing to download backup from ", newest_updated))
        return(newest_items)
    }

    get_gcs_file_contents <- function(file_item, token) {
        response <- httr::GET(file_item$mediaLink, httr::config(token = token))
        httr::stop_for_status(response)
        # Important to process the response as text, otherwise you get raw
        # bytes.
        # https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
        return(httr::content(response, "text"))
    }

    fix_gcs_json_str <- function(bad_json_str) {
        # The content of json files on GCS isn't really json. It's newline
        # delimited json objects. Do some quick find-and-replace to make it a
        # list of objects.
        return(paste0("[", gsub("\\}\n\\{", "},\n{", bad_json_str), "]"))
    }

    write_local_file <- function(file_name, content_str) {
        file_handle <- file(file_name, open = 'w', encoding = 'UTF-8')
        writeLines(content_str, file_handle)
        close(file_handle)
    }

    get_kind_from_table <- function(df) {
        # Determine the type of file it is from the key kind.
        # df[['__key__']] is a dataframe within a data frame.
        return(df[['__key__']]$kind[1])
    }

    table_list <- list()

    print("Downloading data from BigPipe exports in GCS...")

    # Try to load from GCS. May be slow.
    scopes <- 'https://www.googleapis.com/auth/devstorage.read_only'
    token <- google_api$get_token(project_credentials, credential_type, scopes)
    contents_list <- lapply(project_credentials$buckets,
                            function(b) list_bucket(b, token))
    json_items <- choose_newest_json_items(contents_list, token)

    if (tables != 'all') {
      json_items <- Filter(
        function (i) i$name %in% paste0(tables, '.json'),
        json_items
      )
    }

    print(paste0("Ready to download ", length(json_items), " files."))
    print(sapply(json_items, function(i) i$name))

    for (item in json_items) {
        bad_json_str <- get_gcs_file_contents(item, token)
        json_str <- fix_gcs_json_str(bad_json_str)
        raw_df <- jsonlite::fromJSON(json_str)

        table_list[[get_kind_from_table(raw_df)]] <- raw_df %>%
          drop_internal_columns() %>%
          drop_unsafe_columns()
    }

    print(paste0("Done loading files for ", project_id))
    print("Found tables:")
    print(names(table_list))

    return(table_list);
}

# The library httr saves oauth tokens to a hidden file, which doesn't clear
# or reset even if you clear your R workspace. So if the authentication token
# you've got is invalid for some reason, you'll be stuck with a
# 401 Unauthorized or similar error with no apparent fix.
#
# This function is the solution. Call it and try big pipe again.
# See http://stackoverflow.com/questions/27501775/retrieving-cached-oauth-token-with-packages-httr-twitter-and-streamr
big_pipe.reset <- function() { saveRDS(NULL, '.httr-oauth') }


get_column_names_by_type <- function(d, types) {
  column_types <- sapply(d, class)
  names(d)[column_types %in% types]
}

remove_obvious_garbage <- function(d) {
  d[d$is_archived == FALSE & d$is_test == FALSE & d$deleted == FALSE, ]
}



rename_verbose_columns <- function(d) {
  columns_to_rename <- names(d)[grepl("_list$", names(d))]
  for (name in columns_to_rename) {
    new_name <- substring(name, 0, nchar(name) - 5)  # strip ending "_list"
    d[[new_name]] <- d[[name]]
  }
  d[!names(d) %in% columns_to_rename]
}

fix_nested_data_frames <- function(d) {
  #   BigQuery has been screwing with me and changing how Email and Text
  # types are imported by changing them into "Record" types. We need to un-
  # screw any such columns.
  #   This is relatively easy fix because a Record column is itself a data
  # frame. We just overwrite it with one of the nested data frame's columns
  # to get a simple character vector.

  # Document known record columns, their column name, and their nested column
  # that contains the data we want.
  known_df_cols <- list(
    login_email = 'email',
    aggregation_json = 'text'
  )
  # Identify columns that are actually data frames.
  df_cols <- get_column_names_by_type(d, "data.frame")
  # Try to fix them all, but give a warning if a new, unknown type shows up.
  for (col in df_cols) {
    if (col %in% names(known_df_cols)) {
      nested_col <- known_df_cols[[col]]
      d[[col]] <- d[[col]][[nested_col]]
    } else {
      stop("Column " %+% col %+% " is a nested data frame, but isn't " %+%
           "known to BigPipe so it cannot be simplified. Please choose " %+%
           "the sub-column to keep and add it to `known_df_cols` in " %+%
           "big_pipe.R")
    }
  }

  return(d)
}

reduce_to_first_element <- function(list_column) {
  sapply(list_column, function(x) ifelse(length(x) > 0, x[[1]], NA))
}

fix_nested_lists <- function(d) {
  # Some of the columns have elements which are simple flat lists.
  # These we want to reduce to the first element.

  # Find all the columns that are list objects.
  list_cols <- get_column_names_by_type(d, "list")

  for (col in list_cols) {
    d[[col]] <- reduce_to_first_element(d[[col]])
  }

  return(d)
}

clean_platform_table <- function(nested_df) {
  # Function to clean a platform table, agnostic as to kind/type of table (e.g.
  # User vs. Classroom) and to version of the platform (e.g. Yellowstone vs.
  # Mariposa). Flattens ALL repeated values, like association lists, assuming
  # that such information is only relevant for permissions, e.g. which cohorts
  # a coordinator may see on the dashboard, and have no bearing on analysis.
  # Every other case is a simple, single-valued parent-child association, which
  # we perfer to store as a primitive value rather than a list.


  nested_df %>%
    remove_obvious_garbage() %>%
    drop_unsafe_columns() %>%
    drop_unused_columns() %>%
    drop_internal_columns() %>%
    rename_verbose_columns() %>%
    fix_nested_data_frames() %>%
    fix_nested_lists()
}

