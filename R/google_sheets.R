# functions for interacting with google sheets (write and read)
google_api <- import_module("google_api")
regex <- import_module("regex")
logging <- import_module("logging")


handle_response <- function(response, method_name) {
  content <- httr::content(response)

  if (response$status_code >= 300) {
    stop(paste0(
      "Error: non-successful response in ",
      method_name,
      "(): ",
      response$status_code,
      content
    ))
  }

  return(content)
}

parse_range_begin <- function(range_begin) {
  if (grepl(":", range_begin)) {
    stop("range_begin must not be a range, i.e. must not contain ':'.")
  }

  # Sort out what part of the range syntax is a tab/sheet, a column (which
  # will need letter converted to number), and a row. Including a tab/sheet
  # is optional.

  tab <- NA
  begin_cell <- toupper(range_begin)
  matches <- regex$extract_groups("^(.*?)!(.*)$", range_begin)
  if (length(matches) > 0) {
    tab <- matches[[1]][1]
    begin_cell <- toupper(matches[[1]][2])
  }

  matches <- regex$extract_groups("^([A-Z]+)([0-9]+)$", begin_cell)
  begin_col <- cellranger::letter_to_num(matches[[1]][1])
  begin_row <- as.numeric(matches[[1]][2])

  list(
    tab = tab,
    cell = begin_cell,
    col = begin_col,
    row = begin_row,
    sheet_prefix = ifelse(is.na(tab), "", paste0(tab, "!"))
  )
}

data_range_with_header <- function(df, range_begin) {
  # Returns a range with the specified upper left corner with nrow(df) + 1 rows,
  # where the extra is for the column headers.
  begin <- parse_range_begin(range_begin)
  end_col_a1 <- cellranger::num_to_letter(begin$col + ncol(df) - 1)
  end_row <- begin$row + nrow(df) # fenceposts: gives nrow(df) + 1 rows
  range_a1 <- paste0(begin$sheet_prefix, begin$cell, ":", end_col_a1, end_row)

  range_a1
}

data_range_raw <- function(df, range_begin) {
  # Returns a range with the specified upper left corner with nrow(df) rows.
  begin <- parse_range_begin(range_begin)
  end_col_a1 <- cellranger::num_to_letter(begin$col + ncol(df) - 1)
  end_row <- begin$row + nrow(df) - 1 # fenceposts: gives nrow(df) rows
  range_a1 <- paste0(begin$sheet_prefix, begin$cell, ":", end_col_a1, end_row)

  range_a1
}

data_range_skip_header <- function(df, range_begin) {
  # Returns a range _one row lower_ than the specified upper left corner with
  # nrow(df) rows.
  begin <- parse_range_begin(range_begin)

  begin$row <- begin$row + 1
  begin$cell <- paste0(cellranger::num_to_letter(begin$col), begin$row)

  end_col_a1 <- cellranger::num_to_letter(begin$col + ncol(df) - 1)
  end_row <- begin$row + nrow(df) - 1 # fenceposts: gives nrow(df) rows
  range_a1 <- paste0(begin$sheet_prefix, begin$cell, ":", end_col_a1, end_row)

  range_a1
}

open <- function(credentials, credential_type = "client_id", token = NULL) {
  # Authenticate with several Google APIs, using OAuth 2.0, capturing at token
  # in the closure.
  #
  # Args:
  #   credentials - list, from Google (parsed JSON)
  #   credential_type - character, length 1, one of:
  #     'client_id' default, for interactive/console use by analysts
  #     'service_account_key' for automated code like RServe
  #
  # See google_api$get_token().
  #
  # Example use:
  #
  # gs <- import_module("google_sheets")
  # gs_conn <- gs$open(credentials)
  # df <- gs_conn$read(my_sheet_id, range_a1 = "Sheet1")
  # gs_conn$append(my_sheet_id, df2)
  # (no need to close the "connection")

  scopes <- c(
    "https://www.googleapis.com/auth/drive",
    "https://www.googleapis.com/auth/drive.file",
    "https://www.googleapis.com/auth/spreadsheets"
  )
  if (is.null(token)) {
    token <- google_api$get_token(credentials, credential_type, scopes)
  }

  sheets_api_url <- "https://sheets.googleapis.com/v4/spreadsheets/"
  drive_api_url <- "https://www.googleapis.com/drive/v3/files/"

  create <- function(sheet_title = "Made By google_sheets.R") {
    response <- httr::POST(
      url = sheets_api_url,
      httr::config(token = token),
      encode = "json",
      body = list(
        properties = list(
          title = sheet_title
        )
      )
    )
    content <- handle_response(response, "create")
    return(content$spreadsheetId)
  }

  share <- function(sheet_id, email_address, role = "writer") {
    # Share a sheet (or any Google Drive file) with some other account. Likely
    # values for `role` are 'fileOrganizer', 'writer', or 'commenter', see
    # https://developers.google.com/drive/api/v3/reference/permissions#resource

    url <- httr::modify_url(
      paste0(drive_api_url, sheet_id, "/permissions"),
      query = list(sendNotificationEmail = "false")
    )

    response <- httr::POST(
      url = url,
      httr::config(token = token),
      encode = "json",
      body = list(
        role = role,
        type = "user",
        emailAddress = email_address
      )
    )

    content <- handle_response(response, "share")

    return(content)
  }

  list_collaborators <- function(sheet_id) {
    # Returns list of accounts associated with the sheet (or any Google Drive
    # file).
    url <- paste0(drive_api_url, sheet_id, "/permissions")
    response <- httr::GET(url = url, httr::config(token = token))

    content <- handle_response(response, "list_collaborators")

    return(content)
  }

  list_sheets <- function() {
    # Returns vector of sheet ids, with sheets names as element names.
    url <- httr::modify_url(
      drive_api_url,
      query = list(
        q = "mimeType = 'application/vnd.google-apps.spreadsheet'",
        orderBy = "name"
      )
    )

    response <- httr::GET(
      url = url,
      httr::config(token = token)
    )

    content <- handle_response(response, "list_sheets")

    sheet_ids <- sapply(content$files, function(f) f$id)
    names(sheet_ids) <- sapply(content$files, function(f) f$name)

    return(sheet_ids)
  }

  get_tabs <- function(sheet_id) {
    url <- httr::modify_url(
      paste0("https://sheets.googleapis.com/v4/spreadsheets/", sheet_id),
      query = list(fields = "sheets.properties")
    )

    # https://developers.google.com/sheets/api/samples/sheet
    response <- httr::GET(
      url,
      httr::config(token = token)
    )

    content <- handle_response(response, "get_tabs")

    return(content$sheets)
  }


  get_tab_id <- function(sheet_id, tab_title) {
    tabs <- get_tabs(sheet_id)
    tab_id <- NULL
    for (tab in tabs) {
      if (tab$properties$title == tab_title) {
        tab_id <- tab$properties$sheetId
      }
    }
    return(tab_id)
  }

  add_tab <- function(sheet_id, title) {
    tab_id <- get_tab_id(sheet_id, title)
    if (!is.null(tab_id)) {
      # Don't add if a tab with the same name already exists.
      return()
    }

    url <- paste0(
      "https://sheets.googleapis.com/v4/spreadsheets/",
      sheet_id,
      ":batchUpdate"
    )

    # https://developers.google.com/sheets/api/samples/sheet
    response <- httr::POST(
      url,
      httr::config(token = token),
      encode = "json",
      body = list(
        requests = list(
          list(
            addSheet = list(
              properties = list(
                title = title
              )
            )
          )
        )
      )
    )

    content <- handle_response(response, "add_tab")

    return(content)
  }

  clear_tab <- function(sheet_id, title) {
    # Preserves formatting in tab.
    tab_id <- get_tab_id(sheet_id, title)
    if (is.null(tab_id)) {
      msg <- paste0("Tab ", title, " not found!")
      logging$warning(msg)
      return(msg)
    }

    url <- paste0(
      "https://sheets.googleapis.com/v4/spreadsheets/",
      sheet_id,
      ":batchUpdate"
    )
    # https://developers.google.com/sheets/api/samples/sheet#clear_a_sheet_of_all_values_while_preserving_formats
    response <- httr::POST(
      url,
      httr::config(token = token),
      encode = "json",
      body = list(
        requests = list(
          list(
            updateCells = list(
              range = list(
                sheetId = tab_id
              ),
              fields = "userEnteredValue"
            )
          )
        )
      )
    )

    content <- handle_response(response, "clear_tab")

    return(content)
  }

  clear_range <- function(sheet_id, title, range_a1) {
    # clears the specified range in a tab
    tab_id <- get_tab_id(sheet_id, title)
    if (is.null(tab_id)) {
      msg <- paste0("Tab ", title, " not found!")
      logging$warning(msg)
      return(msg)
    }
    url <- paste0(
      "https://sheets.googleapis.com/v4/spreadsheets/",
      sheet_id,
      "/values/",
      paste0(title, "!", range_a1),
      ":clear"
    )
    # https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/clear
    response <- httr::POST(
      url,
      httr::config(token = token),
      encode = "json",
      body = list(
        requests = list()
      )
    )

    content <- handle_response(response, "clear_range")

    return(content)
  }

  overwrite <- function(sheet_id,
                        df,
                        range_begin = "A1",
                        include_column_labels = TRUE) {
    # Write data to a sheet, without any regard for what is currently there.

    # We need to write the column labels and data columns separately so we can
    # respect types. If we try, for instance, to write a column of integers
    # with its label all at once, R will coerce that to a character vector, and
    # the sheet will save cell values as text like `'1`, which you can't do
    # sheet formulae with.
    if (include_column_labels) {
      column_label_range <- range_begin
      data_range <- data_range_skip_header(df, range_begin)
    } else {
      column_label_range <- NULL
      data_range <- data_range_raw(df, range_begin)
    }

    if (!is.null(column_label_range)) {
      column_label_url <- httr::modify_url(
        paste0(sheets_api_url, sheet_id, "/values/", column_label_range),
        query = list(
          valueInputOption = "RAW",
          includeValuesInResponse = "false"
        )
      )
      response <- httr::PUT(
        url = column_label_url,
        httr::config(token = token),
        encode = "json",
        body = list(
          range = column_label_range,
          majorDimension = "ROWS",
          values = list(names(df))
        )
      )
      handle_response(response, "overwrite:column-labels")
    }

    data_url <- httr::modify_url(
      paste0(sheets_api_url, sheet_id, "/values/", data_range),
      query = list(
        valueInputOption = "RAW",
        includeValuesInResponse = "false"
      )
    )
    response <- httr::PUT(
      url = data_url,
      httr::config(token = token),
      encode = "json",
      body = list(
        range = data_range,
        majorDimension = "COLUMNS",
        values = unname(as.list(df))
      )
    )

    handle_response(response, "overwrite")
  }

  append <- function(sheet_id, df, range_a1 = "A1") {
    # Add rows to a table of data in a sheet. The default range, 'A1', searches
    # all of the default sheet for what looks like the last table.
    # See https://developers.google.com/sheets/api/guides/values#appending_values
    #
    # If you want to target a different tab within the spreadsheet, defined
    # range to something like "Sheet2!A1"
    #
    # Not tested with rows whose width do not match the existing data.

    values <- as.list(df)
    names(values) <- NULL

    url <- httr::modify_url(
      paste0(sheets_api_url, sheet_id, "/values/", range_a1, ":append"),
      query = list(
        valueInputOption = "RAW",
        includeValuesInResponse = "false"
      )
    )
    result <- httr::POST(
      url = url,
      httr::config(token = token),
      encode = "json",
      body = list(
        range = range_a1,
        majorDimension = "COLUMNS",
        values = lapply(values, I) # don't unbox values if df has only 1 row
      )
    )

    content <- handle_response(result, "append")

    return(content)
  }

  read <- function(sheet_id, range_a1, major_dimension = "COLUMNS") {
    # Note that `range` can be an entire sheet, like "Sheet1"
    # Returns a data frame with all columns as characters.
    url <- httr::modify_url(
      paste0(sheets_api_url, sheet_id, "/values/", range_a1),
      query = list(majorDimension = major_dimension)
    )
    response <- httr::GET(url = url, httr::config(token = token))

    handle_response(response, "read")

    content <- jsonlite::fromJSON(httr::content(response, "text"))

    raw_values <- content$values
    col_names <- as.data.frame(raw_values)[["V1"]]
    # Watch out for data.frame() converting characters to factors!
    result_df <- as.data.frame(
      t(raw_values),
      row.names = as.character(0:length(raw_values)),
      optional = TRUE,
      stringsAsFactors = FALSE
    )[-1, ]

    names(result_df) <- col_names

    return(result_df)
  }

  delete <- function(sheet_id) {
    # Delete a sheet (or any Google Drive file). Takes action immediately,
    # no take-backs.
    response <- httr::DELETE(
      url = paste0(drive_api_url, sheet_id),
      httr::config(token = token)
    )

    handle_response(response, "delete")
  }

  format_simple_header_row <- function(sheet_id, tab_title) {
    tab_id <- get_tab_id(sheet_id, tab_title)
    if (is.null(tab_id)) {
      msg <- paste0("Tab ", tab_title, " not found!")
      logging$warning(msg)
      return(msg)
    }
    url <- paste0(
      "https://sheets.googleapis.com/v4/spreadsheets/",
      sheet_id,
      ":batchUpdate"
    )
    # https://developers.google.com/sheets/api/samples/formatting#format_a_header_row
    response <- httr::POST(
      url,
      httr::config(token = token),
      encode = "json",
      body = list(
        requests = list(
          list(
            repeatCell = list(
              range = list(
                sheetId = tab_id,
                startRowIndex = 0,
                endRowIndex = 1
              ),
              cell = list(
                userEnteredFormat = list(
                  textFormat = list(
                    fontSize = 11,
                    bold = TRUE
                  ),
                  wrapStrategy = 'WRAP'
                )
              ),
              fields = "userEnteredFormat(textFormat,wrapStrategy)"
            )
          ),
          list(
            updateSheetProperties = list(
              properties = list(
                sheetId = tab_id,
                gridProperties = list(
                  frozenRowCount = 1
                )
              ),
              fields = "gridProperties.frozenRowCount"
            )
          )
        )
      )
    )

    content <- handle_response(response, "format_simple_header_row")

    return(content)
  }

  set_column_format <- function(sheet_id, tab_title, column_ordinal, type) {
    # Supported types are: TEXT, NUMBER, PERCENT, CURRENCY, DATE, TIME,
    # SCIENTIFIC.
    # https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/cells#numberformattype
    tab_id <- get_tab_id(sheet_id, tab_title)
    if (is.null(tab_id)) {
      msg <- paste0("Tab ", tab_title, " not found!")
      logging$warning(msg)
      return(msg)
    }
    url <- paste0(
      "https://sheets.googleapis.com/v4/spreadsheets/",
      sheet_id,
      ":batchUpdate"
    )

    if (type %in% 'PERCENT') {
      pattern <- '#0%'
    } else {
      pattern <- NULL
    }
    response <- httr::POST(
      url,
      httr::config(token = token),
      encode = "json",
      body = list(
        requests = list(
          list(
            repeatCell = list(
              range = list(
                sheetId = tab_id,
                startRowIndex = 1,
                startColumnIndex = column_ordinal - 1,
                endColumnIndex = column_ordinal
              ),
              cell = list(
                userEnteredFormat = list(
                  numberFormat = list(
                    type = type,
                    pattern = pattern
                  )
                )
              ),
              fields = "userEnteredFormat.numberFormat"
            )
          )
        )
      )
    )

    content <- handle_response(response, "set_column_format")

    return(content)
  }

  return(list(
    add_tab = add_tab,
    append = append,
    clear_range = clear_range,
    clear_tab = clear_tab,
    create = create,
    delete = delete,
    format_simple_header_row = format_simple_header_row,
    get_tabs = get_tabs,
    list_collaborators = list_collaborators,
    list_sheets = list_sheets,
    overwrite = overwrite,
    read = read,
    set_column_format = set_column_format,
    share = share,
    token = token
  ))
}
