# packages: DBI, RMySQL

util <- import_module('util')

connect <- function (server_ip, dbname = NA, ssl_file_names = list(),
                     ssl_credentials = list(), password = NULL,
                     mysql_user = "readonly") {
  # Get a connection to a MySQL database.
  #
  # Args:
  #   server_ip - atomic char, e.g. 35.12.78.103
  #   dbname - atomic char, optional, same as running `USE mydatabase;`
  #   ssl_file_names - list with names c("ca", "cert", "key") each with file
  #     names of the appropriate ssl keys and certificates. These files will
  #     be looked up in mounted crypt files. Mutually exclusive with
  #     ssl_credentials.
  #   ssl_credentials - list with names c("ca", "cert", "key") each with a
  #     string of the appropriate ssl keys and certificates. Mutually exclusive
  #     with ssl_file_names.
  #   password - scalar character, optional password to use
  #   mysql_user - scalar character, default "readonly", name of mysql user
  #     account to use

  CNF_PATH <- paste0(getwd(), "/sql_connect.tmp.cnf")
  SERVER_PORT <- 3306

  # These will be deleted as soon as they're not needed.
  temporary_file_paths <- CNF_PATH

  ssl_paths <- NULL
  if (length(ssl_file_names) > 0) {
    ssl_paths <- util$find_crypt_paths(ssl_file_names, max_depth=4)
  } else if (length(ssl_credentials) > 0) {
    if(file.exists(CNF_PATH)) unlink(CNF_PATH, force = T)
    ssl_paths <- list()
    toWrite <- list(
      ca = paste0(dbname,"_ca.pem"),
      cert = paste0(dbname,"_cert.pem"),
      key = paste0(dbname,"_key.pem")
    )
    for (k in names(toWrite)) {
      temp_path <- paste0(getwd(), "/", toWrite[[k]])
      # if files already exist, delete them
      for (path in temp_path) {
        if(file.exists(path)) {
          print(paste0("removing ", path))
          unlink(path, force = T)
        }
      }
      writeLines(ssl_credentials[[k]], temp_path)
      system(paste0("chmod 640 ", shQuote(temp_path)))
      temporary_file_paths <- c(temporary_file_paths, temp_path)
      ssl_paths[[k]] <- temp_path
    }
  }

  # Basic connection arguments.
  conn_args <- list(
    DBI::dbDriver("MySQL"),
    user = mysql_user,
    dbname = dbname,
    host = server_ip,
    port = SERVER_PORT
  )

  # If we're using a password, add it to the arguments.
  if (!is.null(password)) {
    conn_args$password <- password
  }

  # If we're using SSL, create the credential file required by RMySQL, and add
  # it to the arguments.
  if (!is.null(ssl_paths)) {
    # This will be written to a temporary file, provided to specify that we use
    # SSL to connect to the db, and deleted again.
    text_body <- paste(
      "[client]",
      paste0("user=", mysql_user),
      paste0("ssl-ca=", ssl_paths[["ca"]]),
      paste0("ssl-cert=", ssl_paths[["cert"]]),
      paste0("ssl-key=", ssl_paths[["key"]]),
      sep = "\n"
    )

    writeLines(text_body, CNF_PATH)
    system(paste0("chmod 640 ", shQuote(CNF_PATH)))

    conn_args$default.file <- CNF_PATH
  }

  conn <- do.call(RMySQL::dbConnect, conn_args)

  # If any credential file was created, delete it so sensitive credentials don't
  # proliferate.
  for (path in temporary_file_paths) {
    if (file.exists(path)) {
      unlink(path, force = T)
    }
  }

  return(conn)
}

disconnect <- RMySQL::dbDisconnect

disconnect_all <- function () {
  # there might be an issue with number of open connection. Check discussion here:
  # https://stackoverflow.com/questions/32139596/cannot-allocate-a-new-connection-16-connections-already-opened-rmysql
  all_conns <- RMySQL::dbListConnections(RMySQL::MySQL())

  for(conn in all_conns) {
    RMySQL::dbDisconnect(conn)
  }

  all_conns <- RMySQL::dbListConnections(RMySQL::MySQL())

  return(length(all_conns))
}

# This sends lots of annoying warnings about various fields getting imported
# as various types, e.g. "Unsigned INTEGER in col 8 imported as numeric".
# We're happy with how the package handles types and never want these warnings.
query <- function (...) {
  result <- NULL
  suppressWarnings({
    result <- DBI::dbGetQuery(...)
  })

  return(result)
}

escape_strings <- RMySQL::dbEscapeStrings

row_to_values <- function (conn, row) {
  # `row` example: c(5, "5", foo", NA)
  # `value_literals example: c("5", "5", 'foo'", "NULL")
  # returns length-1 character: "(5, 5, 'foo', NULL)"
  value_literals <- ifelse(
    is.na(row),
    'NULL',
    ifelse(
      !is.na(as.numeric(row)),
      as.character(row),
      paste0("'", RMySQL::dbEscapeStrings(conn, as.character(row)), "'")
    )
  )
  return(paste0("(", paste0(value_literals, collapse = ","), ")"))
}

get_table <- function (conn, table_name) {
  query(conn, paste0("SELECT * FROM `", table_name, "`"))
}

prefix_tables <- function (tables) {
  # Useful if using the BigPipe convention of storing a series of dataframes in a
  # named list, named by their table.
  #
  # Args:
  #   tables - list where element names are table names and elements are data
  #     frames, e.g. list(User = data.frame(id = c(1, 2)))
  # Returns: matching table where all the dataframes have their column names
  #   prefixed by their (lowercased) table name, e.g.
  #   list(User = data.frame(user.id = c(1, 2)))

  for (table_name in names(tables)) {
    df <- tables[[table_name]]
    tables[[table_name]] <- util$prefix_columns(df, tolower(table_name))
  }

  return(tables)
}

create_service <- function (...) {
  # Capture the flexible arguments so we can modify them.
  args <- list(...)

  # If a password file is specificed (but the password itself is not), look
  # up the content of that file and use it as the password.
  print(args$password)
  print(args$password_file_name)
  if (is.null(args$password) && !is.null(args$password_file_name)) {
    print("looking for pw file")
    paths <- util$find_crypt_paths(list(password = args$password_file_name))
    print(paths)
    args$password <- readLines(paths$password)
    args$password_file_name <- NULL
  }
  print(args)

  # Create the connection with the adjusted arguments.
  service_connection <- do.call(connect, args)

  # Embed the connection in the service for easy calling.
  return(list(
    disconnect = function (...) disconnect(service_connection, ...),
    query = function (...) query(service_connection, ...),
    escape_strings = function (...) escape_strings(service_connection, ...),
    row_to_values = function (...) row_to_values(service_connection, ...),
    get_table = function (...) get_table(service_connection, ...)
  ))
}

create_neptune_service <- function () {
  # Requires that perts_crypt.vc be mounted.
  return(create_service(
    server_ip = '34.123.13.210',
    dbname = 'neptune',
    ssl_file_names = list(
      key = "neptune_replica_client-key.pem",
      cert = "neptune_replica_client-cert.pem",
      ca = "neptune_replica_server-ca.pem"
    ),
    mysql_user = 'readonly',
    password_file_name = 'neptune_replica_readonly_password.txt',
    password = NULL
  ))
}

create_triton_service <- function () {
  # Requires that perts_crypt.vc be mounted.
  return(create_service(
    server_ip = '35.188.76.62',
    dbname = 'triton',
    ssl_file_names = list(
      ca = 'triton_sql_production-01-analysis-replica.ca',
      key = 'triton_sql_production-01-analysis-replica.key',
      cert = 'triton_sql_production-01-analysis-replica.cert'
    ),
    mysql_user = 'readonly'
  ))
}

create_neptune_test_service <- function () {
  return(create_service(
    server_ip = '127.0.0.1',
    dbname = 'neptune-test-rserve',
    mysql_user = 'neptune',
    password = 'neptune'
  ))
}

create_triton_test_service <- function () {
  # Requires that perts_crypt.vc be mounted.
  return(create_service(
    server_ip = '127.0.0.1',
    dbname = 'triton-test-rserve',
    mysql_user = 'triton',
    password = 'triton'
  ))
}
