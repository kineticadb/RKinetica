#' @include kineticaDriver.R
#' @include kineticaConnection.R
#' @include kineticaResult.R
#' @include kineticaSQL.R

#' @import httr
#' @importFrom stats setNames
#' @import RJSONIO
#' @import bit64
#' @import purrr

# constants
.empty_set <- stats::setNames(RJSONIO::fromJSON('{}'), character(0))

# endpoint
.show_security <- "/show/security"

# Internal function that returns this user's default schema
get_default_schema <- function(url = "character", username = "character", password = "character") {
  if (!is.null(username) && nchar(username) > 0)
    name <- username
  else
    name <- "anonymous"
  resp <- .try_connection(url = url, endpoint = .show_security, username = username, password = password,
                          body = list(names = list(name)), options = I(.empty_set))

  if (status_code(resp) == 200) {
    permissions <- RJSONIO::fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)$permissions
    user_properties <- permissions[[name]]
    for (item in user_properties) {
      if ("default_schema" %in% attributes(item)$names) {
        return (item$default_schema)
      }
    }
    # None of the username collection attributes matches "default_schema" label,
    # so the user is not able to read/write tables in any schema
    stop(paste("User", name, "does not have a 'default_schema' assigned. Please contact your administrator." ), call. = FALSE)
  } else {
    # Return error if the endpoint cannot be reached
    stop(content(resp)$message, call. = FALSE)
  }
}

# endpoint
.show_system_properties <- "/show/system/properties"

# Internal function that verifies that Kinetica DB is accessible as a single
# instance or HA-enabled ring and returns Kinetica DB configuration properties
show_system_properties <- function(url = "character", username = "character", password = "character", ha_ring = "character") {
  secondary_connection_refused <- NULL
  tryCatch({
    # Try connecting with primary URL
    primary_connection_refused <- ""
    resp <- .try_connection(url = url, endpoint = .show_system_properties, username = username, password = password)

    # Established connection with primary url
    if (status_code(resp) == 200) {
      data <- RJSONIO::fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)
      on.exit(rm(data))
      # Return Kinetica DB instance properties
      return (data$property_map)
    } else if (status_code(resp) %in% c(500, 503, 504)) {
      # Print a warning before trying HA ring failover
      warning(paste("Primary instance at", url, "failed connection. ", content(resp)$message), call. = FALSE)
      primary_connection_refused <<- primary_connection_refused
    } else {
      # Return error if it is unrelated to connection issues
      stop(content(resp)$message, call. = FALSE)
    }
  },
  error = function(e) {
    # Catch curl-level exception and print a warning
    warning(paste("Primary instance at", url, "failed connection. ", e$message), call. = FALSE)
    primary_connection_refused <<- e$message
  })
  # Establishing connection with primary url failed.
  # Parse and clean provided ha_ring, if available.
  if (!is.na(ha_ring) && !is.null(ha_ring) && ha_ring != "") {
    uris <- unlist(strsplit(ha_ring, ","))
    # Find position of primary url in ha_ring
    primary_url_idx <- match(url, uris)
    if (!is.na(primary_url_idx)) {
      # Exclude primary url from potential retry attempts because it has failed already
      uris <- uris[-primary_url_idx]
    }
    # Set default error message
    secondary_connection_refused <- "Provided ha_ring configuration is not valid"
    # Retry connecting with HA_ring instances
    for (uri in uris) {
      tryCatch ({
        resp <- .try_connection(url = uri, endpoint = .show_system_properties, username = username, password = password)
        if (status_code(resp) == 200) {
          data <- RJSONIO::fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)
          on.exit(rm(data))
          # On success, return secondary Kinetica DB instance properties
          return (data$property_map)
        } else {
          # Save error message
          warning(paste("Backup instance at", uri, "failed connection.", content(resp)$message), call. = FALSE)
          secondary_connection_refused <<- content(resp)$data_str
        }
      },
      error = function(e) {
        # Catch curl-level exception and print a warning
        warning(paste("Backup instance at", url, "failed.", e$message), call. = FALSE)
        secondary_connection_refused <<- e$message
      })
    }
    # Both primary url and ha_ring instances failed to establish connection, rethrow error
    stop(paste("Provided HA ring ", ha_ring, "failed to establish connection.", secondary_connection_refused), call. = FALSE)
  }
  # Primary url failed to establish a successful connection and ha_ring was not provided, rethrow error
  stop(paste("Failed to establish connection with primary instance", url, " and HA ring was not provided. ",
             primary_connection_refused), call. = FALSE)
}

# endpoint
.has_table <- "/has/table"

# Internal function that checks that table exists by table name
has_table <- function(conn = "KineticaConnection", name = "character") {
  resp <- .post(conn, .has_table, body = list(table_name = name), options = I(.empty_set))
  RJSONIO::fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)$table_exists
}

# endpoints
.has_schema <- "/has/schema"
.create_schema <- "/create/schema"
.options_create_schema <- list(no_error_if_exists = "true")


# Internal function that checks that schema extracted from the table name exists
check_schema_name <- function(conn = "KineticaConnection", id = "KineticaId") {
  if (is.na(id@name["schema"])) {
    # If no schema provided, user's default schema would be used
    return (TRUE)
  }
  schema <- unname(id@name["schema"])
  resp <- .post(conn, .has_schema, body = list(schema_name = schema), options = I(.empty_set))
  schema_exists <- RJSONIO::fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)$schema_exists
  if (!schema_exists) {
    # Create the schema if it does not exist before creating table within it
    create_resp <- .post(conn, .create_schema, body = list(schema_name = schema), options = .options_create_schema)
    RJSONIO::fromJSON(content(create_resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)$schema_name
    return (TRUE)
  }
}


# endpoint
.show_table <- "/show/table"
.options_show_collection_tables <- list(get_column_info = "false", get_sizes = "false", show_children = "true", no_error_if_not_exists = "false")

# Internal function that lists all tables with exact or partial name match
show_tables <- function(conn = "KineticaConnection", name = "character") {
  tryCatch({
    resp <- .post(conn, .show_table, body = list(table_name = name), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)
    as.character(data$table_names)
  },
  error = function(e) {
    # "*" search string lists all top level schemas and schema tables within that user has permissions to access.
    resp <- .post(conn, .show_table, body = list(table_name = "*"), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)
    t_names <- as.character(data$table_names)
    found <- grep(name, t_names)
    t_names[found]
  })
}

# Internal function that lists all top level objects (including collections)
show_objects <- function(conn = "KineticaConnection", name = "character") {
  tryCatch({
    resp <- .post(conn, .show_table, body = list(table_name = name), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)
    t_names <- as.character(data$table_names)
    t_types <- as.character(data$type_labels)
    # Marking all Kinetica <collection> objects as prefix to allow user lookup tables within
    # by submitting is_prefix strings in search text.
    is_prefix <- (t_types == rep("<collection>", length(t_types)))
    ds <- data.frame(table = t_names, is_prefix = is_prefix, type = t_types)
  },
  error = function(e) {
    resp <- .post(conn, .show_table, body = list(table_name = "*"), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)
    t_names <- as.character(data$table_names)
    found <- grep(name, t_names)

    t_types <- as.character(data$type_labels)
    # Marking all Kinetica <collection> objects as prefix to allow user lookup tables within
    # by submitting is_prefix strings in search text.
    is_prefix <- (t_types == rep("<collection>", length(t_types)))
    ds <- data.frame(table = t_names[found], is_prefix = is_prefix[found], type = t_types[found])
  })

}

# endpoint
.execute_sql <- "/execute/sql"
.options_execute_sql <- list(parallel_execution = "false")
.options_bind <- list(parallel_execution = "false", prepare_mode = "false")

# Internal function that processes all data manipulation queries
execute_sql <- function(conn = "KineticaConnection", statement = "character", offset = NULL, limit = NULL,
                        data = NULL, prepare_mode = NULL, no_return_data = NULL) {
  # print(paste("SQL >", statement))
  if (grepl("cast(? as ", statement, fixed = TRUE)) {
    stop(paste("Bind operation on return value is not supported by Kinetica. ",
               "\nUnsupported cast expression was found in this statement [SQL:", statement, "]"), call. = FALSE)
  }

  if (grepl("[$]", statement)) {
    stop(paste("Named or numbered parameters are not supported by Kinetica. ",
               "\nUnsupported placeholders with prefix '$' were found in this statement [SQL:", statement, "]"), call. = FALSE)
  }
  if (missing(offset) || is.null(offset) || is.na(offset) ) {
    offset <- 0L
  } else {
    offset <- as.integer(offset)
  }
  if (missing(limit) || is.null(limit) || is.na(limit)) {
    limit <- conn@row_limit
  } else {
    limit <- as.integer(limit)
  }
  if (missing(no_return_data) || is.null(no_return_data) || is.na(no_return_data)) {
    no_return_data <- !has_return_data(statement)
  }
  statement <- trim_leading_spaces(statement)
  options <- .options_execute_sql

  placeholder_data <- list()
  resp <- .post(conn, .execute_sql,
    body = list(statement = statement, offset = offset, limit = limit, encoding ="json", request_schema_str = "", data = placeholder_data),
    options = options)

  data_raw <- RJSONIO::fromJSON(content(resp)$data_str, nullValue=NA, simplifyWithNames = FALSE)
  json_obj <- RJSONIO::fromJSON(data_raw$json_encoded_response, nullValue=NA, simplifyWithNames = FALSE)

  # pick out colnames and datatypes
  col_names <- json_obj$column_headers
  data_fields <- json_obj$column_datatypes
  json_obj$column_headers <- NULL
  json_obj$column_datatypes <- NULL

  if (missing(prepare_mode) || !is.logical(prepare_mode)) {
    prepare_mode <- FALSE
  }

  if (no_return_data || prepare_mode || is.null(unlist(json_obj))) {
    df <- NULL
    dataset <- skeletonDataFrame(col_names, data_fields)
  } else {
      # Speedy purrr::map2_dfc data parsing
    tryCatch({
      df <- purrr::map2_dfc(json_obj, sapply(data_fields, conversion_fn),
                          function(x, y) do.call(y, list(x)))
    }, error = function(e) {
      stop(paste0("There was an error during JSON data parsing.\n",
      e$message))
    })
    colnames(df) <- col_names
    dataset <- as.data.frame(
      df,
      row.names = NULL,
      optional = FALSE,
      cut.names = FALSE,
      col.names = col_names,
      fix.empty.names = TRUE,
      stringsAsFactors = default.stringsAsFactors()
    )
  }

  names(data_fields) <- col_names
  result <- KineticaResult(
    connection = conn,
    statement = statement,
    data = dataset,
    fields = data_fields,
    count_affected = data_raw$count_affected,
    total_number_of_records = data_raw$total_number_of_records,
    has_more_records = data_raw$has_more_records,
    offset = offset,
    limit = limit,
    paging_table = data_raw$paging_table)

  on.exit(rm(resp, data_raw, json_obj, col_names, data_fields, df, dataset))
  return(result)
}

# Internal function that preprocesses SQL statements
trim_leading_spaces <- function(x) sub("^\\s+", "", x)

# Internal function that checks if SQL returns dataset or number of rows affected
has_return_data <- function(statement) {
  statement <- trim_leading_spaces(statement)
  return(startsWith(statement, "SELECT") || startsWith(statement, "DESC") || startsWith(statement, "SHOW"))
}

# Internal function that creates a blank data.frame based on JSON schema
skeletonDataFrame <- function(column_headers, column_datatypes){
  df = data.frame(stringsAsFactors = default.stringsAsFactors())
  for (i in 1:length(column_headers)) {
    type <- column_datatypes[i]

    if (type %in% c('int', 'int8', 'int16', 'integer', 'long')) {
      df[column_headers[i]] = integer(0)
    } else if (type %in% c('char1', 'char2', 'char4', 'char8', 'char16', 'char32', 'char64',
              'char128', 'char256', 'string', 'ipv4', 'wkt', 'geography', 'geometry')) {
      df[column_headers[i]] = character(0)
    } else if (type %in% c('double', 'float', 'decimal')) {
      df[column_headers[i]] = numeric(0)
    } else if (type %in% c('datetime', 'date')) {
      df[column_headers[i]] = as.character.Date(rep(NA,0))
    } else if (type == 'time') {
      df[column_headers[i]] = hms::hms()
    } else if (type == 'timestamp') {
      df[column_headers[i]] = .POSIXct(rep(NA,0))
    } else if (type == 'bytes') {
      df[column_headers[i]] = raw(0)
    } else {
      df[column_headers[i]] = character(0)
    }
  }
  row.names(df) = NULL
  return(df)
}

# Internal function that fast-maps Kinetica type to conversion function
conversion_fn  <- function(type) {
  # if character or numeric then it's automatically picked up
  if (type %in% c('char1', 'char2', 'char4', 'char8', 'char16', 'char32',
                  'char64', 'char128', 'char256', 'string', 'ipv4',
                  'wkt', 'geography', 'geometry')) {
    conversion_fn = as.character
  } else if (type %in% c('int', 'int8', 'int16', 'integer', 'long')) {
    conversion_fn = as.integer
  } else if (type %in% c('double', 'float', 'decimal', 'ulong')) {
    conversion_fn = as.numeric
  } else if (type %in% c('datetime', 'date')) {
    conversion_fn = function(x)
      as.character.Date(
        x,
        tryFormats=c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d", "%Y/%m/%d"),
        origin = "1900-01-01",
        optional = FALSE
      )
  } else if (type == 'time') {
    conversion_fn = function(x) hms::as_hms(x)
  } else if (type == 'timestamp') {
    conversion_fn = function(x) {
      stopifnot(is.numeric(x))
      as.POSIXct(x / 1000, origin = "1970-01-01")
      attr(x, "tzone") <- "UTC"
      x
    }
  } else if (type == 'bytes') {
    conversion_fn = as.raw
  } else {
    stop (paste('Unknown type:', type))
  }
}

# const HTTP headers
.headers <- c(Accept = "application/json", "Accept-Encoding" = "UTF-8", "Content-Type" = "application/json")

# Internal function that wraps POST request with HA ring request failover
.post <- function(conn = "KineticaConnection", endpoint = "character", body = "ANY", options = "ANY") {

  primary_connection_refused <- ""
  tryCatch({
    resp = .try_connection(url = .get_url(conn), endpoint = endpoint, username = conn@username, password = conn@password,
                           body = body, options = options)
    if (status_code(resp) == 200) {
      # Successful connection
      return (resp)
    } else if (!conn@ha_enabled || !(status_code(resp) %in% c(500, 503, 504))) {
      # Return error, if it is unrelated to connection issues or HA is not enabled
      stop(content(resp)$message, call. = FALSE)
    } else {
      # Save the error message before retrying HA ring
      primary_connection_refused <<- content(resp)$message
    }
  }, error = function(e){
    if (!conn@ha_enabled) {
      # Rethrow error when HA ring unavailable
      stop(e$message, call. = FALSE)
    } else {
      # Save the error message before retrying HA ring
      primary_connection_refused <<- e$message
    }
  })

  # Try running the query on backup HA instances
  if (conn@ha_enabled) {
    # Can't connect with current instance due to server-side error.
    # Retry query for each instance in ha_ring until successful
    # connection reached or all instances failed to connect.
    ring_size <- length(conn@ha_ring)
    # Use primary url position in ha_ring as a starter to iterate over it
    current <- match(conn@url, conn@ha_ring)
    # Alternative: use the position of last active ptr
    # current <- as.integer(conn@ha_ptr[["current"]])
    for (i in 1:ring_size) {
      tryCatch({
        resp <- .try_connection(url = conn@ha_ring[current], endpoint = endpoint, username = conn@username,
                        password = conn@password, body = body, options = options)
        if (status_code(resp) == 200) {
          # Successful connection reached, save current instance to ha_ptr
          conn@ha_ptr[["current"]] <- current
          return (resp)
        } else if (! status_code(resp) %in% c(500, 503, 504)) {
          # Return error if it is unrelated to connection issues
          stop(content(resp)$message, call. = FALSE)
        } else {
          # Catch connection error and save for later
          secondary_connection_refused <<- content(resp)$message
        }
      },
      error = function(e) {
        # Catch curl error and save for later
        secondary_connection_refused <<- e$message
      })
      # Move current index to the right in ha_ring until all instances are tried
      current <- ifelse (current < ring_size, current + 1L, 1L)
    }
    # All the instances in ha_ring failed connection, throw exception
    stop(paste("HA number of re-tries exceeded.", secondary_connection_refused), call. = FALSE)
  } else {
    # Primary url failed to establish a successful connection and ha_ring was not provided, rethrow error
    stop(paste("Failed to establish connection with instance", url, " and HA ring was not provided. ",
               primary_connection_refused), call. = FALSE)
  }
}


# Internal function that sends POST request and returns response as is
.try_connection <- function(url = "character", endpoint = "character", username = "character", password = "character",
                            body = .empty_set, options = .empty_set) {
  uri <- paste0(url, endpoint, sep = "")
  body$options <- options
  json_body <- toJSON(body, method = "C")
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  if (!is.null(username) && nchar(username) > 0 ) {
    return(POST(uri, authenticate(username, password), add_headers(.headers),  body = json_body, encode = "json"))
  } else {
    return(POST(uri, add_headers(.headers), body = json_body, encode = "json"))
  }
}

# Internal function that returns primary url or current HA ring url
.get_url <- function(conn = "KineticaConnection") {
  if (!conn@ha_enabled)
    return (conn@url)
  else {
    current <- as.integer(conn@ha_ptr[["current"]])
    return (conn@ha_ring[[current]])
  }
}
