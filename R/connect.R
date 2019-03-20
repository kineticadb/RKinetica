#' @include kineticaDriver.R
#' @include kineticaConnection.R
#' @include kineticaResult.R
#' @include kineticaSQL.R

#' @import httr
#' @import rjson
#' @import bit64

#constants
.empty_set <- setNames(rjson::fromJSON('{}'), character(0))

.has_table <- "/has/table"

has_table <- function(conn = "KineticaConnection", name = "character") {
  resp <- .post(conn, .has_table, body = list(table_name = name), options = I(.empty_set))
  rjson::fromJSON(content(resp)$data_str)$table_exists
}


.show_table <- "/show/table"

.options_show_collection_tables <- list(get_column_info = "false", get_sizes = "false", show_children = "true", no_error_if_not_exists = "false")


show_tables <- function(conn = "KineticaConnection", name = "character") {
  tryCatch({
    resp <- .post(conn, .show_table, body = list(table_name = name), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str)
    as.character(data$table_names)
  },
  error = function(e) {
    resp <- .post(conn, .show_table, body = list(table_name = ""), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str)
    t_names <- as.character(data$table_names)
    found <- grep(name, t_names)
    t_names[found]
  })
}

show_objects <- function(conn = "KineticaConnection", name = "character") {
  tryCatch({
    resp <- .post(conn, .show_table, body = list(table_name = name), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str)
    t_names <- as.character(data$table_names)
    t_types <- as.character(data$table_descriptions)
    is_prefix <- (t_types == rep("COLLECTION", length(t_types)))
    ds <- data.frame(table = t_names, is_prefix = is_prefix, type = t_types)
  },
  error = function(e) {
    resp <- .post(conn, .show_table, body = list(table_name = ""), options = .options_show_collection_tables)
    data <- fromJSON(content(resp)$data_str)
    t_names <- as.character(data$table_names)
    found <- grep(name, t_names)

    t_types <- as.character(data$table_descriptions)
    is_prefix <- (t_types == rep("COLLECTION", length(t_types)))
    ds <- data.frame(table = t_names[found], is_prefix = is_prefix[found], type = t_types[found])
  })

}


.execute_sql <- "/execute/sql"
.options_execute_sql <- list(parallel_execution = "false")

execute_sql <- function(conn = "KineticaConnection", statement = "character", offset = NULL, limit = NULL, data = NULL, no_return_statement = TRUE) {
  if (is.null(offset)) {
    offset <- 0L
  } else {
    offset <-as.integer(offset)
  }
  if (is.null(limit)) {
    limit <- 10000L
  } else {
    limit <- as.integer(limit)
  }
  if (is.null(data) || is.na(data)) {
    data <- list()
  }

  resp <- .post(conn, .execute_sql,
      body = list(statement = statement, offset = offset, limit = limit, encoding ="json", request_schema_str = "", data = data),
      options = .options_execute_sql)
  data_raw <- rjson::fromJSON(content(resp)$data_str)

  json_obj <- rjson::fromJSON(data_raw$json_encoded_response)

  col_names <- as.character(json_obj$column_headers)

  data_fields <- as.character(lapply(json_obj$column_datatypes, .RtypeFromKinetica))

  names(data_fields) <- col_names

  if (no_return_statement) {
    dataset <- data.frame()
  } else if (data_raw$total_number_of_records > 0) {
    dataset <- kineticaJSONtoDataFrame(as.character(data_raw$json_encoded_response))
  }
   else {
    dataset <- skeletonDataFrame(as.character(data_raw$json_encoded_response))
   }

  result <- KineticaResult(
                  connection = conn,
                  statement = statement,
                  data = dataset,
                  fields = data_fields,
                  count_affected = data_raw$count_affected,
                  total_number_of_records = data_raw$total_number_of_records,
                  has_more_records = data_raw$has_more_records)
  on.exit(rm(resp, data_raw, json_obj, col_names, data_fields, dataset))
  return(result)
}

#' @importFrom bit64 integer64 as.integer64
kineticaJSONtoDataFrame <- function (str){
  js = rjson::fromJSON(str)
  temp = as.data.frame(lapply(seq_along(js$column_datatypes), function (inx) {
    type = js$column_datatypes[inx]
    conversion_fn = NULL
    if (type %in% c('int', 'int16', 'integer')) {
      conversion_fn = as.integer
    } else if (type == 'int8') {
      conversion_fn = as.logical
    } else if (type == 'long') {
      conversion_fn = as.integer64
    } else if (type %in% c('char1', 'char2', 'char4', 'char8', 'char16', 'char32', 'char64', 'char128', 'char256', 'string', 'ipv4')) {
      conversion_fn = as.character
    } else if (type %in% c('double', 'float', 'decimal')) {
      conversion_fn = as.numeric
    } else if (type %in% c('datetime', 'date')) {
      conversion_fn = function(x) as.character.Date(x, tryFormats = c("%Y-%m-%d",  "%Y-%m-%d %H:%M:%S", "%Y/%m/%d"), origin="1900-01-01", optional = FALSE)
    } else if (type == 'time') {
      conversion_fn = function(x) hms::as.hms(x)
    } else if (type == 'timestamp') {
      conversion_fn = function(x) {
        stopifnot(is.numeric(x))
        as.POSIXct(x/1000, tzone = "UTC", origin = "1970-01-01")
      }
    } else {
      stop (paste('Unknown type:', type))
    }
    sapply(js[[paste('column', inx, sep = '_')]],
           FUN = function(x) {
             conversion_fn(ifelse(is.null(x), NA,  x))
           },
           simplify = T)
  }), row.names = NULL, optional = FALSE, cut.names = FALSE, col.names = js$column_headers,
  fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors())
}

skeletonDataFrame <- function(str){
  js = rjson::fromJSON(str)
  df = data.frame()
  for (i in 1:length(js$column_headers)) {
    type <- js$column_datatypes[i]

    if (type %in% c('int', 'int16', 'integer')) {
      df[js$column_headers[i]] = integer(0)
    } else if (type == 'int8') {
      df[js$column_headers[i]] = logical(0)
    } else if (type == 'long') {
      df[js$column_headers[i]] = bit64::integer64(0)
    } else if (type %in% c('char1', 'char2', 'char4', 'char8', 'char16', 'char32', 'char64', 'char128', 'char256', 'string', 'ipv4')) {
      df[js$column_headers[i]] = character(0)
    } else if (type %in% c('double', 'float', 'decimal')) {
      df[js$column_headers[i]] = numeric(0)
    } else if (type %in% c('datetime', 'date')) {
      df[js$column_headers[i]] = as.character.Date(rep(NA,0))
    } else if (type == 'time') {
      df[js$column_headers[i]] = hms::hms()
    } else if (type == 'timestamp') {
      df[js$column_headers[i]] = .POSIXct(rep(NA,0))
    } else {
      df[js$column_headers[i]] = character(0)
    }
  }
  row.names(df) = NULL
  return(df)
}

# const
.headers <- c(Accept = "application/json", "Accept-Encoding" = "UTF-8", "Content-Type" = "application/json")

.post <- function(conn = "KineticaConnection", path = "character", body = "ANY", options = "ANY") {
  path <- paste0(conn@url, path, sep = "")

  body$options <- options
  json_body <- toJSON(body, method = "C")
  sql <- body$statement

  if (!is.null(conn@username) && nchar(conn@username) > 0 ) {
    resp <- POST(path, authenticate(conn@username, conn@password), add_headers(.headers),  body = json_body, encode = "json")
  } else {
    resp <- POST(path, add_headers(.headers), body = json_body, encode = "json")
  }
  if (status_code(resp) == 200) {
    resp
  } else {
    stop(content(resp)$message, call. = FALSE)
  }
}

.RtypeFromKinetica <- function(x) {
  x <- as.character(x)
    switch(x,
           "int8" = "logical",
           "int" = "integer",
           "integer" = "integer",
           "int16" = "integer",
           "long" = "integer64",

           "float" = "double",
           "double" = "double",
           "decimal" = "double",

           "bytes" = "raw",

           "string" = "character",
           "char256" = "character",
           "char128" = "character",
           "char64" = "character",
           "char32" = "character",
           "char16" = "character",
           "char8" = "character",
           "char4" = "character",
           "char2" = "character",
           "char1" = "character",
           "ipv4" = "character",

           "date" = "character.Date",
           "time" = "character.Date",
           "datetime" = "character.Date",
           "timestamp" = "numeric.POSIXt",
           stop(paste0("Unknown Kinetica type, can't migrate to R data ", x))
          )

}