#' @include kineticaDriver.R

#' Class KineticaConnection
#'
#' This virtual class encapsulates the connection to Kinetica DB, provides access to queries,
#' result sets, SQL expressions and literals, session management tools, etc.
#'
#' @docType class
#' @keywords internal
#' @aliases KineticaConnection
#' @import DBI
#' @import methods
#' @slot drv an object derived from [KineticaDriver-class]
#' @slot host character string for Kinetica DB host address
#' @slot port integer value for Kinetica DB port
#' @slot url character string for Kinetica DB url (protocol + host + port),
#' when HA is enabled, this slot holds the primary Kinetica url.
#' @slot username character string for Kinetica DB username
#' @slot password character string for Kinetica DB password
#' @slot timeout  integer value for Kinetica DB connection timeout
#' @slot ptr connection pointer (for looking up connection in Driver connection pool)
#' @slot db.version Kinetica DB version returned by client upon connection
#' @slot transaction a local environment storing transaction state
#' @slot row_limit maximum amount of rows returned in a query
#' @slot ha_enabled logical flag if KineticaDB instance provides HA ring
#' @slot ha_ring a ring of HA urls to be used when primary url fails
#' @slot ha_ptr a local environment storing pointer at the active url
#' @slot assume_no_nulls logical flag for faster record parsing which
#' requires that no NULL values are present in the incoming dataset
#' @slot results a local environment storing active query results
#' @export
setClass("KineticaConnection",
         contains = c("DBIConnection", "KineticaObject"),
         slots = list(
           drv = "KineticaDriver",
           host = "character",
           port = "numeric",
           url = "character",
           username = "character",
           password = "character",
           timeout = "numeric",
           ptr = "character",
           db.version = "character",
           transaction = "environment",
           row_limit = "integer",
           ha_enabled = "logical",
           ha_ring = "character",
           ha_ptr = "environment",
           assume_no_nulls = "logical",
           results = "environment"
         )
)


#' show()
#'
#' Minimal KineticaConnection string representation
#' Provides url of the Kinetica DB instance
#' @family KineticaConnection methods
#' @rdname show
#' @param object [KineticaConnection-class]
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(d, url = "http://localhost:9191")
#' show(con)
#' <KineticaConnection>
#'   url: http://localhost:9191
#'}
setMethod("show", "KineticaConnection", function(object) {
  if (!dbIsValid(object)) {
    stop("Invalid Kinetica Connection", call. = FALSE)
  }
  cat("<", is(object)[1], ">\n", sep = "")
  if (object@ha_enabled) cat(object@ha_ptr[["label"]], "enabled\n")
  if (object@username != "") cat("user:", object@username, "\n")
  if (object@ha_enabled)
    cat("Current URL:", object@ha_ring[[as.integer(object@ha_ptr[["current"]])]], "\n")
  else
    cat("URL:", object@url, "\n")
})


#' dbGetInfo()
#'
#' Basic properties of KineticaConnection object
#' provides DB version, primary url, host, port and username of the
#' current connection, flag for HA availability, HA ring of secondary
#' urls and maximum row count that can be returned in one query
#' @family KineticaConnection methods
#' @rdname dbGetInfo
#' @param dbObj object [KineticaConnection-class]
#' @param ...  Other arguments omitted in generic signature
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(d, url = "http://localhost:9191")
#' # db.version
#' # "7.0.0.0"
#' # dbname
#' # "Kinetica"
#' # url
#' # "http://localhost:9191"
#' # host
#' # "localhost"
#' # port
#' # 9191
#' # username
#' # ""
#'}
setMethod("dbGetInfo", "KineticaConnection",
  function(dbObj, ...) {
    if (!dbIsValid(dbObj)) {
      stop("Invalid connection", call. = FALSE)
    }
    list(name = "KineticaConnection",
         db.version = dbObj@db.version,
         dbname = dbObj@drv@dbname,
         url =  dbObj@url,
         host = dbObj@host,
         port = dbObj@port,
         username = dbObj@username,
         ha_enabled = dbObj@ha_enabled,
         ha_ring = dbObj@ha_ring,
         row_limit = dbObj@row_limit)
  }
)


#' dbAppendTable()
#'
#' Appends rows to an existing table
#' @family KineticaConnection methods
#' @rdname dbAppendTable
#' @param conn an object of [KineticaConnection-class]
#' @param name character string for table name
#' @param value A data frame. Factors will be converted to character vectors.
#' @param ...  Other arguments omitted in generic signature
#' @param row.names a flag with logical, character or NULL value
#' @export
setMethod("dbAppendTable", "KineticaConnection",
  function(conn, name, value, ..., row.names = NULL) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    if(missing(name) || .invalid_character(name)) {
      stop(paste("Invalid table name", name), call. = FALSE)
    }
    if(!is.null(row.names)) {
      stop("Row names are not supported")
    }
    if(!dbExistsTable(conn, name)) {
      stop(paste("Table ", name, "does not exist"))
    }
    query <- sqlAppendTableTemplate(
      con = conn,
      table = name,
      values = value,
      row.names = row.names,
      prefix = "?",
      pattern = "",
      ...
    )

    dbExecute(conn, query, params = as.list(value))
  })

#' sqlAppendTable
#'
#' Compose query to insert rows into a table
#'
#' `sqlAppendTable()` generates a single SQL string that inserts a
#' data frame into an existing table. `sqlAppendTableTemplate()` generates
#' a template suitable for use with [dbBind()].
#' The default methods are ANSI SQL 99 compliant.
#' These methods are mostly useful for backend implementers.
#'
#' The `row.names` argument must be passed explicitly in order to avoid
#' a compatibility warning.  The default will be changed in a later release.
#'
#' @param con A database connection.
#' @param table Name of the table. Escaped with [dbQuoteIdentifier()].
#' @param values A data frame. Factors will be converted to character vectors.
#' @param row.names a flag with logical, character or NULL value
#' @param ... Other parameters passed on to methods.
#'   Character vectors will be escaped with [dbQuoteString()].
#' @family SQL generation
#' @export
setGeneric("sqlAppendTable",
     def = function(con, table, values, row.names = NA, ...) standardGeneric("sqlAppendTable")
)

#' @export
setMethod("sqlAppendTable", signature("DBIConnection"),
    function(con, table, values, row.names = NA, ...) {
      if(!is.list(values)) {
        stop(paste("Values are not in expected format", values), call. = FALSE)
      }

      if (missing(row.names)) {
        warning("Do not rely on the default value of the row.names argument for sqlAppendTable(), it will change in the future.",
                call. = FALSE
        )
      }

      sql_values <- sqlData(con, values, row.names)
      table <- dbQuoteIdentifier(con, table)
      fields <- dbQuoteIdentifier(con, names(sql_values))

      # Convert fields into a character matrix
      rows <- do.call(paste, c(sql_values, sep = ", "))
      SQL(paste0(
        "INSERT INTO ", table, "\n",
        "  (", paste(fields, collapse = ", "), ")\n",
        "VALUES\n",
        paste0("  (", rows, ")", collapse = ",\n")
      ))
    }
)

#' sqlAppendTableTemplate()
#'
#' Creates a placeholder-based template
#'
#' @param con A database connection.
#' @param table Name of the table. Escaped with [dbQuoteIdentifier()].
#' @param values A data frame. Factors will be converted to character vectors.
#' @param row.names a flag with logical, character or NULL value
#' @param ... Other parameters passed on to methods.
#'   Character vectors will be escaped with [dbQuoteString()].
#' @param prefix Parameter prefix to use for placeholders.
#' @param pattern Parameter pattern to use for placeholders:
#' - `""`: no pattern
#' - `"1"`: position
#' - anything else: field name
#' @family SQL generation
#' @export
sqlAppendTableTemplate <- function(con, table, values, row.names = NA, prefix = "?", ..., pattern = "1") {
  if (missing(row.names)) {
    warning("Do not rely on the default value of the row.names argument for sqlAppendTableTemplate(), it will change in the future.",
            call. = FALSE
    )
  }

  table <- dbQuoteIdentifier(con, table)

  values <- DBI::sqlRownamesToColumn(values[0, , drop = FALSE], row.names)
  fields <- dbQuoteIdentifier(con, names(values))

  if (pattern == "") {
    suffix <- rep("", length(fields))
  } else if (pattern == "1") {
    suffix <- as.character(seq_along(fields))
  } else {
    suffix <- names(fields)
  }

  placeholders <- lapply(paste0(prefix, suffix), SQL)
  names(placeholders) <- names(values)

  sqlAppendTable(
    con = con,
    table = table,
    values = placeholders,
    row.names = row.names
  )
}

#' sqlCreateTable()
#'
#' Customised version of the generic sqlCreateTable method
#' (Kinetica DB uses TEMP keyword instead of TEMPORARY)
#' @family KineticaConnection methods
#' @rdname sqlCreateTable
#' @param con A database connection.
#' @param table Name of the table. Escaped with
#'   [dbQuoteIdentifier()].
#' @param fields Either a character vector or a data frame.
#'
#' A named character vector: Names are column names, values are types.
#' Names are escaped with [dbQuoteIdentifier()].
#' Field types are unescaped.
#'
#' A data frame: field types are generated using [dbDataType()].
#' @param row.names a flag with logical, character or NULL value
#' @param temporary logical TRUE/FALSE for temporary/permanent table created
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("sqlCreateTable", signature("KineticaConnection"),
  function(con, table, fields, row.names = NA, temporary = FALSE, ...) {
    if (missing(row.names)) {
      warning("Do not rely on the default value of the row.names argument for sqlCreateTable(), it will change in the future.",
              call. = FALSE
      )
      row.names = NA
    }

    table <- dbQuoteIdentifier(con, table)

    if (is.data.frame(fields)) {
      fields <- DBI::sqlRownamesToColumn(fields, row.names)
      fields <- vapply(fields, function(x) dbDataType(con, x), character(1))
    }

    field_names <- dbQuoteIdentifier(con, names(fields))
    field_types <- unname(fields)
    fields <- paste0(field_names, " ", field_types)

    SQL(paste0(
      "CREATE ", if (temporary) "TEMP ", "TABLE ", table, " (\n",
      "  ", paste(fields, collapse = ",\n  "), "\n)\n"
    ))
  }
)


#' dbCreateTable()
#'
#' Creates a new table
#' @family KineticaConnection methods
#' @rdname dbCreateTable
#' @param conn an object of [KineticaConnection-class]
#' @param name character string for table name
#' @param fields either a named character vector or a data frame
#' @param ... Other parameters passed on to methods.
#' @param row.names Must be `NULL`.
#' @param temporary logical flag value whether table should be temporary
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbCreateTable(con, "tableA", data.frame(a = 1))
#' dbExistsTable(con, "tableA")
#' # TRUE
#' dbDisconnect(con)
#'}
#' @export
setMethod ("dbCreateTable", "KineticaConnection",
   function(conn, name, fields, ..., row.names = NULL, temporary = FALSE) {
     if (!dbIsValid(conn)) {
       stop("Invalid Kinetica Connection", call. = FALSE)
     }
     if(missing(name) || .invalid_character(name)) {
       stop(paste("Invalid table name", name), call. = FALSE)
     }
     if (!is.null(row.names) ) {
       stop("Invalid row names ", call. = FALSE)
     }
     if(!is.logical(temporary) || length(temporary) != 1L) {
       stop("Invalid temporary parameter ", call. = FALSE)
     }
     if(dbExistsTable(conn, name)) {
       warning(paste("Table ", name, "already exists"))
     } else {
         query <- sqlCreateTable(
           con = conn,
           table = dbQuoteIdentifier(conn, name),
           fields = fields,
           row.names = row.names,
           temporary = temporary,
           ...
         )
         dbExecute(conn, query)
         invisible(TRUE)
         }
   }
)

#' dbDataType()
#'
#' Redirect to [KineticaDriver-class] dbDataType method for data type mapping of provided object
#' @family KineticaConnection methods
#' @param dbObj an object of [KineticaConnection-class]
#' @param obj generic object
#' @param ... Other arguments omitted in generic signature
#' @export
setMethod("dbDataType", signature("KineticaConnection", "ANY"),
    function(dbObj, obj, ...) {
      if (!dbIsValid(dbObj)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      dbDataType(dbObj@drv, obj)
})


#' dbDisconnect()
#'
#' Disconnects the current connection, releasing memory from all uncleared result objects
#' @family KineticaConnection methods
#' @rdname dbDisconnect
#' @param conn an object of [KineticaConnection-class]
#' @param ... Other parameters passed on to methods.
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbDisconnect(con)
#'}
setMethod("dbDisconnect", "KineticaConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      warning("Connection already closed.", call. = TRUE)
    } else {
      if(length(ls(conn@results)>0)) {
        warning("Some results have not been cleared")
      }
      rm(list = ls(conn@results), envir = as.environment(conn@results), inherits = FALSE)
      rm(list = as.character(conn@ptr), envir = as.environment(conn@drv@connections), inherits = FALSE)
      gc()
    }
    invisible(TRUE)
})

#' dbExecute()
#'
#' Executes the statement, returning the number of affected rows or objects
#' @family KineticaConnection methods
#' @rdname dbExecute
#' @param conn an object of [KineticaConnection-class]
#' @param statement character
#' @param params a set of values
#' @param ... Other parameters passed on to methods.
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbCreateTable(con, "tableA", data.frame(a = 1))
#' dbExistsTable(con, "tableA")
#' # TRUE
#' dbExecute(con, "DROP TABLE tableA")
#' # 1
#' dbDisconnect(con)
#'}
setMethod("dbExecute", signature("KineticaConnection", "character"),
  function(conn, statement, ..., params = NULL) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    res <- dbSendStatement(conn = conn, statement = statement, params = params)

    row_count <- dbGetRowsAffected(res)
    dbClearResult(res)
    row_count
})



#' dbExistsTable()
#'
#' Checks if the table exists.
#' Provide either table name or collection name, not both
#' Kinetica DB table names are unique, different collections don't allow tables with the same name.
#' @family KineticaConnection methods
#' @rdname dbExistsTable
#' @param conn an object of [KineticaConnection-class]
#' @param name character string for table or collection name
#' @param ... Other parameters passed on to methods.
#' @return logical
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbCreateTable(con, "tableA", data.frame(a = 1))
#' dbExistsTable(con, "tableA")
#' # TRUE
#' dbExecute(con, "DROP TABLE tableA")
#' # 1
#' dbExistsTable(con, "tableA")
#' # FALSE
#' dbDisconnect(con)
#'}
setMethod("dbExistsTable", signature("KineticaConnection", "character"),
  function(conn, name, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    if (missing(name) || is.null(name) || is.na(name) || !is.character(name) || length(name) != 1 || nchar(name) < 1) {
      stop(paste("Invalid table name", name), call. = FALSE)
    }
    name <- dbUnquoteIdentifier(conn, name)
    return(has_table(conn, name))
  })


#' dbGetQuery()
#'
#' Executes the Query and returns the resulting data.frame of n rows (or less)
#' When n is not provided, returns all rows available
#' @family KineticaConnection methods
#' @rdname dbGetQuery
#' @param conn A [KineticaConnection-class] object
#' @param statement a character string containing SQL.
#' @param n Number of rows to fetch, default -1
#' @param ... Other parameters passed on to methods.
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbWriteTable(con, "tableA", data.frame(a = 1:40))
#' dbExistsTable(con, "tableA")
#' # TRUE
#' ds <- dbGetQuery(con, "tableA", 3)
#' show(ds)
#'     a
#'1   1
#'2   2
#'3   3
#' dbRemoveTable(con, "tableA")
#' dbDisconnect(con)
#'}
setMethod("dbGetQuery", signature("KineticaConnection", "character"),
  function(conn, statement, n = -1, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }

    if (.invalid_int(n)) {
      stop("Parameter n should be a whole positive number, -1L or Inf.", call. = FALSE)
    }
    res <- dbSendQuery(conn, statement)
    ds <- dbFetch(res, n)
    dbClearResult(res)
    ds
  })


#' dbIsValid()
#'
#' Checks if the connection is valid
#' RKinetica Connection is a configuration of url, username and password to establish
#' connection to Kinetica DB, configuration does not go stale when memory objects get swapped to file.
#' Thus RKinetica accepts stale connections as valid, and allows multiple results per connection.
#' @family KineticaConnection methods
#' @param dbObj A [KineticaConnection-class] object
#' @param ...  Other arguments omitted in generic signature
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbIsValid(con)
#' # TRUE
#' dbDisconnect(con)
#'}
setMethod("dbIsValid", signature("KineticaConnection"),
  function(dbObj, ...) {
    exists(as.character(dbObj@ptr), envir = as.environment(dbObj@drv@connections), inherits = FALSE)
  }
)


#' dbListFields()
#'
#' Lists field names of a given table
#' @family KineticaConnection methods
#' @rdname dbListFields
#' @param conn A [KineticaConnection-class] object
#' @param name a character string for table name
#' @param ... Other parameters passed on to methods.
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbWriteTable(con, "tableA", data.frame(a = 1:40))
#' dbExistsTable(con, "tableA")
#' # TRUE
#' fields <- dbListFields(con, "tableA")
#' fields
#' # "a"
#' dbRemoveTable(con, "tableA")
#' dbDisconnect(con)
#'}
setMethod("dbListFields", signature("KineticaConnection", "character"),
    function(conn, name, ...) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if(missing(name) || .invalid_character(name)) {
        stop(paste("Invalid table name", name), call. = FALSE)
      }
      if (!dbExistsTable(conn, name)) {
        stop(paste("Table", name, "does not exist"), call. = FALSE)
      }
      res <- dbSendQuery(conn, paste("SELECT * FROM ", dbQuoteIdentifier(conn, name), "LIMIT 0"))
      fields <- names(res@data)
      dbClearResult(res)
      fields
    })


#' dbListObjects()
#'
#' Lists database Objects available to the user, including tables, views and collections
#' @family KineticaConnection methods
#' @rdname dbListObjects
#' @param conn A [KineticaConnection-class] object
#' @param prefix character
#' @param ... Other parameters passed on to methods.
#' @return data.frame
#' @export
setMethod("dbListObjects", signature("KineticaConnection"),
      function(conn, prefix, ...) {
        if (!dbIsValid(conn)) {
          stop("Invalid Kinetica connection")
        }
        if (missing(prefix)) {
          prefix <- ""
        }
        show_objects(conn = conn, name = prefix)
      })


#' dbListResults()
#'
#' Lists all active results of the current connection
#' @family KineticaConnection methods
#' @rdname dbListResults
#' @param conn an object of [KineticaConnection-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbListResults", signature("KineticaConnection"),
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Connection", call. = FALSE)
    }
    for (name in ls(conn@results)) {
      if (!endsWith(name, "_pos")) {
        show(get(name, envir = as.environment(conn@results), inherits = FALSE))
      }
    }
})


#' dbListTables()
#'
#' Lists all database tables available to the user
#' @family KineticaConnection methods
#' @rdname dbListTables
#' @param conn an object of [KineticaConnection-class]
#' @param name character
#' @param ...  Other arguments omitted in generic signature
#' @export
setMethod("dbListTables", signature("KineticaConnection"),
    function(conn, name, ...) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if (missing(name)) {
        name <- ""
      }
      show_tables(conn = conn, name = name)
    })


#' dbReadTable()
#'
#' Reads the data of a given table into data.frame object,
#' clearing the KineticaResult object upon exit
#' @family KineticaConnection methods
#' @rdname dbReadTable
#' @param conn an object of [KineticaConnection-class]
#' @param name a character string table name
#' @param row.names a logical flag to create extra row_names column
#' @param check.names a logical flag to check names
#' @param ...  Other arguments omitted in generic signature
#' @export
setMethod("dbReadTable", "KineticaConnection",
  function(conn, name, ..., row.names = FALSE, check.names = TRUE) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    if (missing(name) || is.null(name) || is.na(name) || !is.character(name)
        || length(name) != 1 || nchar(name)<1) {
      stop("Invalid table name", call. = FALSE)
    }
    if (!dbExistsTable(conn, name)) {
      stop(paste("Table", name, "does not exist"), call. = FALSE)
    }
    if (!missing(row.names) && !is.null(row.names) && !is.na(row.names)) {
      if (!is.logical(row.names) && !is.character(row.names) || length(row.names)!= 1)
        stop("Invalid row.names parameter value, expected string, TRUE/FALSE or NULL value.", call. = FALSE)
    }
    if (!missing(check.names) && (.invalid_logical(check.names) || is.na(check.names))) {
      stop("Invalid check.names parameter value, expected TRUE/FALSE.", call. = FALSE)
    }
    ds <- dbGetQuery(conn, paste("SELECT * FROM ", dbQuoteIdentifier(conn, name)))
    ds <- DBI::sqlColumnToRownames(ds, row.names)
    if (check.names) {
      names(ds) <- make.names(names(ds), unique = TRUE)
    }

    ds
})

#' dbRemoveTable()
#'
#' Drops the table with the given name if one exists
#' @family KineticaConnection methods
#' @rdname dbRemoveTable
#' @param conn an object of [KineticaConnection-class]
#' @param name character string for table name
#' @param ...  Other arguments omitted in generic signature
#' @export
setMethod("dbRemoveTable",  signature("KineticaConnection", "character"),
  function(conn, name, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }

    if (missing(name) || length(name) != 1 || is.null(name) || is.na(name) || !is.character(name) || nchar(name) < 1) {
      stop("Invalid table name", call. = FALSE)
    }
    dbExecute(conn, paste("DROP TABLE IF EXISTS ", dbQuoteIdentifier(conn, name)))

    invisible(TRUE)
})



#' dbSendQuery()
#'
#' Executes SQL query and returns a result set. The returned result object should be read with dbFetch(result) and
#' then cleared with dbClearResult(result)
#' @family KineticaConnection methods
#' @rdname dbSendQuery
#' @param conn object [KineticaConnection-class]
#' @param statement character
#' @param params a list of query named parameters
#' @param ...  Other arguments omitted in generic signature
#' @export
setMethod("dbSendQuery", signature(conn ="KineticaConnection", statement = "character"),
    function(conn, statement, params = NULL, offset = NULL, limit = NULL, ...) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if (missing(statement) || is.null(statement) || is.na(statement)
	|| .invalid_character(statement) || length(statement) != 1) {
        stop("Invalid statement", call. = FALSE)
      }

      if (missing(params) || is.null(params) || is.na(params)) {
        prepare_mode <- grepl("?", statement, fixed = TRUE)
        execute_sql(conn = conn, statement = statement, offset = offset, limit = limit, data = params, prepare_mode = prepare_mode, no_return_data = FALSE)
      } else {
        res <- execute_sql(conn = conn, statement = statement, offset = offset, limit = limit, data = NULL, prepare_mode = FALSE, no_return_data = FALSE)
        dbBind(res, params)
      }
    }
)


#' dbSendStatement()
#'
#' Executes SQL statement that does not return a result set
#' @seealso [dbSendQuery()] [dbGetQuery()] [dbExecute()]
#' @rdname dbSendStatement
#' @family KineticaConnection methods
#' @param conn object [KineticaConnection-class]
#' @param statement character
#' @param params a list of query named parameters
#' @param ...  Other arguments omitted in generic signature
#' @export
setMethod("dbSendStatement", signature(conn ="KineticaConnection", statement = "character"),
    function(conn, statement, params = NULL, offset = NULL, limit = NULL, ...) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if (missing(statement) || is.null(statement) || is.na(statement)
	|| .invalid_character(statement) || length(statement) != 1) {
        stop("Invalid statement", call. = FALSE)
      }

      if (missing(params) || is.null(params) || is.na(params)) {
        prepare_mode <- grepl("?", statement, fixed = TRUE)
        execute_sql(conn = conn, statement = statement, offset = offset, limit = limit, data = params, prepare_mode = prepare_mode, no_return_data = TRUE)
      } else {
        res <- execute_sql(conn = conn, statement = statement, offset = offset, limit = limit, data = NULL, prepare_mode = FALSE, no_return_data = TRUE)
        dbBind(res, params)
      }

    }
)

#' dbWriteTable()
#'
#' Writes a dataset into Kinetica DB, appending, creating or overwriting a table
#' as indicated by flags.
#' @seealso [dbCreateTable()] [dbAppendTable()] [dbRemoveTable()]
#' @family KineticaConnection methods
#' @rdname dbWriteTable
#' @param conn an object of [KineticaConnection-class]
#' @param name character string for table name
#' @param value a [data.frame] (or object coercible to data.frame)
#' @param row.names a logical flag, NULL or chaacter value to create extra row_names column
#' @param overwrite a logical flag to overwrite existing table with new columns and values
#' @param append a logical flag to preserve existing data and append new records
#' @param field.types a named character vector of value field names and types
#' @param temporary a logical flag to create table as temporary storage
#' @param ...  Other arguments omitted in generic signature
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbWriteTable(con, "test", data.frame(a = 1L:3L, b = 2.1:4.2), row.names = FALSE)
#' dbDisconnect(con)
#' }
setMethod("dbWriteTable", signature("KineticaConnection"),
  function(conn, name, value, ..., row.names = FALSE,
           overwrite = FALSE, append = FALSE, field.types = NULL, temporary = FALSE) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }

    if (missing(name) || .invalid_character(name)) {
      stop("Invalid table name", call. = FALSE)
    }

    if (overwrite && append) {
      stop("Both overwrite and append can't be set to TRUE.", call. = FALSE)
    }

    if (!missing(overwrite) && (is.null(overwrite) || is.na(overwrite) || .invalid_logical(overwrite))) {
      stop("Invalid overwrite parameter value, expected TRUE/FALSE.", call. = FALSE)
    }

    if (!missing(append) && (is.null(append) || is.na(append) || .invalid_logical(append))) {
      stop("Invalid append parameter value, expected TRUE/FALSE.", call. = FALSE)
    }

    if (!missing(temporary) && (is.null(temporary) || is.na(temporary) || .invalid_logical(temporary))) {
      stop("Invalid temporary parameter value, expected TRUE/FALSE.", call. = FALSE)
    }

    if (!missing(row.names) && !is.null(row.names) && !is.na(row.names)) {
      if (!is.logical(row.names) && !is.character(row.names) || length(row.names)!= 1 )
        stop("Invalid row.names parameter value, expected string, TRUE/FALSE or NULL value.", call. = FALSE)
    }

    if (!missing(field.types) && .invalid_field_types(field.types)) {
      stop("Invalid or incompatible with existing table field.types parameter value, expected a named character list or NULL.", call. = FALSE)
    }

    tbl_exists <- dbExistsTable(conn, name)
    if (tbl_exists && !overwrite && !append) {
      stop(paste("Table", name, "exists, but neither APPEND or OVERWRITE flags are TRUE. "))
    }
    if(overwrite && tbl_exists) {
      dbRemoveTable(conn, name)
    }

    tbl_values <- sqlData(conn, value[, , drop = FALSE], row.names = row.names)

    # if (tbl_exists && !missing(value)) {
    #   # KineticaDB existing column names
    #   ex_fields <- dbListFields(conn, name)
    #   # value data.frame field names
    #   new_fields <- names(tbl_values)
    #
    #   if(length(ex_fields) < length(new_fields) || length(new_fields) != length(unique(new_fields)) ||
    #       !all(new_fields %in% ex_fields)){
    #     stop(paste("Table", name, "exists, its columns don't match the data to be appended. "), call. = FALSE)
    #   }
    # }

    if (!missing(field.types) && !missing(value)) {
      # field.types names
      ex_fields <- names(field.types)
      # value data.frame field names
      new_fields <- names(tbl_values)

      if(length(ex_fields) < length(new_fields)
         || length(ex_fields) != length(unique(new_fields))
         || !all(new_fields %in% ex_fields)) {
        stop(paste("Parameter names in field.types (", paste0(ex_fields, collapse=","),
                   ") and dataset names provided (", paste0(new_fields, collapse = ","), ") do not match."), call. = FALSE)
      }
    }


    if(!tbl_exists || overwrite) {
      sql <- sqlCreateTable(con = conn, table = name, values = tbl_values, fields = tbl_values, row.names = FALSE, temporary = temporary)
      dbExecute(conn = conn, statement = sql)
    }

    if(nrow(value) > 0) {
      sql <- sqlAppendTable(con = conn, table = name, values = tbl_values, row.names = FALSE)
      dbExecute(conn = conn, statement = sql)

    }
    invisible(TRUE)
  })

.invalid_int <- function(n = "numeric") {
    if (length(n) != 1 || !is.numeric(n) || (is.numeric(n) && !is.infinite(n) && n %% 1 != 0)
        || (is.numeric(n) && !is.infinite(n) && n %% 1 == 0 && n < 0 && n != -1 )) {
      TRUE
    } else {
      FALSE
    }
}

.invalid_logical <- function(x) {
  if (length(x) != 1 || is.character(x) || is.raw(x) || is.numeric(x) || is.integer(x)) {
    TRUE
  } else {
    FALSE
  }
}

.invalid_character <- function(name) {
  if (is.null(name) || is.na(name) || !is.character(name) || length(name) != 1 || nchar(name)<1)  {
    TRUE
  } else {
    FALSE
  }
}

.invalid_field_types <- function(fields) {
  if (is.null(fields)) {
    FALSE
  } else if (is.numeric(fields) || is.logical(fields) || is.raw(fields) || is.null(fields)
      || !is.null(names(fields)) && is.na(names(fields))
      || length(fields) != length(unique(names(fields))) ) {
    TRUE
  } else {
    FALSE
  }
}

#' dbBegin()
#'
#' Executes the statement, returning the number of affected rows or objects
#' @family KineticaConnection methods
#' @name transactions
#' @param conn an object of [KineticaConnection-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbBegin", "KineticaConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    if (exists("status", envir = as.environment(conn@transaction), inherits = FALSE)) {
      stop("Nested transactions are not supported", call. = FALSE)
    } else {
      conn@transaction[["status"]] <- "BEGIN"
    }

    invisible(TRUE)
  })

#' dbCommit()
#'
#' Executes the statement, returning the number of affected rows or objects
#' @family KineticaConnection methods
#' @rdname transactions
#' @param conn an object of [KineticaConnection-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbCommit", "KineticaConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }

    if (exists("status", envir = as.environment(conn@transaction), inherits = FALSE)
        && conn@transaction[["status"]] == "BEGIN") {
      conn@transaction[["status"]] <- "COMMIT"
      # transaction "comitted", object in memory is destroyed
      rm(list = "status", envir = as.environment(conn@transaction), inherits = FALSE)
    } else {
      stop("No transaction started, nothing to commit", call. = FALSE)
    }
    invisible(TRUE)
  })

#' dbRollback()
#'
#' Executes the statement, returning the number of affected rows or objects
#' @family KineticaConnection methods
#' @rdname transactions
#' @param conn an object of [KineticaConnection-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbRollback", "KineticaConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    if (exists("status", envir = as.environment(conn@transaction), inherits = FALSE)
        && conn@transaction[["status"]] == "BEGIN") {
      conn@transaction[["status"]] <- "ROLLBACK"
      # transaction "rolled back", object in memory is destroyed
      rm(list = "status", envir = as.environment(conn@transaction), inherits = FALSE)
    } else {
      stop("No transaction started, nothing to roll back", call. = FALSE)
    }

    invisible(TRUE)
  })

