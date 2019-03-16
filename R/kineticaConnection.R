#' @include kineticaObject.R
#' @include kineticaDriver.R
#' @include connect.R
#' @include kineticaSQL.R

#' Class: KineticaConnection
#' @keywords internal
#' @import DBI
#' @import methods
#' @export
#' @name KineticaConnection-class
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
           results = "environment"
         )
)
#> <KineticaConnection>

#' Minimal KineticaConnection string representation
#' Provides url of the Kinetica DB instance
#' @param object [KineticaConnection-class]
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(d, url = "http://localhost:9191")
#' show(con)
#'> <KineticaConnection>
#'>   url: http://localhost:9191
#'}
setMethod("show", "KineticaConnection", function(object) {
  if (!dbIsValid(object)) {
    stop("Invalid Kinetica Connection", call. = FALSE)
  }
  cat("<", is(object)[1], ">\n", sep = "")
  cat("url:", object@url, "\n"

  )
})


#' Basic properties of KineticaConnection object
#' provides DB version, url, host, port and username of the current connection
#' @param dbObj object [KineticaConnection-class]
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(d, url = "http://localhost:9191")
#'> $db.version
#'> [1] "7.0.0.0"
#'> $dbname
#'> [1] "Kinetica"
#'> $url
#'> [1] "http://localhost:9191"
#'> $host
#'> [1] "localhost"
#'> $port
#'> [1] 9191
#'> $username
#'> [1] ""
#'}
setMethod("dbGetInfo", "KineticaConnection",
  function(dbObj, ...) {
    if (!dbIsValid(dbObj)) {
      stop("Invalid connection", call. = FALSE)
    }
    list(name = "KineticaConnection",
         db.version = dbObj@drv@driver.version,
         dbname = dbObj@drv@dbname,
         url =  dbObj@url,
         host = dbObj@host,
         port = dbObj@port,
         username = dbObj@username )
  }
)


#' @inheritParams sqlCreateTable
#' @inheritParams rownames
#' @param values A data frame. Factors will be converted to character vectors.
#' @export
setGeneric("dbAppendTable",
    def = function(conn, name, value, ..., row.names = NULL) standardGeneric("dbAppendTable"))

#' @export
setMethod("dbAppendTable", "KineticaConnection",
  function(conn, name, value, ..., row.names = NULL) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
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

    dbExecute(conn, query, params = unname(as.list(value)))
  })

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
#' @param temporary If `TRUE`, will generate a temporary table statement.
#' @inheritParams rownames
#' @param ... Other arguments used by individual methods.
#' @export
setGeneric("sqlCreateTable",
  def = function(con, table, fields, row.names = NA, temporary = FALSE, ...) standardGeneric("sqlCreateTable")
)

#' Customised version of the generic sqlCreateTable method
#' (Kinetica DB uses TEMP keyword instead of TEMPORARY)
#' @rdname hidden_aliases
#' @param row.names a flag with logical, character or NULL value
#' @param temporary logical TRUE/FALSE for temporary/permanent table created
#' @export
setMethod("sqlCreateTable", signature("KineticaConnection"),
  function(con, table, fields, row.names = NA, temporary = FALSE, ...) {
    if (missing(row.names)) {
      warning("Do not rely on the default value of the row.names argument for sqlCreateTable(), it will change in the future.",
              call. = FALSE
      )
      row.names = NULL
    }

    table <- dbQuoteIdentifier(con, table)

    if (is.data.frame(fields)) {
      fields <- sqlRownamesToColumn(fields, row.names)
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


#' @param conn an object of [KineticaConnection-class]
#' @param name table name
#' @param row.names Must be `NULL`.
#' @inheritParams sqlCreateTable
#' @inheritParams dbDisconnect
# @param fields either a named character verctor or a data frame
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbCreateTable(con, "tableA", data.frame(a = 1))
#' dbExistsTable(con, "tableA")
#'> [1] TRUE
#' dbDisconnect(con)
#'}
setGeneric("dbCreateTable",
  def = function(conn, name, fields, ..., row.names = NULL, temporary = FALSE) standardGeneric("dbCreateTable"))

#' @param conn an object of [KineticaConnection-class]
#' @param name table name
#' @param fields either a named character vector or a data frame
#' @export
setMethod ("dbCreateTable", "KineticaConnection",
   function(conn, name, fields, ..., row.names = NULL, temporary = FALSE) {
     if (!dbIsValid(conn)) {
       stop("Invalid Kinetica Connection", call. = FALSE)
     }
     if (!(is.null(row.names) || is.logical(row.names) || is.character(row.names)) ) {
       stop("Invalid row names ", call. = FALSE)
     }
     if(!is.logical(temporary) || !length(temporary) == 1L) {
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

#' Redirect to driver dbDataType method for data type mapping of provided object
#' @param obj generic object
#' @export
setMethod("dbDataType", signature("KineticaConnection", "ANY"),
    function(dbObj, obj, ...) {
      if (!dbIsValid(dbObj)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      dbDataType(dbObj@drv, obj)
})


#' Disconnects the current connection, releasing memory from all uncleared result objects
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

#' Executes the statement, returning the number of affected rows or objects
#' @param statement character
#' @param params a set of values
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbCreateTable(con, "tableA", data.frame(a = 1))
#' dbExistsTable(con, "tableA")
#'> [1] TRUE
#' dbExecute(con, "DROP TABLE tableA")
#'> [1] 1
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


#' @param conn an object of [KineticaConnection-class]
#' @param name table or collection name
#' @return logical
#' @export
setGeneric("dbExistsTable",
   def = function(conn, name, ...) standardGeneric("dbExistsTable"),
   valueClass = "logical"
)

#' Checks if the table exists.
#' Provide either table name or collection name, not both
#' Kinetica DB table names are unique, different collections don't allow tables with the same name.
#' @param name character
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbCreateTable(con, "tableA", data.frame(a = 1))
#' dbExistsTable(con, "tableA")
#'> [1] TRUE
#' dbExecute(con, "DROP TABLE tableA")
#'> [1] 1
#' dbExistsTable(con, "tableA")
#'> [1] FALSE
#' dbDisconnect(con)
#'}
setMethod("dbExistsTable", signature("KineticaConnection", "character"),
  function(conn, name, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    return(has_table(conn, name))
  })

#' @param conn A [KineticaConnection-class] object
#' @param statement a character string containing SQL.
#' @param ... Other parameters passed on to methods.
#' @export
setGeneric("dbGetQuery",
  def = function(conn, statement, ...) standardGeneric("dbGetQuery")
)

#' Executes the Query and returns the resulting data.frame of n rows (or less)
#' When n is not provided, returns all rows available
#' @param n Number of rows to fetch, default -1
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbWriteTable(con, "tableA", data.frame(a = 1:40))
#' dbExistsTable(con, "tableA")
#> [1] TRUE
#' ds <- dbGetQuery(con, "tableA", 3)
#' show(ds)
#>     a
#>1   1
#>2   2
#>3   3
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


#' Checks if the connection is valid
#' RKinetica Connection is a configuration of url, username and password to establish
#' connection to Kinetica DB, configuration does not go stale when memory objects get swapped to file.
#' Thus RKinetica accepts stale connections as valid, and allows multiple results per connection.
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbIsValid(con)
#> [1] TRUE
#' dbDisconnect(con)
#'}
setMethod("dbIsValid", signature("KineticaConnection"),
  function(dbObj, ...) {
    exists(as.character(dbObj@ptr), envir = as.environment(dbObj@drv@connections), inherits = FALSE)
  }
)

#' @param conn A [KineticaConnection-class] object
#' @param name a character string table name
#' @param ... Other parameters passed on to methods.
#' @export
setGeneric("dbListFields",
   def = function(conn, name, ...) standardGeneric("dbListFields"),
   valueClass = "character"
)


#' @param name a character string table name
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' dbWriteTable(con, "tableA", data.frame(a = 1:40))
#' dbExistsTable(con, "tableA")
#'> [1] TRUE
#' fields <- dbListFields(con, "tableA")
#' fields
#'> [1] "a"
#' dbRemoveTable(con, "tableA")
#' dbDisconnect(con)
#'}
setMethod("dbListFields", signature("KineticaConnection", "character"),
    function(conn, name, ...) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if (missing(name) || !is.character(name) || length(name) != 1 || nchar(name)<1) {
        stop("Invalid table name", call. = FALSE)
      }
      if (!dbExistsTable(conn, name)) {
        stop(paste("Table", name, "does not exist"), call. = FALSE)
      }
      res <- dbSendQuery(conn, paste("SELECT * FROM ", dbQuoteIdentifier(conn, name), "LIMIT 0"))
      fields <- names(res@data)
      dbClearResult(res)
      fields
    })


#' #' @param conn A [KineticaConnection-class] object
#' #' @param ... Other parameters passed on to methods.
#' #' @export
#' setGeneric("dbListObjects",
#'     def = function(conn, ...) standardGeneric("dbListObjects"), valueClass = "data.frame"
#' )

#' @param prefix character
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

#' @param conn an object of [KineticaConnection-class]
#' @param ... Other parameters passed on to methods.
#' @export
setGeneric("dbListResults",
           def = function(conn, ...) standardGeneric("dbListResults"))

#' Lists all active results in the current connection
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

#' #' @export
#' setGeneric("dbListTables",
#'   def = function(conn, ...) standardGeneric("dbListTables"),
#'            valueClass = "character")

# @param conn an object of [KineticaConnection-class]
#' @param name character
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

#' @param conn an object of [KineticaConnection-class]
#' @param name a character string table name
#' @export
setGeneric("dbReadTable",
     def = function(conn, name, ...) standardGeneric("dbReadTable"),
     valueClass = "data.frame")

#' @param row.names a logical flag to create extra row_names column
#' @param check.names a logical flag to check names
#' @export
setMethod("dbReadTable", signature("KineticaConnection", "character"),
    function(conn, name, ..., row.names = FALSE, check.names) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if (missing(name) || !is.character(name) || length(name) != 1 || nchar(name)<1) {
        stop("Invalid table name", call. = FALSE)
      }
      if (!dbExistsTable(conn, name)) {
        stop(paste("Table", name, "does not exist"), call. = FALSE)
      }
      if (!missing(row.names) && !is.null(row.names) && .invalid_logical(row.names)) {
        stop("Invalid row.names parameter value, expected TRUE/FALSE or NULL.", call. = FALSE)
      }
      if (!missing(check.names) && (.invalid_logical(check.names) || is.na(check.names))) {
        stop("Invalid check.names parameter value, expected TRUE/FALSE.", call. = FALSE)
      }
      res <- dbSendQuery(conn, paste("SELECT * FROM ", dbQuoteIdentifier(conn, name)))
      ds <- dbFetch(res)
      dbClearResult(res)
      ds
    })


#' @param name table name
#' @export
setMethod("dbRemoveTable",  signature("KineticaConnection", "character"),
  function(conn, name, ...) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }

    if (missing(name) || length(name) != 1 || !is.character(name) || nchar(name) < 1) {
      stop("Invalid table name", call. = FALSE)
    }
    res <- dbSendQuery(conn, paste("DROP TABLE IF EXISTS ", dbQuoteIdentifier(conn, name)))
    on.exit(dbClearResult(res))
    invisible(TRUE)
})



# @param conn object [KineticaConnection-class]
#' @param statement character
#' @export
setMethod("dbSendQuery", signature(conn ="KineticaConnection", statement = "character"),
    function(conn, statement, ...) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if (is.na(statement) || is.null(statement)) {
        stop("Invalid statement", call. = FALSE)
      }
      execute_sql(conn = conn, statement = statement, no_return_statement = FALSE)
    }
)


# @param conn object [KineticaConnection-class]
#' @param statement character
#' @param params a list of query named parameters
#' @export
setMethod("dbSendStatement", signature(conn ="KineticaConnection", statement = "character"),
    function(conn, statement, params = NULL, ...) {
      if (!dbIsValid(conn)) {
        stop("Invalid Kinetica Connection", call. = FALSE)
      }
      if (is.na(statement) || is.null(statement)) {
        stop("Invalid statement", call. = FALSE)
      }
      if (!is.null(params)) {
        res <- new ("KineticaResult", connection = conn, statement = statement,
                    data = data.frame(), fields = character(0), count_affected = -1, total_number_of_records = -1, has_more_records = FALSE)
        dbBind(res, params)
      } else {
        execute_sql(conn = conn, statement = statement, no_return_statement = TRUE)
      }
    }
)

#' @inheritParams dbGetQuery
#' @inheritParams dbReadTable
#' @param value a [data.frame] (or coercible to data.frame).
#' @export
setGeneric("dbWriteTable",
           function(conn, name, value, ...) standardGeneric("dbWriteTable"))
#' @param row.names a logical flag to create extra row_names column
#' @param overwrite a logical flag to overwrite existing table with new columns and values
#' @param append a logical flag to preserve existing data and append new records
#' @param field.types a named character vector of value field names and types
#' @temporary a flag to use created table for temporary storage and use during the lifetime of current connection
#' @export
setMethod("dbWriteTable", signature("KineticaConnection", "character"),
  function(conn, name, value, ..., row.names = FALSE, overwrite = FALSE, append = FALSE, field.types = NULL, temporary = FALSE) {
    if (!dbIsValid(conn)) {
      stop("Invalid Kinetica Connection", call. = FALSE)
    }
    if (missing(name) || .invalid_character(name)) {
      stop("Invalid table name", call. = FALSE)
    }
    if (overwrite && append) {
      stop("Both overwrite and append can't be set to TRUE.", call. = FALSE)
    }
    if (!missing(overwrite) && .invalid_logical(overwrite)) {
      stop("Invalid overwrite parameter value, expected TRUE/FALSE.", call. = FALSE)
    }
    if (!missing(append) && .invalid_logical(append)) {
      stop("Invalid append parameter value, expected TRUE/FALSE.", call. = FALSE)
    }
    if (!missing(temporary) && .invalid_logical(temporary)) {
      stop("Invalid temporary parameter value, expected TRUE/FALSE.", call. = FALSE)
    }
    if (!missing(row.names) && !is.null(row.names) && .invalid_logical(row.names)) {
      stop("Invalid row.names parameter value, expected TRUE/FALSE or NULL.", call. = FALSE)
    }
    if (!missing(field.types) && .invalid_field_types(field.types)) {
      stop("Invalid field.types parameter value, expected a named character list or NULL.", call. = FALSE)
    }
    if(is.logical(row.names) || is.null(row.names)) {
      row.names <- NULL
    }

    tbl_exists <- dbExistsTable(conn, name)
    if(overwrite && tbl_exists) {
      dbRemoveTable(conn, name)
    }

    tbl_values <- sqlData(conn, value[, , drop = FALSE], row.names = row.names)
    if (!tbl_exists || overwrite) {
      dbCreateTable(conn = conn, name = name, fields = tbl_values, row.names = FALSE, temporary = temporary)
    }

    if(nrow(value) > 0) {
      name <- dbQuoteIdentifier(conn, name)
      fields <- dbQuoteIdentifier(conn, names(tbl_values))
      params <- rep("?", length(fields))

      sql <- paste0(
        "INSERT INTO ", name, " (", paste0(fields, collapse = ", "), ")\n",
        "VALUES (", paste0(params, collapse = ", "), ")"
      )
      res <- dbExecute(conn = conn, statement = sql, params = tbl_values)

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
  if (length(name) != 1 || !is.character(name) || is.na(name) || is.null(name) || length(name) !=1)  {
    TRUE
  } else {
    FALSE
  }
}

.invalid_field_types <- function(fields) {
  if (is.numeric(fields) || is.logical(fields) || is.raw(fields) || is.na(fields)
      || is.null(names(fields)) || is.na(names(fields))) { # unique(unlist(x, use.names = FALSE))
    TRUE
  } else {
    FALSE
  }
}
