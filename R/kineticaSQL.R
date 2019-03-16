#' @include kineticaObject.R
#' @include kineticaDriver.R
#' @include kineticaConnection.R
#' @include kineticaResult.R

#' @param x A character vector to label as being escaped SQL.
#' @param ... Other arguments passed on to methods. Not otherwise used.
#' @param names Names for the returned object, must have the same length as `x`.
#' @return An object of class `SQL`.
#' @export
SQL <- function(x, ..., names = NULL) {
  if (!is.null(names)) {
    stopifnot(length(x) == length(names))
  }
  names(x) <- names
  new("SQL", x)
}

#' @export
#' @aliases
#'   SQL-class
setClass("SQL", contains = "character")

#' @export
setMethod("show", "SQL", function(object) {
  cat(paste0("<SQL> ", object, collapse = "\n"), "\n", sep = "")
})

#' @export
`[.SQL` <- function(x, ...) SQL(NextMethod())

#' @export
`[[.SQL` <- function(x, ...) SQL(NextMethod())

#' @export
#' @param conn object KineticaConnection-class
#' @param x a character vector or SQL object
setGeneric("dbQuoteIdentifier",
           def = function(conn, x, ...) standardGeneric("dbQuoteIdentifier")
)

# quote_idstr <-
#   function(x) {
#     options(useFancyQuotes = FALSE)
#     sapply(lapply(strsplit(enc2utf8(x), "[.]"), dQuote), paste, collapse = ".")
#   }

quote_identifier <-
  function(conn, x, ...) {

    if (is(x, "SQL")) return(x)
    # if (is(x, "Id")) {
    #   return(SQL(paste0(dbQuoteIdentifier(conn, x@name), collapse = ".")))
    # }
    if (!is.character(x)) stop("x must be character or SQL", call. = FALSE)

    if (any(is.na(x))) {
      stop("Cannot pass NA to dbQuoteIdentifier()", call. = FALSE)
    }
    x <- gsub('"', '""', enc2utf8(x))

    if (length(x) == 0L) {
      SQL(character(), names = names(x))
    } else {
      # Not calling encodeString() here to keep things simple
      SQL(paste('"', x, '"', sep = ""), names = names(x))
    }
}
#' @rdname hidden_aliases
#' @export
setMethod("dbQuoteIdentifier", signature("KineticaConnection"), quote_identifier)

#' @rdname hidden_aliases
#' @export
#' @param conn object [KineticaConnection-class]
#' @param x a character vector or SQL object
setMethod("dbQuoteIdentifier", signature("KineticaConnection", "character"),
          function(conn, x, ...) quote_identifier(conn = conn, x = x, ...)
)

#' @rdname hidden_aliases
#' @export
#' @param conn object [KineticaConnection-class]
#' @param x a character vector or SQL object
setMethod("dbQuoteIdentifier", signature("KineticaConnection", "SQL"),
          function(conn, x, ...) quote_identifier(conn = conn, x = x, ...)
)


#' @export
setGeneric("dbUnquoteIdentifier",
           def = function(conn, x, ...) standardGeneric("dbUnquoteIdentifier")
)

#' @param conn A subclass of [KineticaConnection-class], representing
#'   an active connection to an DBMS.
#' @param x An [SQL] object or character vector to unquote.
#' @param ... Other arguments passed on to methods.
#' @export
setMethod("dbUnquoteIdentifier", signature("KineticaConnection"), function(conn, x, ...) {
  if (is(x, "SQL")) {
    rx <- '^(?:(?:|"((?:[^"]|"")+)"[.])(?:|"((?:[^"]|"")+)"[.])(?:|"((?:[^"]|"")*)")|([^". ]+))$'
    bad <- grep(rx, x, invert = TRUE)
    if (length(bad) > 0) {
      stop("Can't unquote ", x[bad[[1]]], call. = FALSE)
    }
    catalog <- gsub(rx, "\\1", x)
    catalog <- gsub('""', '"', catalog)
    schema <- gsub(rx, "\\2", x)
    schema <- gsub('""', '"', schema)
    table <- gsub(rx, "\\3", x)
    table <- gsub('""', '"', table)
    naked_table <- gsub(rx, "\\4", x)

    ret <- Map(catalog, schema, table, naked_table, f = as_table)
    names(ret) <- names(x)
    return(ret)
  }
  if (is(x, "Id")) {
    return(list(x))
  }
  stop("x must be SQL or Id", call. = FALSE)
})

as_table <- function(catalog, schema, table, naked_table = NULL) {
  args <- c(catalog = catalog, schema = schema, table = table, table = naked_table)
  # Also omits NA args
  args <- args[!is.na(args) & args != ""]
  do.call(Id, as.list(args))
}

#' @param conn A subclass of [KineticaConnection-class], representing
#'   an active connection to an DBMS.
#' @param x A character vector to quote as string.
#' @param ... Other arguments passed on to methods.
#' @export
setGeneric("dbQuoteString",
           def = function(conn, x, ...) standardGeneric("dbQuoteString")
)

#' # quote_string <-
#' #   function(conn, x, ...) {
#' # #    if (is(x, "SQL")) return(x)
#' #     if (!is.character(x)) stop("x must be character or SQL", call. = FALSE)
#' #     x <- gsub("'", "''")
#' #     str <- paste("'", x, "'", sep = "")
#' # }
quote_string <-
  function(conn, x, ...) {
    if (is(x, "SQL")) return(x)
    if (!is.character(x)) stop("x must be character or SQL", call. = FALSE)

    # Avoid fixed = TRUE due to https://github.com/r-dbi/DBItest/issues/156
    x <- gsub("'", "''", enc2utf8(x))

    if (length(x) == 0L) {
      SQL(character())
    } else {
      # Not calling encodeString() here, see also http://stackoverflow.com/a/549244/946850
      str <- paste("'", x, "'", sep = "")
      str[is.na(x)] <- "NULL"
      SQL(str)
    }
  }


#' @export
setMethod("dbQuoteString", signature("KineticaConnection"), quote_string)

#' @export
setMethod("dbQuoteString", signature("KineticaConnection", "character"), quote_string)

#' @export
setMethod("dbQuoteString", signature("KineticaConnection", "SQL"), quote_string)

#' @param x A vector to quote as string.
#' @export
setMethod("dbQuoteLiteral", signature("KineticaConnection"),
          function(conn, x, ...) {
            # Switchpatching to avoid ambiguous S4 dispatch, so that our method
            # is used only if no alternatives are available.

            if (is(x, "SQL")) return(x)

            if (is.factor(x)) return(dbQuoteString(conn, as.character(x)))

            if (is.character(x)) return(quote_string(conn, x))

            if (inherits(x, "POSIXt")) {
              return(dbQuoteString(
                conn,
                strftime(as.POSIXct(x), "%Y%m%d%H%M%S", tz = "UTC")
              ))
            }

            if (inherits(x, "Date")) return(dbQuoteString(conn, as.character(x, usetz = TRUE)))

            if (is.list(x)) {
              blob_data <- vapply(
                x,
                function(x) {
                  if (is.null(x)) {
                    "NULL"
                  } else if (is.raw(x)) {
                    paste0("X'", paste(format(x), collapse = ""), "'")
                  } else {
                    stop("Lists must contain raw vectors or NULL", call. = FALSE)
                  }
                },
                character(1)
              )
              return(SQL(blob_data, names = names(x)))
            }

            if (is.logical(x)) x <- as.numeric(x)
            x <- as.character(x)
            x[is.na(x)] <- "NULL"
            SQL(x, names = names(x))
          }
)

#' @inheritParams sqlCreateTable
#' @inheritParams rownames
#' @param value A data frame
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' sqlData(con, head(iris))
#' sqlData(con, head(mtcars))
#'
#' dbDisconnect(con)
#'}
setGeneric("sqlData",
           def = function(con, value, row.names = NA, ...) standardGeneric("sqlData")
)

#' @export
setMethod("sqlData", signature("KineticaConnection"), function(con, value, row.names = NA, ...) {
  value <- sqlRownamesToColumn(value, row.names)

  # Convert factors to strings
  is_factor <- vapply(value, is.factor, logical(1))
  value[is_factor] <- lapply(value[is_factor], as.character)

  # Quote all strings
  is_char <- vapply(value, is.character, logical(1))
  value[is_char] <- lapply(value[is_char], function(x) {
    enc2utf8(dbQuoteString(con, x))
  })

  # Convert everything to character and turn NAs into NULL
#  value[] <- lapply(value, as.character)
  value[is.na(value)] <- "NULL"

  value
})


coerce <- function(sqlvar, from, to) {
  list(from = from, to = to)
}

sqlDate <- function() {
  coerce(
    function(x) as.integer(x),
    function(x) {
      # stopifnot(is.integer(x))
      # structure(x, class = "Date")
      as.Date(x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"), origin="1900-01-01", optional = FALSE)
    }
  )
}

sqlDateTime <- function() {
  coerce(
    function(x) as.numeric(x),
    function(x) {
      stopifnot(is.numeric(x))
      structure(x, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    }
  )
}

sqlSerialize <- function() {
  coerce(
    function(x) {
      lapply(x, serialize, connection = NULL)
    },
    function(x) {
      lapply(x, unserialize)
    }
  )
}

sqlBoolean <- function() {
  coerce(
    function(x) as.integer(x),
    function(x) as.logical(x)
  )
}

sqlFactor <- function(levels) {
  coerce(
    function(x) as.integer(x),
    function(x) factor(x, levels = levels)
  )
}

