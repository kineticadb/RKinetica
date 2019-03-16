## Driver for Kinetica DB.
##

#' @include connect.R
#' @include kineticaObject.R
## Constants
.KineticaPkgName <- "RKinetica"
.KineticaVersion <- "7.0.0.0"


#' KineticaDriver class
#'
#' Kinetica DB implementation of base DBIDriver class (DBI package).
#' Defines operations for creating connections, mapping data types,
#' validating connection configuration, etc.
#'
#' @docType class
#' @name KineticaDriver-class
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
setClass("KineticaDriver",
        contains = c("DBIDriver", "KineticaObject"),
        slots = list(
          dbname = "character",
          driver.version = "character",
          client.version = "character",
          connections = "environment")
)
#> <KineticaDriver>

#' Kinetica Driver constructor
#' @rdname KineticaDriver-class
#' @export
#' @examples
#' \dontrun{
#' drv <- RKinetica::Kinetica()
#' # or
#' d <- Kinetica()
#'}
Kinetica <- function() {
  connections <- new.env()
  new ("KineticaDriver",
       dbname =  "Kinetica",
       driver.version = .KineticaVersion,
       client.version = .KineticaVersion,
       connections = connections)
}
#> no output


#' Unload RKinetica Driver by releasing memory from Driver connections and embedded results
#' @param drv Kinetica driver object
#' @export
#' @rdname KineticaDriver-class
setMethod("dbUnloadDriver", "KineticaDriver", function(drv, ...) {
  if(length(ls(drv@connections)>0)) {
    rm(list = ls(drv@connections), envir = as.environment(drv@connections), inherits = FALSE)
  }
  rm(drv)
  gc()
  invisible(TRUE)
})
#> [1] "dbUnloadDriver"

#' Method allows to check if provided Kinetica Connection configuration could be used to connect
#' to Kinetica DB instance successfully. It takes all the arguments of dbConnect signature,
#' creates a connection and destroys it upon exit.
#' @param drv Kinetica driver object
#' @param ... Other parameters passed on to methods.
#' @export
#' @rdname Kinetica-class
setGeneric("dbCanConnect", def = function(drv, ...) standardGeneric("dbCanConnect"),
           valueClass = "logical")
#> [1] "dbCanConnect"

#' @param host Kinetica DB host address
#' @param port Kinetica DB port
#' @param url Kinetica DB url (host+port)
#' @param username DB username
#' @param password DB password
#' @param timeout  DB connection timeout
#' @export
#' @examples
#' \dontrun{
#' # If you jave a Kinetica DB instance running in local docker image, the minimum set of parameters
#' # to connect is url
#' dbCanConnect(Kinetica(), url = "http://localhost:9191")
#' # You can also use host and port combination
#' dbCanConnect(Kinetica(), host = "127.0.0.1", port = 9191)
#' # Remote instances would also require username and password.
#' dbCanConnect(Kinetica(), host = "127.0.0.1", port = 9191, username = "username", password = "Pa$$w0rd")
#'}
setMethod("dbCanConnect", "KineticaDriver",
  function(drv, host = NULL, port = NULL, url = NULL, username = NULL, password = NULL, timeout = NULL, ...) {
    tryCatch(
      {
        con <- dbConnect(drv, host = host, port = port, url = url, username = username, password = password, timeout = timeout, ...)
        dbDisconnect(con)
        TRUE
      },
      error = function(e) {
        structure (FALSE, reason = conditionMessage(e))
      })
  })
#> [1] TRUE

#' Retrieves brief Driver information, dbname and driver/client version
#' @param dbObj an object derived of [KineticaDriver-class] type
#' @export
#' @examples
#' \dontrun{
#' # Retrieves brief Driver information
#' drv <- Kinetica()
#' dbGetInfo(drv)
#'}
setMethod("dbGetInfo", "KineticaDriver",
  function(dbObj) {
    list(name = "KineticaDriver",
         dbname =  "Kinetica",
         driver.version = dbObj@driver.version,
         client.version = dbObj@client.version,
         max.connections = NA )
  }
)
#> $name
#> [1] "KineticaDriver"
#> $dbname
#> [1] "Kinetica"
#> $driver.version
#> [1] "7.0.0.0"
#> $client.version
#> [1] "7.0.0.0"
#> $max.connections
#> [1] NA

#' Checks if the provided Driver object is valid
#' @param dbObj an object of [KineticaDriver-class] type
#' @export
#' @examples
#' \dontrun{
#' drv <- Kinetica()
#' dbIsValid(drv)
#'}
setMethod("dbIsValid", signature("KineticaDriver"),
  function(dbObj, ...) TRUE
)
#> [1] TRUE

#' Show - RKinetica Driver display string
#' @param object Kinetica Driver
#' @export
setMethod("show", "KineticaDriver", function(object) {
  cat("<", is(object)[1], " ", .KineticaVersion, ">\n", sep = "")
})
#> [1] "show"


#' Lists all active connections in this Driver's connection pool
#' invoking show() on connection details
#' @export
setMethod("dbListConnections", "KineticaDriver", function(drv, ...) {
  if (!dbIsValid(drv)) {
    stop("Driver is invalid", call. = FALSE)
  }
  ls(drv@connections)
})

#' @param dbObj an object of [KineticaDriver-class] type
#' @param obj any ODBC object that can be saved in Kinetica DB
#' @param ... Other parameters passed on to methods.
#' @export
setGeneric("dbDataType",
   def = function(dbObj, obj, ...) standardGeneric("dbDataType"),
   valueClass = "character"
)

#' Returns a matching SQLtype used when creating Kinetica DB object for the R data object
#' @param obj data object
#' @importFrom bit64 integer64 is.integer64
#' @export
#' @examples
#' \dontrun{
#' dbDataType(Kinetica(), 1L)
#'> [1] "INTEGER"
#' dbDataType(Kinetica(), 1)
#'> [1] "DOUBLE"
#' dbDataType(Kinetica(), "Test string")
#'> [1] "VARCHAR(256)"
#' dbDataType(Kinetica(), Sys.Date())
#'> [1] "DATE"
#' dbDataType(Kinetica(), list())
#'> [1] "BINARY"
#'}
setMethod("dbDataType", "KineticaDriver", function(dbObj, obj, ...) {

  if (is.null(obj)) {
    stop("NULL is not a valid type", call. = FALSE)
  }
  if (is.factor(obj)) return("VARCHAR(256)")
  if (is.data.frame(obj)) return(callNextMethod(dbObj, obj))
  #   # return(vapply(x, dbDataType, FUN.VALUE = character(1), USE.NAMES = TRUE))
  if (is.integer64(obj)) return("LONG")

  if (class(obj)[[1]] == "AsIs") {
    oldClass(obj) <- oldClass(obj)[-1]
    return (dbDataType(dbObj, obj))
  }

  if (class(obj)[[1]] == "Date") return("DATE")
  if (class(obj)[[1]] %in% c("POSIXct", "POSIXt")) return("TIMESTAMP")
  if (class(obj)[[1]] %in% c("hms", "difftime")) return("TIME")

  if (is.integer(obj)) return("INTEGER")
  if (is.logical(obj)) return("TINYINT")
  if (is.raw(obj)) return("BYTES")
  if (is.list(obj)) return("BINARY")

  switch(typeof(obj),
         integer = "INTEGER",
         double = "DOUBLE",
         character = "VARCHAR(256)",
         logical = "TINYINT",
         list = "BINARY",
         raw = "BYTES",
         warning("Unsupported type", call. = FALSE)
  )
})
#> [1] "dbDataType"

#' @export
Kinetica()
#> <KineticaDriver>

#' Connect to a Kinetica DB going through the appropriate authentication procedure.
#' You can have multiple connections open and connections can have multiple results attached.
#' This function may be invoked repeatedly assigning its output to different
#' objects.
#' Authentication can be provided by using username/password variables.
#' Use [dbCanConnect()] to check if a connection can be established.
#' @rdname KineticaDriver-class
#' @param host Kinetica DB host address
#' @param port Kinetica DB port
#' @param url Kinetica DB url (host+port)
#' @param username DB username
#' @param password DB password
#' @param timeout  DB connection timeout
#' @export
#' @examples
#' \dontrun{
#' # If you jave a Kinetica DB instance running in local docker image, the minimum set of parameters
#' # to connect is url
#' dbConnect(Kinetica(), url = "http://localhost:9191")
#' # You can also use host and port combination
#' dbConnect(Kinetica(), host = "127.0.0.1", port = 9191)
#' # Remote instances would also require username and password.
#' dbConnect(Kinetica(), host = "127.0.0.1", port = 9191, username = "username", password = "Pa$$w0rd")
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' show(con)
#'> <KineticaConnection>
#'>   url: http://localhost:9191
#'}
setMethod("dbConnect", "KineticaDriver",
  function(drv, host = NULL, port = NULL, url = NULL, username = NULL, password = NULL, timeout = NULL, ...) {
    username <- ifelse(is.null(username), "", username)
    password <- ifelse(is.null(password), "", password)
    timeout <- ifelse(is.null(timeout), 0L, timeout)

    if (!is.null(host) && !nchar(host)==0) {
      # format url
      url <- .make_url(host = host, port = port)
    } else if (!is.null(url) && !nchar(url)==0) {
      # extract host and port from url
      obj <- .validate_url(url)
      host <- as.character(obj["host"])
      port <- as.integer(obj["port"])
      url <- as.character(obj["url"])
    } else {
      stop("Unsupported Connection configuration, please supply url string or host and port parameters.", call. = FALSE)
    }

    # create a new connection
    ptr <- sha1_hash(paste0(url, username, Sys.time()), key = "Kinetica")
    results <- new.env()
    conn <- new ("KineticaConnection", drv = drv, host = host, port = port, url = url, username = username, password = password, timeout = timeout, ptr = ptr, results = results, ...)
    drv@connections[[conn@ptr]] <- conn

    return (conn)
  }
)

# internal utility functions not exposed to user
.make_url <- function(host = "character", port = "ANY") {
  if (is.null(port) || !is.numeric(port) || port == 0L) {
    port <- 9191L
  }
  url <- paste0("http://", host, ":", port)
}

.validate_url <- function(.url = "character") {
  # remove trailing "/"
  if (endsWith(.url, "/")) {
    len <- nchar(.url)
    .url <- substr(.url, 1, len-1)
  }

  #check protocol
  parts <- strsplit(.url, "://")
  if (length(parts[[1]]) > 1) {
    #extract protocol
    protocol <- parts[[1]][1]
    remainder <- parts[[1]][2]

    if(!startsWith(protocol, "http")) {
      warning(paste("Protocol ", protocol, "is not valid, replacing with default HTTP protocol"))
      protocol <- "http"
    }
  } else {
    warning(paste("No protocol provided, using default HTTP protocol"))
    # no protocol
    protocol <- "http"
    remainder <- .url
  }

  tails <- strsplit(remainder, "/")
  head <- tails[[1]][1]
  # check if port exists by splitting url on ":"
  heads <- strsplit(head, ":")
  if (length(heads[[1]]) == 1) {
    warning(paste("No port provided, using default port 9191"))
    # no port idetified
    host <- head
    port <- "9191"
  } else {
    # host comes before ":"
    host <- heads[[1]][1]
    port <- heads[[1]][2]
  }
  if (length(tails[[1]])>1) {
    # breadcrumbs
    breadcrumbs <- tails[[1]][-1]
    tail_path <- paste0(breadcrumbs, collapse = "/")
    url <- paste0(protocol, "://", host, ":", port, "/", tail_path)
  } else {
    # no breadcrumbs
    url <- paste0(protocol, "://", host, ":", port)
  }
  list(url = url, host = host, port = port)
}


