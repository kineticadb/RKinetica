#' @include kineticaObject.R

## Constants
.KineticaPkgName <- "RKinetica"
.KineticaVersion <- getNamespaceVersion(.KineticaPkgName)


#' Class KineticaDriver
#'
#' This is the \pkg{RKinetica} implementation of base [DBIDriver-class] class
#' from \pkg{DBI}. Driver object is created with \code{Kinetica()} constructor, which
#' does not need arguments.
#' See "Usage" section for DBI methods of [DBIDriver-class] overriden by \pkg{RKinetica}.
#' KineticaDriver defines operations for validating connection configuration \code{dbCanConnect()},
#' creating connections \code{dbConnect()}, mapping data types \code{dbDataType()},
#' listing active connections \code{dbListConnections()}, destroying connections \code{dbDisconnect()},
#' etc.
#'
#' @keywords internal
#' @docType class
#' @name KineticaDriver-class
#' @aliases KineticaDriver,Kinetica
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


#' Kinetica()
#'
#' Default constructor for KineticaDriver class
#' @rdname KineticaDriver-class
#' @export
#' @seealso dbConnect,dbCanConnect
#' @examples
#' \dontrun{
#' drv <- RKinetica::Kinetica()
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

#' @rdname KineticaDriver-class
#' @export
Kinetica()



#' dbUnloadDriver()
#'
#' Unload RKinetica Driver, releasing memory from Driver connections and embedded results
#' @rdname dbUnloadDriver
#' @family KineticaDriver methods
#' @param drv Kinetica driver object
#' @param ... Other arguments omitted in generic signature
#' @export
setMethod("dbUnloadDriver", "KineticaDriver", function(drv, ...) {
  if(length(ls(drv@connections)>0)) {
    rm(list = ls(drv@connections), envir = as.environment(drv@connections), inherits = FALSE)
  }
  rm(drv)
  gc()
  invisible(TRUE)
})


#' dbCanConnect()
#'
#' Method allows to check if provided Kinetica Connection configuration could be used to connect
#' to Kinetica DB instance successfully. It takes all the arguments of dbConnect signature,
#' creates a connection and destroys it upon exit.
#' @rdname dbCanConnect
#' @family KineticaDriver methods
#' @param drv Kinetica driver object
#' @param ... Other arguments omitted in generic signature
#' @export
#' @examples
#' \dontrun{
#' # If you have a Kinetica DB instance running in local docker image,
#' # the minimum set of parameters to connect is url
#' dbCanConnect(Kinetica(), url = "http://localhost:9191")
#' # TRUE
#' # You can also use host and port combination
#' dbCanConnect(Kinetica(), host = "127.0.0.1", port = 9191)
#' # TRUE
#' # Remote instances would also require username and password.
#' dbCanConnect(Kinetica(), host = "127.0.0.1", port = 9191,
#'     username = "username", password = "Pa$$w0rd")
#' # TRUE
#' # You can obscure username and password values by calling RStudio secure dialogue box:
#' dbCanConnect(Kinetica(), host = "127.0.0.1", port = 9191,
#'              username = rstudioapi::askForPassword("Kinetica DB user"),
#'              password = rstudioapi::askForPassword("Kinetica DB password"))
#' # TRUE
#'}
setMethod("dbCanConnect", "KineticaDriver",
  function(drv, ...) {
    tryCatch(
      {
        con <- dbConnect(drv, ...)
        dbDisconnect(con)
        TRUE
      },
      error = function(e) {
        structure (FALSE, reason = conditionMessage(e))
      })
  })

#' dbGetInfo()
#'
#' Retrieves brief KineticaDriver description:
#' dbname, driver version, client version, max number of connections
#' (NA stands for unlimited)
#' @rdname dbGetInfo
#' @family KineticaDriver methods
#' @param dbObj an object derived of [KineticaDriver-class] type
#' @param ... Other arguments omitted in generic signature
#' @export
#' @examples
#' \donttest{
#' drv <- Kinetica()
#' dbGetInfo(drv)
#' # name
#' #  "KineticaDriver"
#' # dbname
#' #  "Kinetica"
#' # driver.version
#' #  "7.1.0.0"
#' # client.version
#' #  "7.1.0.0"
#' # max.connections
#' #  NA
#'}
setMethod("dbGetInfo", "KineticaDriver",
  function(dbObj, ...) {
    list(name = "KineticaDriver",
         dbname =  "Kinetica",
         driver.version = dbObj@driver.version,
         client.version = dbObj@client.version,
         max.connections = NA )
  }
)


#' dbIsValid()
#'
#' Checks if the provided Driver object is valid
#' @family KineticaDriver methods
#' @rdname dbIsValid
#' @param dbObj an object of [KineticaDriver-class] type
#' @param ... Other arguments omitted in generic signature
#' @export
#' @examples
#' \dontrun{
#' drv <- Kinetica()
#' dbIsValid(drv)
#' #  TRUE
#'}
setMethod("dbIsValid", signature("KineticaDriver"),
  function(dbObj, ...) TRUE
)

#' show()
#'
#' RKinetica Driver short printout string
#' @family KineticaDriver methods
#' @rdname show
#' @export
#' @examples
#' \dontrun{
#' drv <- Kinetica()
#' show(drv)
#' #  KineticaDriver 7.1.0.0
#' }
setMethod("show", "KineticaDriver", function(object) {
  cat("<", is(object)[1], " ", .KineticaVersion, ">\n", sep = "")
})


#' dbListConnections()
#'
#' Lists all active connections in KineticaDriver's connection pool
#' invoking show() on KineticaConnection objects
#' @family KineticaDriver methods
#' @rdname dbListConnections
#' @param drv an object of [KineticaDriver-class] type
#' @param ... Other arguments omitted in generic signature
#' @export
setMethod("dbListConnections", "KineticaDriver", function(drv, ...) {
  if (!dbIsValid(drv)) {
    stop("Driver is invalid", call. = FALSE)
  }
  ls(drv@connections)
})


#' dbDataType()
#'
#' Takes an R object or literal and returns a matching SQLtype
#' used when creating Kinetica DB table value to store it
#' @family KineticaDriver methods
#' @param dbObj KineticaDriver object
#' @param obj any data object or literal
#' @param ... Other arguments omitted in generic signature
#' @importFrom bit64 integer64 is.integer64
#' @export
#' @examples
#' \dontrun{
#' dbDataType(Kinetica(), 1L)
#'  "INTEGER"
#' dbDataType(Kinetica(), 1)
#'  "DOUBLE"
#' dbDataType(Kinetica(), "Test string")
#'  "VARCHAR(256)"
#' dbDataType(Kinetica(), Sys.Date())
#'  "DATE"
#' dbDataType(Kinetica(), list())
#'  "BINARY"
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


#' dbConnect()
#'
#' Connect to a Kinetica DB going through the appropriate authentication procedure.
#' You can have multiple connections open and connections can have multiple results attached.
#' This function may be invoked repeatedly assigning its output to different
#' objects.
#' Authentication can be provided by using username/password variables.
#' row_limit param would limit  the number of rows in query results,
#' it's an integer value allowing up to 2,147,483,647 rows. Value literal
#' must be followed by L, as in row_limit = 1000000L, also see example below.
#' Use [dbCanConnect()] to check if a connection can be established.
#' @family KineticaDriver methods
#' @rdname dbConnect
#' @param drv Kinetica driver object
#' @param host character string for Kinetica DB host address
#' @param port integer value for Kinetica DB port
#' @param url character string for Kinetica DB url (protocol + host + port)
#' @param username character string for Kinetica DB username
#' @param password character string for Kinetica DB password
#' @param timeout  integer value for Kinetica DB connection timeout
#' @param ... Other arguments omitted in generic signature
#' @export
#' @examples
#' \dontrun{
#' # If you jave a Kinetica DB instance running in local docker image,
#' # the minimum set of parameters to connect is url
#' dbConnect(Kinetica(), url = "http://localhost:9191")
#' # You can also use host and port combination
#' dbConnect(Kinetica(), host = "127.0.0.1", port = 9191)
#' # Remote instances would also require username and password.
#' dbConnect(Kinetica(), host = "127.0.0.1", port = 9191,
#'   username = "username", password = "Pa$$w0rd", row_limit = 1000000L)
#' con <- dbConnect(Kinetica(), url = "http://localhost:9191")
#' show(con)
#' # KineticaConnection
#' # url: http://localhost:9191
#' dbDisconnect(con)
#'}
setMethod("dbConnect", "KineticaDriver",
  function(drv, host = NULL, port = NULL, url = NULL, username = NULL, password = NULL, timeout = NULL,
           row_limit = NULL, ha_ring = NULL, ...) {
    username <- ifelse(is.null(username), "", username)
    password <- ifelse(is.null(password), "", password)
    timeout <- ifelse(is.null(timeout), 0L, as.integer(timeout))
    row_limit <- ifelse((missing(row_limit) || is.null(row_limit) || !is.numeric(row_limit)),
                          10000L, as.integer(row_limit))

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

    # verify that Kinetica instance is running and connection can be established
    # on error, user would get a detailed error message from HTTP response
    system_properties <- show_system_properties(url, username, password, ha_ring)
    core_version <- system_properties$version.gpudb_core_version
    version <- paste0(strsplit(core_version, "[.]")[[1]][1:4], collapse= ".")
    if (startsWith(version, "6.") || startsWith(version, "5.")) {
      stop(paste0("Unsupported Kinetica DB version ", version, ". RKinetica expects DB version 7.0 or higher."), call. = FALSE)
    }

    ha_ptr <- new.env()
    ha_enabled <- as.logical(system_properties$conf.enable_ha)
    # if flag value is NA, convert it to FALSE
    ha_enabled <- ifelse(is.na(ha_enabled), FALSE, ha_enabled)

    # set boolean flag and label for self-provided HA ring
    if (!is.na(ha_ring) && !is.null(ha_ring) && ha_ring != "") {
      ha_enabled = TRUE
      ha_ptr[["label"]] <- "Self-provided HA"
      ha_ptr[["current"]] <- 1
    } else {
      ha_ring <- system_properties$conf.ha_ring_head_nodes
      # set boolean flag and label for system-provided HA ring
      if (!is.na(ha_ring) && !is.null(ha_ring) && ha_ring != "") {
        ha_ptr[["label"]] <- "HA"
        ha_ptr[["current"]] <- 1
      }
    }

    # permute the HA ring arbitrarily
    ha_ring <- .permute_ha_ring(ha_ring)
    # find position of primary url in the HA ring
    primary_url_idx <- match(url, ha_ring)
    if (is.na(primary_url_idx)) {
      primary_url_idx <- length(ha_ring) + 1
      ha_ring[[primary_url_idx]] <- url
    }
    # set pointer to position of primary url in the HA ring
    ha_ptr[["current"]] <- primary_url_idx

    # get user's default schema
    default_schema <- get_default_schema(url, username, password)

    # create a new connection object
    ptr <- sha1_hash(paste0(url, username, Sys.time()), key = "Kinetica")
    results <- new.env()
    transaction <- new.env()
    conn <- new ("KineticaConnection", drv = drv, host = host, port = port, url = url,
                 username = username, password = password, timeout = timeout, ptr = ptr,
                 db.version = version, results = results, transaction = transaction,
                 row_limit = row_limit, ha_enabled = ha_enabled, ha_ring = ha_ring,
                 ha_ptr = ha_ptr, default_schema = default_schema, ...)
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

.permute_ha_ring <- function(uris) {
  # Split the string of uris on comma and convert it to vector
  uris <- unlist(strsplit(uris, ","))
  # check that the value is in fact a non-empty vector
  # with more than one member
  if (is.vector(uris) && length(uris)>0) {
    # randomly permute vector values
    sample(uris, length(uris))
  } else {
    # return original object if it's empty
    uris
  }
}
