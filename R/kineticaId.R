#' @include kineticaObject.R
NULL

#' @rdname KineticaId
setClass("KineticaId",
	contains = c("Id", "KineticaObject"),
	slots = list(name = "character"))

#' Refer to a table nested in a hierarchy (e.g. within a schema)
#'
#' Objects of class `KineticaId` have a single slot `name`, which is a named
#' character vector.
#' The [dbQuoteIdentifier()] method converts `KineticaId` objects to strings.
#' Support for `KineticaId` objects depends on the database backend.
#' They can be used in the following methods as `name` argument:
#' - [dbCreateTable()]
#' - [dbAppendTable()]
#' - [dbReadTable()]
#' - [dbWriteTable()]
#' - [dbExistsTable()]
#' - [dbRemoveTable()]
#' Objects of this class are also returned from [dbListObjects()].
#'
#' @param ... Components of the hierarchy, e.g. `schema`, `table`,
#'   or `cluster`, `catalog`, `schema`, `table`.
#'   For more on these concepts, see
#'   \url{http://stackoverflow.com/questions/7022755/}
#' @export
#' @examples
#' KineticaId(schema = "dbo", table = "Customer")
#' dbQuoteIdentifier(Kinetica(), KineticaId(schema = "ki_home", table = "nyctaxi"))
#' KineticaId(schema = "myschema", table = "mytable")
KineticaId <- function(...) {
  components <- c(...)
  if (is.null(names(components)) && (length(components) == 1)) {
    # When components are not named and contain a single character string,
    # split it into schema and table names to create KineticaId named properties
    components <- unlist(strsplit(components[1], "[.]"))

    if (length(components) == 2) {
      # string contains schema and table parts that need labeling
      names(components) <- c("schema", "table")
    } else {
      # no schema provided, don't add "schema" label
      names(components) <- c("table")
    }
  }
  if (is.null(names(components)) || any(names(components) == "") || any(is.na(names(components)))) {
    # Throws an exception if there are parts of table name that are not labeled
    # because Kinetica DB does not allow more than 1 period in table name
    stop(paste("Invalid Kinetica table name provided: ", components), call. = FALSE)
  }
  # If no edge cases detected, allow all valid named part to pass through
  # to keep code consistent with DBI::Id() implementation
  new("KineticaId", name = components)
}

#' @rdname hidden_aliases
#' @param object KineticaId object to print
#' @export
setMethod("show", signature("KineticaId"), function(object) {
  cat(toString(object), "\n", sep = "")
})


#' @rdname hidden_aliases
#' @param object a [KineticaId-class] object
#' @export
setMethod("dbIsValid", signature("KineticaId"),
  function(dbObj, ...) {
    if (class(dbObj) == "KineticaId") {
      TRUE
    } else {
      FALSE
    }
})

#' @export
toString.KineticaId <- function(x, ...) {
  paste0("<KineticaId> ", paste0(names(x@name), " = ", x@name, collapse = ", "))
}
