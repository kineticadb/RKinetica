#' Class KineticaObject
#'
#' Base DB object extending [DBI::DBIObject].
#' @export
#' @docType class
#' @aliases KineticaObject
setClass("KineticaObject", representation("DBIObject", "VIRTUAL"))


#' dbIsValid()
#'
#' Generic dbIsValid() implementation.
#' @param dbObj An object inheriting from [KineticaObject-class],
#'  i.e. [KineticaDriver-class], [KineticaConnection-class],
#'  or a [KineticaResult-class]
#' @param ... Other arguments to methods.
#' @return logical
#' @export
setGeneric("dbIsValid",
           def = function(dbObj, ...) standardGeneric("dbIsValid"),
           valueClass = "logical"
)

#' dbIsReadOnly()
#'
#' Generic dbIsReadOnly() implementation.
#' @param dbObj An object inheriting from [KineticaObject-class]
#' @param ... Other arguments to methods.
#' @return logical
#' @export
setGeneric("dbIsReadOnly",
           def = function(dbObj, ...) standardGeneric("dbIsReadOnly"),
           valueClass = "logical")

#' dbGetInfo()
#'
#' Generic dbGetInfo() implementation.
#' @param dbObj An object inheriting from [KineticaObject-class]
#' @param ... Other arguments to methods.
#' @return a named list
#' @export
setGeneric("dbGetInfo",
           def = function(dbObj, ...) standardGeneric("dbGetInfo")
)

#' summary()
#'
#' Generic summary() implementation.
#' @export
setGeneric("summary")

#' summary()
#'
#' Implementation of summary() for KineticaObject signature.
#' @param object An object inheriting from [KineticaObject-class]
#' @param ... Other arguments to methods.
#' @export
setMethod("summary", "KineticaObject", function(object, ...) {
  info <- dbGetInfo(dbObj = object, ...)
  cat(class(object), "\n")
  invisible(info)
})
