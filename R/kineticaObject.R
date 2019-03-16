#' Class KineticaObject
#' @export
#' @name KineticaObject-class
setClass("KineticaObject", representation("DBIObject", "VIRTUAL"))
#> <KineticaObject>

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

#' @param dbObj An object inheriting from [KineticaObject-class],
#'  i.e. [KineticaDriver-class], [KineticaConnection-class],
#'  or a [KineticaResult-class]
#' @param ... Other arguments to methods.
#' @return logical
#' @export
setGeneric("dbIsReadOnly",
           def = function(dbObj, ...) standardGeneric("dbIsReadOnly"),
           valueClass = "logical")

#' @param dbObj An object inheriting from [KineticaObject-class],
#'  i.e. [KineticaDriver-class], [KineticaConnection-class],
#'  or a [KineticaResult-class]
#' @param ... Other arguments to methods.
#' @return a named list
#' @export
setGeneric("dbGetInfo",
           def = function(dbObj, ...) standardGeneric("dbGetInfo")
)

#' @export
setGeneric("summary")

#' @param dbObj An object inheriting from [KineticaObject-class],
#'  i.e. [KineticaDriver-class], [KineticaConnection-class],
#'  or a [KineticaResult-class]
#' @param ... Other arguments to methods.
#' @export
setMethod("summary", "KineticaObject", function(object, ...) {
  info <- dbGetInfo(dbObj = object, ...)
  cat(class(object), "\n")
  invisible(info)
})
