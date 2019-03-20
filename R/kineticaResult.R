#' @include kineticaConnection.R

#' Class KineticaResult
#'
#' A virtual class representing the dataset returned from Kinetica DB
#' and all its relevant properties.
#'
#' @keywords internal
#' @aliases KineticaResult
#' @import methods
#' @docType class
#' @slot connection an object derived from [KineticaConnection-class]
#' @slot statement character for well-formed SQL statement
#' @slot data object data.frame
#' @slot fields character named list of column data types
#' @slot count_affected numeric
#' @slot total_number_of_records numeric
#' @slot has_more_records logical
#' @slot ref character reference pointer for result
#' @export
setClass("KineticaResult",
         contains = c("DBIResult", "KineticaObject"),
         slots = list(
           connection = "KineticaConnection",
           statement = "character",
           data = "data.frame",
           fields = "character",
           count_affected = "numeric",
           total_number_of_records = "numeric",
           has_more_records = "logical",
           ref = "character"
      )
)

KineticaResult <- function(connection, statement, data, fields, count_affected, total_number_of_records, has_more_records) {
  ref <- sha1_hash(paste0(connection@ptr, statement, Sys.time()), key = "Kinetica")
  obj <- new ("KineticaResult", connection = connection, statement = statement, data = data, fields = fields, count_affected = count_affected,
                                        total_number_of_records = total_number_of_records, has_more_records = has_more_records, ref = ref)
  connection@results[[ref]] <- obj
  connection@results[[paste0(ref,"_pos")]] <- 1

  obj
}

#' show()
#'
#' Brief string printout of [KineticaResult-class] object properties
#' @family KineticaResult methods
#' @rdname show
#' @param object [KineticaResult-class]
#' @export
setMethod("show", "KineticaResult", function(object) {
  if(!dbIsValid(object)) {
    stop("This result set has been cleared already")
  }
  cat("<", is(object)[1], ">\n", sep = "")
  cat("KineticaConnection url:", object@connection@url, "\n",
      "statement: ", object@statement,"\n",
      "dataset with", length(object@fields), "columns and", object@total_number_of_records,  "rows\n",
      "columns: \n", paste(names(object@fields), object@fields, sep = ":", collapse = ", "),"\n"
  )
})

#' dbIsValid()
#'
#' Checks if the KineticaResult object has been cleared
#' @family KineticaResult methods
#' @rdname dbIsValid
#' @param dbObj [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbIsValid", "KineticaResult", function(dbObj, ...) {
  # check if exists in connection results environment
  exists(as.character(dbObj@ref), envir = as.environment(dbObj@connection@results), inherits = FALSE)
})

#' dbGetInfo()
#'
#' Provides basic info on KineticaResult object
#' @family KineticaResult methods
#' @rdname dbGetInfo
#' @param dbObj [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbGetInfo", signature("KineticaResult"), function(dbObj, ...) {
  if (!dbIsValid(dbObj)) {
    warning("This result has been cleared already", call. = FALSE)
  }
  list(
    statement = dbGetStatement(dbObj),
    row.count = dbGetRowCount(dbObj),
    rows.affected = dbGetRowsAffected(dbObj),
    has.completed = dbHasCompleted(dbObj)
  )
})

#' dbClearResult()
#'
#' Clears result, if exists, releasing memory in Connection results environment
#' @family KineticaResult methods
#' @rdname dbClearResult
#' @param res [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbClearResult", "KineticaResult", function(res, ...) {
  # free resources
  if (!dbIsValid(res)) {
    warning("This result has been cleared already", call. = FALSE)
  } else {
    pos_ref <- paste0(res@ref, "_pos")
    rm(list = as.character(res@ref), envir = as.environment(res@connection@results), inherits = FALSE)
    rm(list = as.character(pos_ref), envir = as.environment(res@connection@results), inherits = FALSE)
    rm(pos_ref)
    gc()
  }
  invisible(TRUE)
})



#' dbFetch()
#'
#' Fetches the provided number of rows from Result. If n is not provided,
#' returns all available records
#' @family KineticaResult methods
#' @rdname dbFetch
#' @param res An object inheriting from [KineticaResult-class]
#' @param n maximum number of records to retrieve per fetch. Use `n = -1`
#'   or `n = Inf`
#'   to retrieve all pending records.  Some implementations may recognize other
#'   special values.
#' @param ... Other arguments passed on to methods.
#' @export
setGeneric("dbFetch",
           def = function(res, n = -1, ...) standardGeneric("dbFetch"),
           valueClass = "data.frame"
)

#' dbFetch()
#'
#' Fetches the provided number of rows from Result. If n is not provided,
#' returns all available records
#' @family KineticaResult methods
#' @rdname dbFetch
#' @param res An object inheriting from [KineticaResult-class]
#' @param n maximum number of records to retrieve per fetch. Use `n = -1`
#'   or `n = Inf`
#'   to retrieve all pending records.  Some implementations may recognize other
#'   special values.
#' @param ... Other arguments passed on to methods.
#' @export
#' @export
setMethod("dbFetch", signature("KineticaResult"), function(res, n = -1, ...) {
  if (!dbIsValid(res)) {
    stop("This result has been cleared already", call. = FALSE)
  }
  if (dbGetRowsAffected(res) > 0 || dbGetRowCount(res) == -1) {
    warning("This query is not expected to return rows.", call. = FALSE)
    return (data.frame())
  }
  if (!missing(n)) {
    if (length(n) != 1) {
      stop("n should be a scalar number", call. = FALSE)
    }
    if (!is.numeric(n)) {
      stop("n should be a whole positive number, -1L or Infinity.", call. = FALSE)
    }
    if (is.numeric(n) && !is.infinite(n) && n %% 1 != 0) {
      stop("n should be a whole positive number, -1L or Infinity.", call. = FALSE)
    }
    if (is.numeric(n) && !is.infinite(n) && n %% 1 == 0 && n < 0 && n != -1 ) {
      stop("n should be a whole positive number, -1L or Infinity.", call. = FALSE)
    }
    if (is.numeric(n) && !is.infinite(n) && n %% 1 == 0) {
      n <- as.integer(n)
    }

  }
  ptr_pos_ref <- paste0(res@ref, "_pos")
  if (exists(as.character(ptr_pos_ref), envir = as.environment(res@connection@results), inherits = FALSE)) {
    start_pos <- as.numeric(res@connection@results[[ptr_pos_ref]])
  } else {
    start_pos <- 1
  }

  if (missing(n) || is.infinite(n) || n == -1 || (start_pos + n - 1) > res@total_number_of_records){
    end_pos <- res@total_number_of_records
  } else {
    end_pos <- (start_pos + n - 1)
  }
  res@connection@results[[ptr_pos_ref]] <- (end_pos + 1)
  if (start_pos > end_pos) {
    result <- as.data.frame(res@data[0,], row_names = integer(0L), optional = FALSE, col.names = names(res@fields))
  } else if (start_pos == end_pos){
    result <- res@data[start_pos,]
  } else {
    result <- res@data[start_pos:end_pos,]
  }

  if (!is.data.frame(result)) {
    result <- as.data.frame(result, row_names = integer(0), optional = FALSE, col.names = names(res@fields))
  }
  if (length(result) == length(res@fields)) {
    names(result) <- names(res@fields)
  # } else {
  #   print("Inconsistency with field list and data.frame")
  #   print(result)
  #   print(res@fields)
  }
  # if (end_pos == res@total_number_of_records) {
  #   dbClearResult(res)
  # }
  return(result)

})

#' dbColumnInfo()
#'
#' Provides data.frame as a collection of column names and data types
#' from the result set
#' @family KineticaResult methods
#' @rdname dbColumnInfo
#' @param res An object inheriting from [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setGeneric("dbColumnInfo",
   def = function(res, ...) standardGeneric("dbColumnInfo"),
   valueClass = "data.frame"
)

#' dbColumnInfo()
#'
#' Provides data.frame as a collection of column names and data types
#' from the result set
#' @family KineticaResult methods
#' @rdname dbColumnInfo
#' @param res An object inheriting from [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbColumnInfo", "KineticaResult",
  function(res, ...) {
    if(!dbIsValid(res)) {
      stop("This result set has been cleared already")
    }
    data.frame(name = as.character(names(res@fields)), type = unname(res@fields))
  })


#' dbHasCompleted()
#'
#' Returns a logical value whether the result has completed
#' @family KineticaResult methods
#' @rdname dbHasCompleted
#' @param res an object of [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbHasCompleted", "KineticaResult", function(res, ...) {
  if(!dbIsValid(res)) {
    stop("This result set has been cleared already")
  }
  if (res@total_number_of_records == -1) {
    return(TRUE)
  }

  ptr_pos_ref <- paste0(res@ref, "_pos")
  if (exists(as.character(ptr_pos_ref), envir = as.environment(res@connection@results), inherits = FALSE)) {
    start_pos <- as.numeric(res@connection@results[[ptr_pos_ref]])
    on.exit(rm(start_pos))
    if (res@total_number_of_records > 0 && start_pos == 1) {
      FALSE
    } else {
      (start_pos > res@total_number_of_records)
    }
  }
})


#' dbGetRowsAffected()
#'
#' Returns the number of rows affected by the executed SQL statement
#' @family KineticaResult methods
#' @rdname dbGetRowsAffected
#' @param res an object of [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbGetRowsAffected", "KineticaResult", function(res, ...) {
  if (!dbIsValid(res)) {
    stop("This result set has been cleared already")
  }
  if (res@count_affected != -1) {
    res@count_affected
  } else {
    0L
  }

})

#' dbGetRowCount()
#'
#' Returns the number of rows in the returned data set
#' @family KineticaResult methods
#' @rdname dbGetRowCount
#' @export
#' @param res KineticaResult-class
#' @param ... Other parameters passed on to methods.
setMethod("dbGetRowCount", "KineticaResult", function(res, ...) {
  if (!dbIsValid(res)) {
    stop("This result set has been cleared already")
  }
  if (res@total_number_of_records == -1) {
    return(0L)
  }
  ptr_pos_ref <- paste0(res@ref, "_pos")
  if (exists(as.character(ptr_pos_ref), envir = as.environment(res@connection@results), inherits = FALSE)) {
    start_pos <- as.numeric(res@connection@results[[ptr_pos_ref]])
    on.exit(rm(start_pos))
    if (start_pos == 1) {
      return(0)
    } else {
      return (start_pos - 1)
    }
  } else {
    return(0L)
  }
#  res@total_number_of_records
})

#' dbGetStatement()
#'
#' Returns the SQL statement executed to get this result set
#' @family KineticaResult methods
#' @rdname dbGetStatement
#' @param res an object of [KineticaResult-class]
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbGetStatement", "KineticaResult", function(res, ...) {
  if (!dbIsValid(res)) {
    stop("This result set has been cleared already")
  }
  res@statement
})


#' dbBind()
#'
#' Binds the prepared statement to parameter values provided
#' @family KineticaResult methods
#' @rdname dbBind
#' @param res an object of [KineticaResult-class]
#' @param params A list of bindings, named or unnamed.
#' @param ... Other parameters passed on to methods.
#' @export
setMethod("dbBind", "KineticaResult",
  function(res, params, ...) {
    db_bind(res, params, ...)
  })


db_bind <- function(res, params, ...) {
  connection <- res@connection
  statement <- res@statement
  keep <- strsplit(statement, "VALUES")[[1]][1]

  recs <- list()
  for (i in 1:nrow(params)) {
    recs[i] <- paste("(",paste(params[i,], collapse = ","), ")", collapse = ",")
  }
  statement <- paste(keep, "VALUES", paste(recs, collapse = ","))

  new_res <- execute_sql(conn = connection, statement = statement, no_return_statement = TRUE)
}

