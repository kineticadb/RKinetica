% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaResult.R
\docType{methods}
\name{dbGetRowsAffected,KineticaResult-method}
\alias{dbGetRowsAffected,KineticaResult-method}
\title{dbGetRowsAffected()}
\usage{
\S4method{dbGetRowsAffected}{KineticaResult}(res, ...)
}
\arguments{
\item{res}{an object of [KineticaResult-class]}

\item{...}{Other parameters passed on to methods.}
}
\description{
Returns the number of rows affected by the executed SQL statement
}
\seealso{
Other KineticaResult methods: \code{\link{dbBind,KineticaResult-method}},
  \code{\link{dbClearResult,KineticaResult-method}},
  \code{\link{dbColumnInfo}}, \code{\link{dbFetch}},
  \code{\link{dbGetInfo}},
  \code{\link{dbGetRowCount,KineticaResult-method}},
  \code{\link{dbGetStatement,KineticaResult-method}},
  \code{\link{dbHasCompleted,KineticaResult-method}},
  \code{\link{dbIsValid}},
  \code{\link{show,KineticaDriver-method}}
}
\concept{KineticaResult methods}
