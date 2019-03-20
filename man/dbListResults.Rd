% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaConnection.R
\docType{methods}
\name{dbListResults,KineticaConnection-method}
\alias{dbListResults,KineticaConnection-method}
\title{dbListResults()}
\usage{
\S4method{dbListResults}{KineticaConnection}(conn, ...)
}
\arguments{
\item{conn}{an object of [KineticaConnection-class]}

\item{...}{Other parameters passed on to methods.}
}
\description{
Lists all active results of the current connection
}
\seealso{
Other KineticaConnection methods: \code{\link{dbAppendTable,KineticaConnection-method}},
  \code{\link{dbCreateTable,KineticaConnection-method}},
  \code{\link{dbDataType,KineticaConnection-method}},
  \code{\link{dbDisconnect,KineticaConnection-method}},
  \code{\link{dbExecute,KineticaConnection,character-method}},
  \code{\link{dbExistsTable,KineticaConnection,character-method}},
  \code{\link{dbGetInfo}},
  \code{\link{dbGetQuery,KineticaConnection,character-method}},
  \code{\link{dbIsValid,KineticaConnection-method}},
  \code{\link{dbListFields,KineticaConnection,character-method}},
  \code{\link{dbListObjects,KineticaConnection-method}},
  \code{\link{dbListTables,KineticaConnection-method}},
  \code{\link{dbReadTable,KineticaConnection,character-method}},
  \code{\link{dbRemoveTable,KineticaConnection,character-method}},
  \code{\link{dbSendQuery,KineticaConnection,character-method}},
  \code{\link{dbSendStatement,KineticaConnection,character-method}},
  \code{\link{dbWriteTable,KineticaConnection,character-method}},
  \code{\link{show,KineticaDriver-method}},
  \code{\link{sqlCreateTable,KineticaConnection-method}}
}
\concept{KineticaConnection methods}