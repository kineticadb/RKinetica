% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaConnection.R
\docType{methods}
\name{dbListTables,KineticaConnection-method}
\alias{dbListTables,KineticaConnection-method}
\title{dbListTables()}
\usage{
\S4method{dbListTables}{KineticaConnection}(conn, name, ...)
}
\arguments{
\item{conn}{an object of [KineticaConnection-class]}

\item{name}{character}

\item{...}{Other arguments omitted in generic signature}
}
\description{
Lists all database tables available to the user
}
\seealso{
Other KineticaConnection methods: \code{\link{dbAppendTable}},
  \code{\link{dbCreateTable}},
  \code{\link{dbDataType,KineticaConnection-method}},
  \code{\link{dbDisconnect,KineticaConnection-method}},
  \code{\link{dbExecute,KineticaConnection,character-method}},
  \code{\link{dbExistsTable}}, \code{\link{dbGetInfo}},
  \code{\link{dbGetQuery,KineticaConnection,character-method}},
  \code{\link{dbIsValid}}, \code{\link{dbListFields}},
  \code{\link{dbListObjects,KineticaConnection-method}},
  \code{\link{dbListResults,KineticaConnection-method}},
  \code{\link{dbReadTable}}, \code{\link{dbRemoveTable}},
  \code{\link{dbSendQuery,KineticaConnection,character-method}},
  \code{\link{dbSendStatement,KineticaConnection,character-method}},
  \code{\link{dbWriteTable}},
  \code{\link{show,KineticaDriver-method}},
  \code{\link{sqlCreateTable,KineticaConnection-method}},
  \code{\link{transactions}}
}
\concept{KineticaConnection methods}
