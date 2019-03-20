% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaConnection.R
\docType{methods}
\name{dbSendStatement,KineticaConnection,character-method}
\alias{dbSendStatement,KineticaConnection,character-method}
\title{dbSendStatement()}
\usage{
\S4method{dbSendStatement}{KineticaConnection,character}(conn, statement,
  params = NULL, ...)
}
\arguments{
\item{conn}{object [KineticaConnection-class]}

\item{statement}{character}

\item{params}{a list of query named parameters}

\item{...}{Other arguments ommited in generic signature}
}
\description{
Executes SQL statement that does not return a result set
}
\seealso{
[dbSendQuery()] [dbGetQuery()] [dbExecute()]

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
  \code{\link{dbListResults,KineticaConnection-method}},
  \code{\link{dbListTables,KineticaConnection-method}},
  \code{\link{dbReadTable,KineticaConnection,character-method}},
  \code{\link{dbRemoveTable,KineticaConnection,character-method}},
  \code{\link{dbSendQuery,KineticaConnection,character-method}},
  \code{\link{dbWriteTable,KineticaConnection,character-method}},
  \code{\link{show,KineticaDriver-method}},
  \code{\link{sqlCreateTable,KineticaConnection-method}}
}
\concept{KineticaConnection methods}