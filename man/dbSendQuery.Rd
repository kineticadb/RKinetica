% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaConnection.R
\docType{methods}
\name{dbSendQuery,KineticaConnection,character-method}
\alias{dbSendQuery,KineticaConnection,character-method}
\title{dbSendQuery()}
\usage{
\S4method{dbSendQuery}{KineticaConnection,character}(conn, statement,
  params = NULL, offset = NULL, limit = NULL, ...)
}
\arguments{
\item{conn}{object [KineticaConnection-class]}

\item{statement}{character}

\item{params}{a list of query named parameters}

\item{...}{Other arguments omitted in generic signature}
}
\description{
Executes SQL query and returns a result set. The returned result object should be read with dbFetch(result) and
then cleared with dbClearResult(result)
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
  \code{\link{dbListTables,KineticaConnection-method}},
  \code{\link{dbReadTable}}, \code{\link{dbRemoveTable}},
  \code{\link{dbSendStatement,KineticaConnection,character-method}},
  \code{\link{dbWriteTable}},
  \code{\link{show,KineticaDriver-method}},
  \code{\link{sqlCreateTable,KineticaConnection-method}},
  \code{\link{transactions}}
}
\concept{KineticaConnection methods}
