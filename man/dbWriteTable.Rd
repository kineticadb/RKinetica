% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaConnection.R
\name{dbWriteTable}
\alias{dbWriteTable}
\title{dbWriteTable()}
\usage{
dbWriteTable(conn, name, value, ...)
}
\arguments{
\item{conn}{an object of [KineticaConnection-class]}

\item{name}{character string for table name or [KineticaId-class] object}

\item{value}{a [data.frame] (or object coercible to data.frame)}

\item{...}{Other arguments omitted in generic signature}

\item{row.names}{a logical flag, NULL or chaacter value to create extra row_names column}

\item{overwrite}{a logical flag to overwrite existing table with new columns and values}

\item{append}{a logical flag to preserve existing data and append new records}

\item{field.types}{a named character vector of value field names and types}

\item{temporary}{a logical flag to create table as temporary storage}
}
\description{
Writes a dataset into Kinetica DB, appending, creating or overwriting a table
as indicated by flags.
}
\examples{
\dontrun{
con <- dbConnect(Kinetica(), url = "http://localhost:9191")
dbWriteTable(con, "test", data.frame(a = 1L:3L, b = 2.1:4.2), row.names = FALSE)
dbDisconnect(con)
}
}
\seealso{
[dbCreateTable()] [dbAppendTable()] [dbRemoveTable()]

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
  \code{\link{dbSendQuery,KineticaConnection,character-method}},
  \code{\link{dbSendStatement,KineticaConnection,character-method}},
  \code{\link{show,KineticaDriver-method}},
  \code{\link{sqlCreateTable,KineticaConnection-method}},
  \code{\link{transactions}}
}
\concept{KineticaConnection methods}
