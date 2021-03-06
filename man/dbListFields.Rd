% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaConnection.R
\name{dbListFields}
\alias{dbListFields}
\title{dbListFields()}
\usage{
dbListFields(conn, name, ...)
}
\arguments{
\item{conn}{A [KineticaConnection-class] object}

\item{name}{a character string for table name or [KineticaId-class] object}

\item{...}{Other parameters passed on to methods.}
}
\description{
Lists field names of a given table
}
\examples{
\dontrun{
con <- dbConnect(Kinetica(), url = "http://localhost:9191")
dbWriteTable(con, "tableA", data.frame(a = 1:40))
dbExistsTable(con, "tableA")
# TRUE
fields <- dbListFields(con, "tableA")
fields
# "a"
dbRemoveTable(con, "tableA")
dbDisconnect(con)
}
}
\seealso{
Other KineticaConnection methods: \code{\link{dbAppendTable}},
  \code{\link{dbCreateTable}},
  \code{\link{dbDataType,KineticaConnection-method}},
  \code{\link{dbDisconnect,KineticaConnection-method}},
  \code{\link{dbExecute,KineticaConnection,character-method}},
  \code{\link{dbExistsTable}}, \code{\link{dbGetInfo}},
  \code{\link{dbGetQuery,KineticaConnection,character-method}},
  \code{\link{dbIsValid}},
  \code{\link{dbListObjects,KineticaConnection-method}},
  \code{\link{dbListResults,KineticaConnection-method}},
  \code{\link{dbListTables,KineticaConnection-method}},
  \code{\link{dbReadTable}}, \code{\link{dbRemoveTable}},
  \code{\link{dbSendQuery,KineticaConnection,character-method}},
  \code{\link{dbSendStatement,KineticaConnection,character-method}},
  \code{\link{dbWriteTable}},
  \code{\link{show,KineticaDriver-method}},
  \code{\link{sqlCreateTable,KineticaConnection-method}},
  \code{\link{transactions}}
}
\concept{KineticaConnection methods}
