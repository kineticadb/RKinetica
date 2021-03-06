% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaConnection.R
\docType{methods}
\name{dbExecute,KineticaConnection,character-method}
\alias{dbExecute,KineticaConnection,character-method}
\title{dbExecute()}
\usage{
\S4method{dbExecute}{KineticaConnection,character}(conn, statement, ...,
  params = NULL)
}
\arguments{
\item{conn}{an object of [KineticaConnection-class]}

\item{statement}{character}

\item{...}{Other parameters passed on to methods.}

\item{params}{a set of values}
}
\description{
Executes the statement, returning the number of affected rows or objects
}
\examples{
\dontrun{
con <- dbConnect(Kinetica(), url = "http://localhost:9191")
dbCreateTable(con, "tableA", data.frame(a = 1))
dbExistsTable(con, "tableA")
# TRUE
dbExecute(con, "DROP TABLE tableA")
# 1
dbDisconnect(con)
}
}
\seealso{
Other KineticaConnection methods: \code{\link{dbAppendTable}},
  \code{\link{dbCreateTable}},
  \code{\link{dbDataType,KineticaConnection-method}},
  \code{\link{dbDisconnect,KineticaConnection-method}},
  \code{\link{dbExistsTable}}, \code{\link{dbGetInfo}},
  \code{\link{dbGetQuery,KineticaConnection,character-method}},
  \code{\link{dbIsValid}}, \code{\link{dbListFields}},
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
