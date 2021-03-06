% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaSQL.R
\docType{methods}
\name{sqlData}
\alias{sqlData}
\alias{sqlData,KineticaConnection-method}
\title{sqlData()}
\usage{
sqlData(con, value, row.names = NA, ...)

\S4method{sqlData}{KineticaConnection}(con, value, row.names = NA, ...)
}
\arguments{
\item{con}{A database connection, object of [KineticaConnection-class]}

\item{value}{A data frame}

\item{row.names}{a flag with logical, character or NULL value}

\item{...}{Other parameters passed on to methods.}

\item{con}{A database connection, object of [KineticaConnection-class]}

\item{row.names}{a flag with logical, character or NULL value}

\item{...}{Other parameters passed on to methods.}

\item{value}{A data frame}
}
\description{
Escapes and encodes SQL data.

Escapes and encodes SQL data.
}
\examples{
\dontrun{
con <- dbConnect(RSQLite::SQLite(), ":memory:")

sqlData(con, head(iris))
sqlData(con, head(mtcars))

dbDisconnect(con)
}
}
