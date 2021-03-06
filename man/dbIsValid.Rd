% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaObject.R, R/kineticaDriver.R,
%   R/kineticaConnection.R, R/kineticaResult.R
\docType{methods}
\name{dbIsValid}
\alias{dbIsValid}
\alias{dbIsValid,KineticaDriver-method}
\alias{dbIsValid,KineticaConnection-method}
\alias{dbIsValid,KineticaResult-method}
\title{dbIsValid()}
\usage{
dbIsValid(dbObj, ...)

\S4method{dbIsValid}{KineticaDriver}(dbObj, ...)

\S4method{dbIsValid}{KineticaConnection}(dbObj, ...)

\S4method{dbIsValid}{KineticaResult}(dbObj, ...)
}
\arguments{
\item{dbObj}{An object inheriting from [KineticaObject-class],
i.e. [KineticaDriver-class], [KineticaConnection-class],
[KineticaId-class] or a [KineticaResult-class]}

\item{...}{Other arguments to methods.}

\item{dbObj}{an object of [KineticaDriver-class] type}

\item{...}{Other arguments omitted in generic signature}

\item{dbObj}{A [KineticaConnection-class] object}

\item{...}{Other arguments omitted in generic signature}

\item{dbObj}{[KineticaResult-class]}

\item{...}{Other parameters passed on to methods.}
}
\value{
logical
}
\description{
Generic dbIsValid() implementation.

Checks if the provided Driver object is valid

Checks if the connection is valid

Checks if the KineticaResult object has been cleared
}
\details{
RKinetica Connection is a configuration of url, username and password to establish
connection to Kinetica DB, configuration does not go stale when memory objects get swapped to file.
Thus RKinetica accepts stale connections as valid, and allows multiple results per connection.
}
\examples{
\dontrun{
drv <- Kinetica()
dbIsValid(drv)
#  TRUE
}
\dontrun{
con <- dbConnect(Kinetica(), url = "http://localhost:9191")
dbIsValid(con)
# TRUE
dbDisconnect(con)
}
}
\seealso{
Other KineticaDriver methods: \code{\link{dbCanConnect,KineticaDriver-method}},
  \code{\link{dbConnect,KineticaDriver-method}},
  \code{\link{dbDataType,KineticaDriver-method}},
  \code{\link{dbGetInfo}},
  \code{\link{dbListConnections,KineticaDriver-method}},
  \code{\link{dbUnloadDriver,KineticaDriver-method}},
  \code{\link{show,KineticaDriver-method}}

Other KineticaConnection methods: \code{\link{dbAppendTable}},
  \code{\link{dbCreateTable}},
  \code{\link{dbDataType,KineticaConnection-method}},
  \code{\link{dbDisconnect,KineticaConnection-method}},
  \code{\link{dbExecute,KineticaConnection,character-method}},
  \code{\link{dbExistsTable}}, \code{\link{dbGetInfo}},
  \code{\link{dbGetQuery,KineticaConnection,character-method}},
  \code{\link{dbListFields}},
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

Other KineticaResult methods: \code{\link{dbBind,KineticaResult-method}},
  \code{\link{dbClearResult,KineticaResult-method}},
  \code{\link{dbColumnInfo}}, \code{\link{dbFetch}},
  \code{\link{dbGetInfo}},
  \code{\link{dbGetRowCount,KineticaResult-method}},
  \code{\link{dbGetRowsAffected,KineticaResult-method}},
  \code{\link{dbGetStatement,KineticaResult-method}},
  \code{\link{dbHasCompleted,KineticaResult-method}},
  \code{\link{show,KineticaDriver-method}}
}
\concept{KineticaConnection methods}
\concept{KineticaDriver methods}
\concept{KineticaResult methods}
