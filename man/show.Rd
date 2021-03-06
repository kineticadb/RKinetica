% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kineticaDriver.R, R/kineticaConnection.R,
%   R/kineticaResult.R
\docType{methods}
\name{show,KineticaDriver-method}
\alias{show,KineticaDriver-method}
\alias{show,KineticaConnection-method}
\alias{show,KineticaResult-method}
\title{show()}
\usage{
\S4method{show}{KineticaDriver}(object)

\S4method{show}{KineticaConnection}(object)

\S4method{show}{KineticaResult}(object)
}
\arguments{
\item{object}{[KineticaConnection-class]}

\item{object}{[KineticaResult-class]}
}
\description{
RKinetica Driver short printout string

Minimal KineticaConnection string representation
Provides url of the Kinetica DB instance

Brief string printout of [KineticaResult-class] object properties
}
\examples{
\dontrun{
drv <- Kinetica()
show(drv)
#  KineticaDriver 7.1.0.0
}
\dontrun{
con <- dbConnect(d, url = "http://localhost:9191")
show(con)
<KineticaConnection>
  url: http://localhost:9191
}
}
\seealso{
Other KineticaDriver methods: \code{\link{dbCanConnect,KineticaDriver-method}},
  \code{\link{dbConnect,KineticaDriver-method}},
  \code{\link{dbDataType,KineticaDriver-method}},
  \code{\link{dbGetInfo}}, \code{\link{dbIsValid}},
  \code{\link{dbListConnections,KineticaDriver-method}},
  \code{\link{dbUnloadDriver,KineticaDriver-method}}

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
  \code{\link{dbWriteTable}},
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
  \code{\link{dbIsValid}}
}
\concept{KineticaConnection methods}
\concept{KineticaDriver methods}
\concept{KineticaResult methods}
