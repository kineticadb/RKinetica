[RKinetica.build]: <https://github.com/kineticadb/RKinetica/releases>
[RKinetica.docs]: <https://github.com/kineticadb/RKinetica/blob/master/RKinetica-manual.pdf>


# RKinetica Connector Guide

The following guide provides step by step instructions to get started using
*R* with *Kinetica*. The *RKinetica* connector provides access to the *Kinetica*
database through a REST interface. The *RKinetica* package can be built locally
from source or a prebuilt package release can be downloaded from the
[release page][RKinetica.build]. *Kinetica* strongly recommends that release
builds be used instead of building the package locally.

*RKinetica* interfaces with *Kinetica* similarly to other *R*-database
interactions: using the *DBI* and *dplyr* packages. Interacting with
*RKinetica* is easiest using the *RStudio* interface. *RStudio* and *RKinetica*
require that *R* be installed locally.

Source code for the connector can be found at:

* <https://github.com/kineticadb/RKinetica>


## Contents

* [Build & Install](#build--install)
* [Usage](#usage)
* [Documentation](#documentation)


## Build & Install

*RKinetica* depends on several other *R* packages from the comprehensive *R*
archive network (*CRAN*):

* [DBI](https://cran.r-project.org/web/packages/DBI/index.html)
* [rjson](https://cran.r-project.org/web/packages/rjson/index.html)
* [httr](https://cran.r-project.org/web/packages/httr/index.html)
* [bit64](https://cran.r-project.org/web/packages/bit64/index.html)
* [hms](https://cran.r-project.org/web/packages/hms/index.html)
* [methods](https://cran.r-project.org/web/packages/R.methodsS3/index.html)

**NOTE:**  If installing the *RKinetica* package in *RStudio*, the dependencies
should be installed automatically prior to *RKinetica* being installed.
Otherwise, all packages can be installed via *RStudio* (or the *R* console) like
so:

```
install.packages(c("DBI", "rjson", "httr", "bit64", "hms", "methods"))
```


If opting to build the *RKinetica* package instead of downloading, the connector
package can be built as follows:

```
$ git clone https://github.com/kineticadb/RKinetica.git -b release/v7.0 --single-branch
$ R CMD build RKinetica
```

This sequence produces a `tar.gz` file, which, once installed, is made
available to *R*. The `tar.gz` file is created in the same directory the
`build` command was issued, i.e. not in the `RKinetica` repository. Verify that
`tar.gz` file was created before installing the *RKinetica* package:

```
ls RKinetica*
```

To install the *RKinetica* package via the command line:

```
$ R CMD install RKinetica_7.0.0.0.tar.gz
```

To install the *RKinetica* package in *RStudio* (or *R* console):

```
> install.packages("/path/to/RKinetica_7.0.0.0.tar.gz", repos = NULL, type = "source")
```

## Usage

Before using *RKinetica*, the package must be loaded:

```
library(RKinetica)
```

Create a *Kinetica* connection object using the `dbConnect()` function,
passing in *Kinetica* URL, user, and password parameters:

```
con <- dbConnect(RKinetica::Kinetica(),
                 url = "http://<kinetica-host>:9191",
                 username = "<user>",
                 password = "<password>")
```

**IMPORTANT:** If using *RStudio*, you can use the *rstudioapi* package to
instead prompt for username and password:

```
con <- dbConnect(RKinetica::Kinetica(),
                 url = "http://<kinetica-host>:9191",
                 username = rstudioapi::askForPassword("Database username?"),
                 password = rstudioapi::askForPassword("Database password?"))
```

If you expect the result set to exceed 10,000 rows, set the custom row_limit 
parameter value:

```
con <- dbConnect(RKinetica::Kinetica(),
                 url = "http://<kinetica-host>:9191",
                 username = "<user>",
                 password = "<password>",
                 row_limit = 1000000L)
```

You can then use the *Kinetica* connection object as a regular DBI connection:

```
# Print connection info
dbGetInfo(con)

# List top level database objects and collections with their types
dbListObjects(con)

# Get a list of top level database tables and collections names
dbListTables(con)

# Drop a table if it exists
dbRemoveTable(con, "tableA")

# Check if the table exists
dbExistsTable(con, "tableA")

# Write a table with 3 columns
dbWriteTable(con, "tableA", data.frame(a = 1L, b = 2L, c = 3.0), row.names = NULL)

# Check if the table exists now
dbExistsTable(con, "tableA")

# List tableA fields
dbListFields(con, "tableA")

# Add records to tableA
dbAppendTable(con, "tableA", data.frame(a = 2L:3L, b = 3L:4L, c = 4.0:5.0), row.names = NULL)

# Read table into variable
rows <- dbReadTable(con, "tableA")
print(rows)

# Disconnect
dbDisconnect(con)
```

Additional code examples are available
[in the "examples" subdirectory](examples/).


## Documentation

Detailed *RKinetica* information including method and function descriptions,
usage examples, and arguments can be found in the
[documentation][RKinetica.docs].
