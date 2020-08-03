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
* [Configuring High Availablity (HA)](#configuring-high-availablity-ha)
  * [Automatic Discovery](#automatic-discovery)
  * [Manual Configuration](#manual-configuration)
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
install.packages(c("DBI", "RJSONIO", "httr", "bit64", "hms", "methods", "purrr", "stats", "dplyr"))
```

If opting to build the *RKinetica* package instead of downloading, the connector
package can be built as follows:

```
git clone https://github.com/kineticadb/RKinetica.git -b release/v7.1 --single-branch
R CMD build RKinetica
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
R CMD install RKinetica_7.1.0.0.tar.gz
```

To install the *RKinetica* package in *RStudio* (or *R* console):

```
> install.packages("/path/to/RKinetica_7.1.0.0.tar.gz", repos = NULL, type = "source")
```

## Usage

Before using *RKinetica*, the package must be loaded:

```
library(RKinetica)
```

Create a *KineticaConnection* object using the `dbConnect()` method,
passing in *Kinetica* URL, user, and password parameters:

```
con <- dbConnect(RKinetica::Kinetica(),
                 url = "http://<kinetica-host>:9191",
                 username = "<user>",
                 password = "<password>")
```

**NOTE:** A parameter cannot be added to an existing *KineticaConnection*
object. Instead, a new *KineticaConnection* object must be created to
properly initialize any functionality enabled by additional parameters.

**IMPORTANT:** If using *RStudio*, you can use the *rstudioapi* package to
instead prompt for username and password:

```
con <- dbConnect(RKinetica::Kinetica(),
                 url = "http://<kinetica-host>:9191",
                 username = rstudioapi::askForPassword("Database username?"),
                 password = rstudioapi::askForPassword("Database password?"))
```

If you expect a result set from your queries to exceed 10,000 rows, set the
`row_limit` parameter value accordingly:

```
con <- dbConnect(RKinetica::Kinetica(),
                 url = "http://<kinetica-host>:9191",
                 username = "<user>",
                 password = "<password>",
                 row_limit = 1000000L)
```

You can then use the *KineticaConnection* object as a regular DBI connection.
Some of the most common commands:

* Print connection info:

  ```
  dbGetInfo(con)
  ```

* List top level database objects and collections with their types:

  ```
  dbListObjects(con)
  ```

* Get a list of top level database tables and collections names:

  ```
  dbListTables(con)
  ```

* Drop a table if it exists:

  ```
  dbRemoveTable(con, "tableA")
  ```

* Check if the table exists:

  ```
  dbExistsTable(con, "tableA")
  ```

* Write a table with 3 columns:

  ```
  dbWriteTable(con, "tableA", data.frame(a = 1L, b = 2L, c = 3.0), row.names = NULL)
  ```

* List tableA fields:

  ```
  dbListFields(con, "tableA")
  ```

* Add records to tableA:

  ```
  dbAppendTable(con, "tableA", data.frame(a = 2L:3L, b = 3L:4L, c = 4.0:5.0), row.names = NULL)
  ```

* Read table into variable:

  ```
  rows <- dbReadTable(con, "tableA")
  print(rows)
  ```

* Disconnect:

  ```
  dbDisconnect(con)
  ```


Large tables, views, or query results can have pagination parameters passed into
the `dbSendQuery` or `dbSendStatement` methods. Assuming that the
connection was established with a row limit of 1,000,000, i.e.
`row_limit = 1000000L`, the following example query extracts all records
from the `acquisition` table sorted by `l_id`. The data is retrieved in
batches of one million records and the `offset` is increased by 1 million each
batch. This loop continues until the `dbSendQuery()` resultset is returned
empty:

```
sql_query <- "SELECT l_id, product_type, term, score FROM acquisition ORDER BY l_id"
offset <- 0L
repeat {
  result <- dbSendQuery(con, sql_query, limit = 1000000L, offset = offset)

  # work with current data.frame provided in result@data

  if (nrow(result@data) > 0) {
    # increase offset to get next page of data
    offset <- offset + 1000000L
  } else {
    # exit pagination loop when data.frame is empty
    break
  }
}
```

Additional code examples are available
[in the "examples" subdirectory](examples/).

### Strings vs Factors

When RKinetica reads a character list into R dataframe it can be converted into
a factor. This option is controlled by environment property that's read into
`as.data.frame()` parameter `stringsAsFactors`:

```
stringsAsFactors = default.stringsAsFactors()
```

To set environment option `stringsAsFactors` to TRUE or FALSE explicitly, use
the following syntax at the beginning of your R script or once per session:

```
options(stringsAsFactors = FALSE)
```


## Configuring High Availablity (HA)

### Automatic Discovery

When two or more Kinetica clusters have been configured for an HA ring via
KAgent, RKinetica will automatically discover the additional Kinetica
instance URLs available in the ring. If the connection to the URL of the primary
cluster fails, each additional URL will be tried until a successful connection
is established; every failed connection will result in a warning message. If
all connection attempts fail, an error message will be thrown. Only the URL of
the primary cluster to connect to needs to be specified (via the `url` parameter
in the `dbConnect` method); the URLs for the failover clusters will be retrieved
from the primary cluster upon first connecting to it. The *KineticaConnection*
object has additional parameters to store these failover URLs (via the `ha_ring`
parameter) as well as other connection information:

```
con <- dbConnect(RKinetica::Kinetica(), url = "http://172.123.45.61:9191")
dbGetInfo(con)

$url
[1] "http://172.123.45.61:9191"

$host
[1] "172.123.45.61"

$port
[1] 9191

$ha_enabled
[1] TRUE

$ha_ring
[1] "http://172.123.45.63:9191" "http://172.123.45.61:9191" "http://172.123.45.62:9191"
```

URLs in `ha_ring` list are randomly selected to balance load on secondary URL
instances when the primary URL fails. You can use a `show()` command on a
*KineticaConnection* object at any time to check which URL is being used in the
current connection:

```
show(con)

<KineticaConnection>
HA enabled
Current url: http://172.123.45.62:9191
```

### Manual Configuration

If you want to provide URLs for failover clusters manually, you can do so by
adding the `ha_ring` parameter to the `dbConnect()` method with a
comma-separated list of URIs for the secondary cluster(s):

```
con <- dbConnect(RKinetica::Kinetica(), url = "http://172.123.45.61:9191",
       ha_ring = "http://172.123.45.62:9191,http://172.123.45.63:9191")
show(con)

<KineticaConnection>
Self-provided HA enabled
Current url: http://172.123.45.61:9191
```


## Documentation

Detailed *RKinetica* information including method and function descriptions,
usage examples, and arguments can be found in the
[documentation][RKinetica.docs].
