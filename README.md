#RKinetica

**RKinetica** is an R package providing access to Kinetica ODBC driver.

If you are installing Rkinetica package from a downloaded `*.tar.gz`, 
open the RStudio and use the following syntax:
`install.packages("/path/to/RKinetica_7.0.0.0.tar.gz", repos = NULL, type = "source")`

##Configuring R environemnt

```
# Install the latest DBI release from CRAN:
install.packages("DBI")
```

### Connection Strings
Pass the connection parameters as arguments to the dbConnect() function:

```
library(RKinetica)
con <- dbConnect(RKinetica::Kinetica(),
  url = "http://localhost:9191",
  username = rstudioapi::askForPassword("Database user"),
  password = rstudioapi::askForPassword("Database password")
 )

```

Use connection `con` further on as your regular DBI connection.
