# RKinetica

**RKinetica** is an R package providing access to Kinetica ODBC driver.

This project is the source code of R package. To install RKinetica you can either build it
locally or download a prebuilt package release from [our release page][RKinetica.build].
We would strongly encourage you to use release builds.

[RKinetica.build]: <https://github.com/kineticadb/RKinetica/releases>

Once you have built or downloaded `RKinetica_7.0.0.0.tar.gz` file, 
open the RStudio and use the following syntax in console:
```
install.packages("/path/to/RKinetica_7.0.0.0.tar.gz", repos = NULL, type = "source")
```


## Configuring R environemnt

This package depends on several other CRAN packages: [DBI][DBI_package], [rjson][rjson_package], 
[httr][httr_package], [bit64][bit64_package], [hms][hms_package], and [methods][methods_package].
When you install package from RStudio, its interface recognizes package dependencies and offers
to install them for your convenience, asking for your confirmation to install each package. 
[DBI_package]: <https://cran.r-project.org/web/packages/DBI/index.html>
[rjson_package]: <https://cran.r-project.org/web/packages/rjson/index.html>
[httr_package]: <https://cran.r-project.org/web/packages/httr/index.html>
[bit64_package]: <https://cran.r-project.org/web/packages/bit64/index.html>
[hms_package]: <https://cran.r-project.org/web/packages/hms/index.html>
[methods_package]: <https://cran.r-project.org/web/packages/R.methodsS3/index.html>

Alternatively, you can preinstall packages in one statement:

```
# Install the latest DBI release from CRAN:
install.packages(c("DBI", "rjson", "httr", "bit64", "hms", "methods"))
```

### Connection Strings
Load the RKinetica package as a library. 

```
library(RKinetica)
```

Create a Kinetica connection object by passing connection parameters as arguments to the 
dbConnect() function:

```
con <- dbConnect(RKinetica::Kinetica(),
  url = "http://localhost:9191",
  username = rstudioapi::askForPassword("Database user"),
  password = rstudioapi::askForPassword("Database password")
 )
```

If you don't use rstudio, you can supply username and password values directly:

```
con <- dbConnect(RKinetica::Kinetica(),
  url = "http://localhost:9191", username = "DBuser", password = "DB_p@ssw0rd")
```

Use connection `con` further on as your regular DBI connection.

```
dbListObjects(con)
dbListTables(con)
```
