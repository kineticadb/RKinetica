library(RKinetica)

drv <- Kinetica()

# print driver info
dbGetInfo(drv)

# create connection
con <- dbConnect(drv, url = "http://localhost:9191")

# print connection info
dbGetInfo(con)

# list top level database objects
obj <- dbListObjects(con)
print(obj)

# get a list of top level database tables and collections names
dbListTables(con)

# drop a table if it exists
dbRemoveTable(con, "tableA")

# check if the table exists 
dbExistsTable(con, "tableA")

# write a table with 3 columns
dbWriteTable(con, "tableA", data.frame(a = 1L, b = 2L, c = 3.0), row.names = NULL)

# check if the table exists now
dbExistsTable(con, "tableA")

# list tableA fields
dbListFields(con, "tableA")

# add records to tableA
dbAppendTable(con, "tableA", data.frame(a = 2L:3L, b = 3L:4L, c = 4.0:5.0), row.names = NULL)

# read table into variable
rows <- dbReadTable(con, "tableA")
print(rows)

# disconnect 
dbDisconnect(con)