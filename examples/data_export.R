library(RKinetica)
library(graphics)
library(grDevices)
library(stats)

# create connection to Kinetica DB
con <- dbConnect(Kinetica(), url = "http://localhost:9191")

# Drop the table if already exists
if (dbExistsTable(con, "tidy_chicks")) {
  dbRemoveTable(con, "tidy_chicks")
}

# load dataset provided with RStudio
data("ChickWeight")

# save dataset to a table in Kinetica
dbWriteTable(con, "tidy_chicks", as.data.frame(ChickWeight), row.names = NULL)

# read the table into dataset
data <- dbReadTable(con, "tidy_chicks")

# count number of rows in the table
count <- nrow(data)
print(paste(count, "records inserted into", "tidy_chicks"))

# define category grouping by diet type field unique values
categories <- unique(data$Diet)

for (cat in categories) {
  # for each category print its name
  print(paste0("Category ", cat))
  # build a select statement for individual category
  sql <- paste("SELECT * FROM tidy_chicks where Diet=", cat, sep="")
  print(sql)

  # read data for this category
  ds <- dbGetQuery(con, sql)
  attach(ds)

  # create a pdf document for each diet with 3 charts each
  # pdf documents should be saved one level up from the installed R home
  pdf(file = paste0("diet", cat,".pdf"), title = paste("Chick Diet #", cat, " plots"))
  # chart # 1
  boxplot(ds$weight ~ ds$Diet,
          main = paste("Diet ", cat, " coplot by chick quantity"),
          ylab = "Weight(g)")
  
  # chart # 2
  coplot(ds$weight ~ ds$Time | ds$Chick, data = ds, type = "b", show.given = FALSE,
         xlab = "Weight(g)", ylab = "Time(days)")
  # chart # 3
  qqnorm(ds$weight)
  # close pdf
  dev.off()

  detach(ds)
  
}


# disconnect from Kinetica
dbDisconnect(con)
