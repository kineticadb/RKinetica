library(RKinetica)

# open connection to Kinetica
con<-dbConnect(Kinetica(), url = "http://localhost:9191")

# load RStudio data
trees <- data(trees)

# Write data to table, overwriting existing table if necessary
dbWriteTable(con, "trees", trees, row.names = FALSE, overwrite = TRUE)

# load data from the table
lm_trees<-dbReadTable(con, "trees")

# Scatter plot of data
scatter.smooth(x=lm_trees$Girth, y=lm_trees$Volume, 
               xlab = "Trees Girth", ylab = "Trees Volume", main="Girth ~ Volume")

# correlation
cor(lm_trees$Girth, lm_trees$Volume)

# build linear model and print the result
linearMod<-lm(Girth ~ Volume, data=lm_trees)
print(linearMod)

# pull linear model summary and print it
modelSummary <-summary(linearMod)
print(modelSummary)

# close connection
dbDisconnect(con)
