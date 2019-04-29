library(RKinetica)
library(tibble)
library(stats)
library(dplyr)

# connect with ODBC driver
con <- RKinetica::dbConnect(Kinetica(), url = "http://localhost:9191")
dbGetInfo(con)
data(mtcars)
dbWriteTable(con, "cars", mtcars, overwrite = TRUE, row.names = TRUE)

# create a dplyr table object  based on existing table datasource
dbCars<-tbl(con, "cars")

# select everything
select(dbCars, everything())

# select fields by name
select(dbCars, cyl, gear, mpg, hp)

# apply distinct() to selected fields
distinct(select(dbCars, cyl, gear, mpg, hp))

# select column names starting with 'c'
select(dbCars, starts_with("c"))

# select column names not starting with 'c'
select(dbCars, -starts_with("c"))

# select column names containing 'a'
select(dbCars, contains("a"))

# select column names not containing 'a'
select(dbCars, -contains("a"))

# filter cars with 4 gears and 6 cylinders
filter(dbCars, gear == 4 & cyl == 6)

# show SQL for the above statement
show_query(filter(dbCars, gear == 4 & cyl == 6))

# filter cars with 4 and 6 cylinders
filter(dbCars, cyl %in% c(4, 6))

# show SQL for the above statement
show_query(filter(dbCars, cyl %in% c(4, 6)))

# filter cars with either 3 gears or 6 cylinders
filter(dbCars, gear == 3 | cyl == 6)

# show SQL for the above statement
show_query(filter(dbCars, gear == 3 | cyl == 2))

# calculate mean cylinders and average mpg
# it would throw a warning and a suggested syntax edit
summarise(dbCars, mean_cyl = mean(cyl), avg_mpg = avg(mpg))

# if you follow the suggested syntax edit,
# the following would throw and error due to SQL syntax unsupported by Kinetica
# none of the aggregate functions with extra R arguments like avg(mpg, na.rm = TRUE)
# are translated into SQL correctly and should be avoided
summarise(dbCars, mean_cyl = mean(cyl), avg_mpg = avg(mpg, na.rm = TRUE))

# run sum and avg on all the listed variables
summarise_at(dbCars, vars(mpg, cyl), funs(sum(.), avg(.)))

# the following would fail due to unsupported by Kinetica SQL syntax
# use count instead of n(), and median() is not translated correctly
summarise_at(dbCars, vars(mpg, cyl), funs(n(), sum(.), avg(.)))

# run the aggregation functions conditionally only on numeric fields
summarise_if(dbCars, is.numeric, funs(avg, min, max, mean, count))

# the following would fail due to unsupported by Kinetica SQL syntax
summarise_if(dbCars, is.numeric, funs(n(), median))

# run the aggregation functions on all fields
# string fields would be using alphabetical order
summarise_all(dbCars, funs(min,max))

# sort the dataset by ascending cyl and descending mpg
arrange(dbCars, cyl, desc(mpg))

# sort the dataset by descending mpg and ascending cyl
arrange(dbCars, desc(mpg), cyl)

# group cars by gears and list count, min and max value in each group
summarise_at(group_by(dbCars, gear), vars(mpg, disp, wt), funs(count, min(.), max(.)))

# show query for the statement above
show_query(summarise_at(group_by(dbCars, gear), vars(mpg, disp, wt), funs(count, min(.), max(.))))

# creating a data frame
df <- as.data.frame(dbCars)

# calculate percentile values
df %>% group_by(cyl) %>% summarise(Pecentile_25=quantile(mpg, probs=0.25),
                                   Pecentile_50=quantile(mpg, probs=0.50),
                                   Pecentile_75=quantile(mpg, probs=0.75),
                                   Pecentile_99=quantile(mpg, probs=0.99))

# divide data into 5 bins by mpg value, new column pos contains bin number
x <- data.frame(df)
x <- mutate(x, pos = ntile(x$mpg, 5))

# add columns to dataframe through custom functions, old columns are preserved
mutate(df, trans = ifelse(am == 0, "automatic", "manual"), engine = ifelse(vs == 0, "V-shaped", "straight"))

# add columns, converting all existing numeric columns to percent form, old columns are preserved
# new columns names get a "_percent" suffix
mutate_all(df, funs("percent" = ./100L))

# rank rows by minimum mpg value
mutate_at(df, vars(mpg), funs(Rank = min_rank(.)))

# rank rows by maximum mpg value
mutate_at(df, vars(mpg), funs(Rank = min_rank(desc(.))))

# calculate cumulative sum
df %>% group_by(cyl) %>% mutate(Total=cumsum(drat)) %>% select(cyl, drat, Total)

# create two separate datasets with common column row_names, drat (PK)
ds1 <- select(dbCars, mpg, cyl, disp, drat, row_names)
ds2 <- select(dbCars, row_names, hp, drat, wt, qsec, vs, am, gear, carb)

# inner join on PK
inner_join(ds1, ds2, by = "row_names")

# print the query above
show_query(inner_join(ds1, ds2, by = "row_names"))

# left join on drat
left_join(ds1, ds2, by = "drat")

# print the query above
show_query(left_join(ds1, ds2, by = "drat"))

# right join on drat
right_join(ds1, ds2, by = "drat")

# print the query above
show_query(right_join(ds1, ds2, by = "drat"))

# full join on drat
full_join(ds1, ds2, by = "drat")

# semi join on drat (join where exists)
semi_join(ds1, ds2, by = "drat")

# print the query above
show_query(semi_join(ds1, ds2, by = "drat"))

# anti join on drat (join where does not exist)
anti_join(ds1, ds2, by = "drat")

# print the query above
show_query(anti_join(ds1, ds2, by = "drat"))


# create two separate datasets with same columns and some common rows
ds1 <- filter(dbCars, cyl %in% c(4, 6))
ds2 <- filter(dbCars, cyl %in% c(6, 8))

# dataset intersection - should be rows with cyl = 6
intersect(ds1, ds2)

# dataset union - should be rows with cyl = 4, 6, 8,
# rows with 6 appear twice
union(ds1, ds2)

# set difference - rows that appear in ds1, but not in ds2
setdiff(ds1, ds2)


##################################################################################################
# conditional verbs, where if represents conditional function under which the action is applied
# select non-numeric columns only
select_if(dbCars, is.factor)

# summarise numeric columns only
summarise_if(dbCars, is.numeric, funs(sum, avg))

# mutate if the column is numeric
mutate_if(dbCars, is.numeric, funs("percent" = ./100L))

##################################################################################################
# compute, collect, collapse functions

# fails due to unsupported in Kinetica transaction wrapping
copy_to(mtcars, name = "mtcars2", overwrite = TRUE)

# create a formula
remote <- dbCars %>% group_by(cyl) %>% mutate(Total=cumsum(drat)) %>% select(cyl, drat, Total)

# shows the sql statement and execution order
explain(remote)

# save table remotely - fails due to incorrect TEMPORARY/TEMP sql translation
compute(remote, name = "cars2")

# fails due to incorrect window functions sql transaltion
collect(remote)

# fails due to incorrect window functions sql transaltion
collapse(remote)
dbDisconnect(con)
