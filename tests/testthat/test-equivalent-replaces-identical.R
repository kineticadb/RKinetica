context("expect_equivalent replacing expect_identical in dataframe results")

# in the following DBItests "expect_identical" function has been replaced with "expect_equivalent"
library(DBItest)

skip_unless_has_test_db( {
  ctx <- return_default_context()
})

# replaces DBItest function of the same name
test_that("fetch_n_more_rows", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  query <- paste(paste("SELECT", 1:3, "AS a", sep = " ", collapse = " UNION "), "ORDER BY a")
  res <- dbSendQuery(con, query)
  rows <- check_dataframe(dbFetch(res, 5L))
  expect_equivalent(rows, data.frame(a = 1:3))
  #' If fewer rows than requested are returned, further fetches will
  #' return a data frame with zero rows.
  rows <- check_dataframe(dbFetch(res))
  expect_equivalent(rows, data.frame(a = integer()))
})

# replaces DBItest function of the same name
test_that("fetch_n_zero_rows", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  query <- paste(paste("SELECT", 1:3, "AS a", sep = " ", collapse = " UNION "), "ORDER BY a")
  res <- dbSendQuery(con, query)

  rows <- check_dataframe(dbFetch(res, 0L))
  expect_equivalent(rows, data.frame(a = integer()))
})

# replaces DBItest function of the same name
test_that("get_query_n_zero_rows", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  query <- paste(paste("SELECT", 1:3, "AS a", sep = " ", collapse = " UNION "), "ORDER BY a")
  res <- dbSendQuery(con, query)
  rows <- check_dataframe(dbGetQuery(con, query, n = 0L))
  expect_equivalent(rows, data.frame(a=integer()))
})

# replaces DBItest function of the same name
test_that("read_table", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in)
  iris_out <- check_dataframe(dbReadTable(con, "iris"))
  expect_equivalent(iris_out, iris_in)
  dbRemoveTable(con, "iris")
})

# replaces DBItest function of the same name
test_that("read_table_row_names_na_missing", {
  row.names <- NA

  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in, row.names = FALSE)
  iris_out <- check_dataframe(dbReadTable(con, "iris", row.names = row.names))
  expect_equivalent(iris_out, iris_in)
  dbRemoveTable(con, "iris")
})

# replaces DBItest function of the same name
test_that("read_table_row_names_na_exists",  {
  row.names <- NA
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  mtcars_in <- datasets::mtcars
  dbWriteTable(con, "mtcars", mtcars_in, row.names = TRUE)
  mtcars_out <- check_dataframe(dbReadTable(con, "mtcars", row.names = row.names))
  expect_equivalent(mtcars_out, mtcars_in)
  dbRemoveTable(con, "mtcars")
})

# replaces DBItest function of the same name
test_that("append_table", {
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )
  dbRemoveTable(con, "iris")
  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in[0,])
  expect_error(dbWriteTable(con, "iris", iris_in[1:10,], append = TRUE), NA)
  iris_out <- check_dataframe(dbReadTable(con, "iris"))
  iris_compare <- iris_in[1:10,]
  expect_equivalent(iris_out, iris_compare)

})

# replaces DBItest function of the same name
test_that("append_table_new", {
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  expect_error(dbWriteTable(con, "iris", iris_in[1:10,], append = TRUE), NA)
  iris_out <- check_dataframe(dbReadTable(con, "iris"))
  expect_equivalent(iris_out, iris_in[1:10,])

})

# replaces DBItest function of the same name
test_that("read_table_row_names_false", {
  #' - If `FALSE` or `NULL`, the returned data frame doesn't have row names.
  for (row.names in list(FALSE, NULL)) {
    con <- DBItest:::connect(ctx)
    on.exit(
      tryCatch({
        dbRemoveTable(con, "mtcars")
        dbDisconnect(con)
      }, error = function(e) NULL
      )
    )
    mtcars_in <- datasets::mtcars
    dbWriteTable(con, "mtcars", mtcars_in, row.names = TRUE)
    mtcars_out <- check_dataframe(dbReadTable(con, "mtcars", row.names = row.names))

    expect_true("row_names" %in% names(mtcars_out))
    expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
    expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
    expect_equivalent(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))

    dbRemoveTable(con, "mtcars")
  }
})

# replaces DBItest function of the same name
test_that("read_table_row_names_na_missing", {
  row.names <- NA

  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in, row.names = FALSE)
  iris_out <- check_dataframe(dbReadTable(con, "iris", row.names = row.names))
  expect_equivalent(iris_out, iris_in)
})

# replaces DBItest function of the same name
test_that("read_table_row_names_default",  {
  #'
  #' The default is `row.names = FALSE`.
  #'
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "mtcars")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )
  mtcars_in <- datasets::mtcars
  dbWriteTable(con, "mtcars", mtcars_in, row.names = TRUE)
  mtcars_out <- check_dataframe(dbReadTable(con, "mtcars"))

  expect_true("row_names" %in% names(mtcars_out))
  expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
  expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
  expect_equivalent(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))

})

# replaces DBItest function of the same name
test_that("write_table_row_names_true_exists", {
  #' - If `TRUE`, row names are converted to a column named "row_names",
  row.names <- TRUE
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "mtcars")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )
  mtcars_in <- datasets::mtcars
  dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
  mtcars_out <- check_dataframe(dbReadTable(con, "mtcars", row.names = FALSE))

  expect_true("row_names" %in% names(mtcars_out))
  expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
  expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
  expect_equivalent(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
})

# replaces DBItest function of the same name
test_that("write_table_row_names_true_missing", {
  #'   even if the input data frame only has natural row names from 1 to `nrow(...)`.
  row.names <- TRUE
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )
  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in, row.names = row.names)
  iris_out <- check_dataframe(dbReadTable(con, "iris", row.names = FALSE))

  expect_true("row_names" %in% names(iris_out))
  expect_true(all(rownames(iris_in) %in% iris_out$row_names))
  expect_true(all(iris_out$row_names %in% rownames(iris_in)))
  expect_equivalent(iris_out[names(iris_out) != "row_names"], iris_in)
})

# replaces DBItest function of the same name
test_that("write_table_row_names_na_exists", {
  #' - If `NA`, a column named "row_names" is created if the data has custom row names,
  row.names <- NA

  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "mtcars")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )
  mtcars_in <- datasets::mtcars
  dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
  mtcars_out <- check_dataframe(dbReadTable(con, "mtcars", row.names = FALSE))

  expect_true("row_names" %in% names(mtcars_out))
  expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
  expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
  expect_equivalent(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
})

# replaces DBItest function of the same name
test_that("write_table_row_names_na_missing", {
  #'   no extra column is created in the case of natural row names.
  row.names <- NA
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in, row.names = row.names)
  iris_out <- check_dataframe(dbReadTable(con, "iris", row.names = FALSE))
  expect_equivalent(iris_out, iris_in)
})

# replaces DBItest function of the same name
test_that("write_table_row_names_string_exists", {
  row.names <- "make_model"
  #' - If a string, this specifies the name of the column in the remote table
  #'   that contains the row names,

  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "mtcars")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )
  mtcars_in <- datasets::mtcars

  dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
  mtcars_out <- check_dataframe(dbReadTable(con, "mtcars", row.names = FALSE))

  expect_true("make_model" %in% names(mtcars_out))
  expect_true(all(mtcars_out$make_model %in% rownames(mtcars_in)))
  expect_true(all(rownames(mtcars_in) %in% mtcars_out$make_model))
  expect_equivalent(mtcars_out[names(mtcars_out) != "make_model"], unrowname(mtcars_in))
})

# replaces DBItest function of the same name
test_that("write_table_row_names_string_missing", {
  row.names <- "seq"
  #'   even if the input data frame only has natural row names.
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in, row.names = row.names)
  iris_out <- check_dataframe(dbReadTable(con, "iris", row.names = FALSE))

  expect_true("seq" %in% names(iris_out))
  expect_true(all(iris_out$seq %in% rownames(iris_in)))
  expect_true(all(rownames(iris_in) %in% iris_out$seq))
  expect_equivalent(iris_out[names(iris_out) != "seq"], iris_in)
})

# replaces DBItest function of the same name
test_that("overwrite_table", {
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  dbWriteTable(con, "iris", iris_in)
  expect_error(dbWriteTable(con, "iris", iris_in[1:10,], overwrite = TRUE), NA)
  iris_out <- check_dataframe(dbReadTable(con, "iris"))
  expect_equivalent(iris_out, iris_in[1:10, ])
})


# replaces DBItest function of the same name
test_that("overwrite_table_missing", {
  #' This argument doesn't change behavior if the table does not exist yet.
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "iris")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )

  iris_in <- datasets::iris
  iris_in$Species <- as.character(iris_in$Species)
  expect_error(dbWriteTable(con, "iris", iris[1:10,], overwrite = TRUE), NA)
  iris_out <- check_dataframe(dbReadTable(con, "iris"))
  expect_equivalent(iris_out, iris_in[1:10, ])
})

# KECO-1671 Reading special literals from Kinetica: NaN, Infinity, -Infinity
test_that("reading_special_values_NaN_Infinity", {
  con <- DBItest:::connect(ctx)
  on.exit(
    tryCatch({
      dbRemoveTable(con, "test_nan_values")
      dbDisconnect(con)
    }, error = function(e) NULL
    )
  )
  dbWriteTable(con, "test_nan_values", data.frame(d1 = 0.1, d2 = 0.3))
  dbWriteTable(con, "test_values", data.frame(d1 = 0, d2 = 0))
  dbExecute(con, "INSERT INTO test_nan_values SELECT NULL as d1, SQRT(-1) as d2 FROM test_values; ")
  dbExecute(con, "INSERT INTO test_nan_values SELECT 1.0/d1 as d1, 1.0/d2 as d2 FROM test_values; ")
  dbExecute(con, "DELETE FROM test_nan_values WHERE d1=0.1; ")
  dbRemoveTable(con, "test_values")
  df_nan <- check_dataframe(dbReadTable(con, "test_nan_values", row.names = FALSE))
  # Expected df_nan values:
  #    d1  d2
  # 1  NA NaN
  # 2 Inf Inf
  testthat::expect( is.na(df_nan[[1]][1]), "NA value not present")
  testthat::expect(is.nan(df_nan[[2]][1]), "NaN value not present")
  testthat::expect(is.infinite(df_nan[[1]][2]), "Inf value not present")
  testthat::expect(is.infinite(df_nan[[2]][2]), "Inf value not present")

})
