context("Kinetica_schema_naming")

# in the following tests objects are referenced with and without schema name
library(DBItest)

skip_unless_has_test_db( {
  ctx <- return_default_context()
})

test_that("default_schema_available", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  expect_equivalent(con@default_schema, "ki_home")
  tables <- dbListTables(con)
  expect_true("ki_home" %in% tables)

  table <- "tableA"
  df <- data.frame(a = 1L, b = 2L, c = 3.0)
  dbWriteTable(con, table, df, row.names = NULL, overwrite = TRUE)

  table_exists <- dbExistsTable(con, table)
  expect_true(table_exists)

  table_fields <- dbListFields(con, table)
  expect_true(all(names(df) %in% table_fields))
  expect_true(all(table_fields %in% names(df)))

  dbAppendTable(con, table, data.frame(a = 2L:3L, b = 3L:4L, c = 4.0:5.0), row.names = NULL)
  rows <- dbReadTable(con, table)
  expect_equivalent(length(rows), 3L)

  dbRemoveTable(con, table)
  table_exists_now <- dbExistsTable(con, table)
  expect_false(table_exists_now)
})

test_that("schema_provided_in_name", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  table <- "some_schema.some_table"
  schema <- unlist(strsplit(table, "[.]"))[1]

  df <- data.frame(a = 1L, b = 2L, c = 3.0)
  dbWriteTable(con, table, df, row.names = NULL, overwrite = TRUE)

  tables <- dbListTables(con)
  expect_true(schema %in% tables)

  table_exists <- dbExistsTable(con, table)
  expect_true(table_exists)

  table_fields <- dbListFields(con, table)
  expect_true(all(names(df) %in% table_fields))
  expect_true(all(table_fields %in% names(df)))

  dbAppendTable(con, table, data.frame(a = 2L:3L, b = 3L:4L, c = 4.0:5.0), row.names = NULL)
  rows <- dbReadTable(con, table)
  expect_equivalent(length(rows), 3L)

  dbRemoveTable(con, table)
  table_exists_now <- dbExistsTable(con, table)
  expect_false(table_exists_now)

  dbExecute(con, paste("DROP SCHEMA IF EXISTS ", dbQuoteIdentifier(con, schema)))
})

test_that("schema_provided_in_id_string", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  table <- KineticaId("schema_provided.some_table")
  schema <- table@name["schema"]
  df <- data.frame(a = 1L, b = 2L, c = 3.0)
  dbWriteTable(con, table, df, row.names = NULL, overwrite = TRUE)

  tables <- dbListTables(con)
  expect_true(schema %in% tables)

  table_exists <- dbExistsTable(con, table)
  expect_true(table_exists)
  table_fields <- dbListFields(con, table)
  expect_true(all(names(df) %in% table_fields))
  expect_true(all(table_fields %in% names(df)))

  dbAppendTable(con, table, data.frame(a = 2L:3L, b = 3L:4L, c = 4.0:5.0), row.names = NULL)
  rows <- dbReadTable(con, table)
  expect_equivalent(length(rows), 3L)

  dbRemoveTable(con, table)
  table_exists_now <- dbExistsTable(con, table)
  expect_false(table_exists_now)

  dbExecute(con, paste("DROP SCHEMA IF EXISTS ", dbQuoteIdentifier(con, schema)))
})


test_that("schema_provided_in_id_tuple", {
  con <- DBItest:::connect(ctx)
  on.exit(dbDisconnect(con))

  schema <- "another_schema"
  table <- KineticaId(schema = schema, table ="another_table")
  df <- data.frame(a = 1L, b = 2L, c = 3.0)
  dbWriteTable(con, table, df, row.names = NULL, overwrite = TRUE)

  tables <- dbListTables(con)
  expect_true(schema %in% tables)

  table_exists <- dbExistsTable(con, table)
  expect_true(table_exists)
  table_fields <- dbListFields(con, table)
  expect_true(all(names(df) %in% table_fields))
  expect_true(all(table_fields %in% names(df)))

  dbAppendTable(con, table, data.frame(a = 2L:3L, b = 3L:4L, c = 4.0:5.0), row.names = NULL)
  rows <- dbReadTable(con, table)
  expect_equivalent(length(rows), 3L)

  dbRemoveTable(con, table)
  table_exists_now <- dbExistsTable(con, table)
  expect_false(table_exists_now)

  dbExecute(con, paste("DROP SCHEMA IF EXISTS ", dbQuoteIdentifier(con, schema)))
})

