skip_unless_has_test_db <- function(expr) {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return(skip("On CRAN"))
  }
  tryCatch({
    # check if there is a local Kinetica DB
    DBItest:::connect(expr)
    TRUE
  }, error = function(e) {
    skip(paste0("Can't find a local instance of KineticaDB:\n", conditionMessage(e)))
  })

}

return_default_context <- function() {
  options(stringsAsFactors = FALSE)
  ctx <- DBItest::make_context(
  Kinetica(),
  list(url = "http://localhost:9191", username = "", password = "", assume_no_nulls = TRUE),
  #  set_as_default = TRUE,
  tweaks = DBItest::tweaks(constructor_name = "Kinetica",
                           constructor_relax_args = TRUE,
                           strict_identifier = TRUE,
                           omit_blob_tests = TRUE,
                           placeholder_pattern = c("?"),
                           logical_return = function(x) {if (is.null(x) || is.na(x)) {NULL} else {as.integer(x)}},
                           current_needs_parens = TRUE,
                           date_cast = function(x) paste0("\"DATE\"('", x, "')"),
                           time_cast = function(x) paste0("\"TIME\"('", format.Date(x,"%H:%M:%S"), "')"),
                           timestamp_cast = function(x) paste0("\"TIMESTAMP\"('", x, "')"),
                           date_typed = TRUE,
                           time_typed = TRUE,
                           timestamp_typed = TRUE,
                           temporary_tables = TRUE
    )
  )
}

check_dataframe <- function(df) {
  expect_is(df, "data.frame")
  if (length(df) >= 1L) {
    lengths <- vapply(df, length, integer(1L), USE.NAMES = FALSE)
    expect_equal(diff(lengths), rep(0L, length(lengths) - 1L))
    expect_equal(nrow(df), lengths[[1]])
  }

  df_names <- names(df)
  expect_true(all(df_names != ""))
  expect_false(anyNA(df_names))

  df
}

unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

print_dataframe <- function(label, x, y) {
  print(label)
  print(paste0(label,"_starts______________________"))
  print(x)
  print(paste0(label,"_middle______________________"))
  print(y)
  print(paste0(label,"_break_______________________"))
}
