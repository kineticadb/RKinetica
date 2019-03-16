# ctx <- DBItest::make_context(Kinetica(), list(url = "http://localhost:9191", username = "", password = ""))
ctx <- DBItest::make_context(
  Kinetica(),
  list(url = "http://localhost:9191", username = "", password = ""),
#  set_as_default = TRUE,
  tweaks = DBItest::tweaks(constructor_name = "Kinetica",
                           constructor_relax_args = TRUE,
                           strict_identifier = TRUE,
                           omit_blob_tests = TRUE,
                           placeholder_pattern = c("?"),
                           logical_return = function(x) as.integer(x),
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
DBItest::test_getting_started()
DBItest::test_driver()

# Kinetica does not support the notion of "stale" connections, thus stale connection is not considered invalid
DBItest::test_connection(skip = c("disconnect_invalid_connection"), ctx)

DBItest::test_result(skip = c(
  # Kinetica does not support the notion of "stale" connections, since it stores only the connection configuration and
  # does not establish a physical connection until the actual query is sent. Thus stale connection is not considered invalid
    "send_query_invalid_connection", "get_query_invalid_connection", "execute_invalid_connection", "send_statement_invalid_connection",

  # KineticaConnection can support multiple results per query, so no warnings should be expected for multiple result cases
    "send_query_only_one_result_set", "send_statement_only_one_result_set",

  # international language support is going to be added in later releases
    "data_character", "data_logical",

  # full 64-bit integer support is going to be added in later releases
    "data_64_bit_numeric", "data_64_bit_numeric_warning", "data_64_bit_lossless",

  # Kinetica DB engine does not process generic ITER-based UNIONs with NULLs in the first SELECT, and generates error:
  # 'Unable to create union from input columns due to incompatible datatypes'.
  # A similar test case with strongly typed first SELECT statement would pass without error.
    "data_date", "data_date_current", "data_time_current", "data_timestamp_current", "data_date_typed",
    "data_date_current_typed", "data_timestamp_typed", "data_timestamp_current_typed", "data_time", "data_timestamp",

  # Kinetica currently does not support row.names concept, it will be addressed in later versions of RKinetica.
  # The tests listed below fail on the row.names component with NULL value being stored in character mode, not numeric mode,
  # which makes the expected output fail identity clause, while providing a valid equality clause due to row.names storage mode only.
    "fetch_n_more_rows", "fetch_n_zero_rows", "get_query_n_zero_rows"), ctx)

# Strings with excessive use of single quotes are going to be addressed in later versions of RKinetica
#DBItest::test_sql()

#DBItest::test_meta(skip = NULL, ctx)

# Kinetica does not support transactions and does not perform rollbacks
# DBItest::test_transaction(skip = c("begin_commit_return_value", "begin_rollback_return_value", "begin_begin",
#   "begin_commit", "begin_write_commit", "begin_rollback"), ctx)

# DBItest::test_compliance(skip = c("compliance", ellipsis"), ctx)
