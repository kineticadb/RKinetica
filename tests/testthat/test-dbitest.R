ctx <- return_default_context()
# DBItest::test_all(skip = NULL, ctx)

# DBItest::test_getting_started(skip=NULL, ctx)
# DBItest::test_driver(skip=NULL, ctx)
# DBItest::test_connection(skip=NULL, ctx)
# DBItest::test_transaction(skip = c("begin_commit_invalid", "with_transaction_error_invalid"), ctx)
# DBItest::test_some(test = c("spec_sql_append_table", "spec_sql_write_table"), ctx)
# DBItest::test_compliance(skip=NULL, ctx)
DBItest::test_all(skip = c(
  # Kinetica does not support the notion of "stale" connections, since it stores only the connection configuration and
  # does not establish a physical connection until the actual query is sent via http request.
  # Thus "stale" connection, unloaded from memory, stored outside and re-loaded again is not considered invalid.
  "disconnect_invalid_connection", "send_query_invalid_connection", "get_query_invalid_connection",
  "send_statement_invalid_connection", "execute_invalid_connection", "read_table_invalid_connection",
  "write_table_invalid_connection", "list_tables_invalid_connection", "exists_table_invalid_connection",
  "remove_table_invalid_connection", "begin_commit_invalid", "with_transaction_error_invalid",


  # Kinetica allows TEMP namespace to be visible outside current connection
   "table_visible_in_other_connection", "remove_table_other_con",

  # The following tests have been customized for Kinetica DB, preserving
  # dataset and actions order of the original test and replacing function call for
  # check_identical() on results with check_equivalent().
  # See "test-equivalent-replaces-identical.R" for customized tests for:
  "append_table", "fetch_n_more_rows", "fetch_n_zero_rows", "get_query_n_zero_rows",
  "read_table_error", "overwrite_table_missing", "write_table_row_names_true_missing",

  # TODO KECO-577
  # multiquote string value support
  "quote_string_roundtrip",
  "roundtrip_quotes", "roundtrip_field_types", "roundtrip_keywords",
  "roundtrip_character", "roundtrip_character_empty", "roundtrip_factor",

  # TODO KECO-586
  # integer 64 support
   "data_64_bit_numeric", "data_64_bit_numeric_warning", "data_64_bit_lossless",
   "roundtrip_64_bit_numeric", "roundtrip_64_bit_character",

  # TODO KECO-587
  # date/time format convertions support
  # "data_date", "data_date_current", "data_time_current",
  "data_timestamp_current",
  "data_date_typed", "data_date_current_typed", "data_timestamp_typed", "data_timestamp_current_typed",
  "data_time", "data_timestamp", "roundtrip_date", "roundtrip_time", "roundtrip_timestamp",

  # TODO KECO-575
  # international characters string support
  "data_character", "roundtrip_character_native",

  # TODO KECO-561
  # logical values support
  "roundtrip_logical",

  # TODO KECO-561
  # infinity support
  "roundtrip_numeric_special", "roundtrip_null",

  # TODO KECO-588
  # row.names string to factor support
  "roundtrip_mixed",

  # KECO-1238 Kinetica transaction management
  # Kinetica does not support transactions at all, but RKinetica provides a fake transaction wrapper object
  # for the sake of dplyr compatibility. Transaction object has BEGIN/COMMIT/ROLLBACK states
  # and satisfies DBI tests as long as no data rollbacks are expected.
  # The tests listed fail on functionality not supported because of transaction rollback.
  "begin_write_rollback", "begin_write_disconnect", "with_transaction_failure", "with_transaction_break",

  # TODO KECO-1265
  # To pass compliance, RKinetica package needs graceful load/unload and redefined environment
  "compliance", "ellipsis",

  # Bind operation on return value, or binding parameters by number or name are not supported by Kinetica DB.
  "bind_return_value", "bind_wrong_name", "bind_named_param_unnamed_placeholders",
  "bind_named_param_empty_placeholders", "bind_named_param_na_placeholders", "bind_repeated",
  "bind_repeated_untouched", "column_info", "get_info_result",

  # Multirow binding is supported by Kinetica, but bind operation on return value is not.
  "bind_multi_row", "bind_multi_row_zero_length", "bind_multi_row_statement",

  # TODO needs error message clarification
  "send_query_only_one_result_set", "send_statement_only_one_result_set", "roundtrip_integer", "roundtrip_numeric",

  # TODO needs error message clarification: closed connection
  "read_table_closed_connection", "remove_table_closed_connection", "get_statement_statement", "temporary_table",


  # The following tests fail due to unsupported value cast syntax, not because of the data type tested.
  # Cast of return parameter name is not allowed.
  # TODO Adding a custom Kinetica test suite to redefine functionality with supported SQL is going to be addressed in future releases.
  "bind_integer", "bind_numeric", "bind_logical", "bind_null", "bind_character", "bind_factor", "bind_date",
  "bind_timestamp", "bind_timestamp_lt",


  # Kinetica DB does not support table names with embedded comma
  "exists_table_error", "exists_table_name", "write_table_error"

  ), ctx)
