ctx <- return_default_context()

DBItest::test_getting_started(skip=NULL, ctx)
DBItest::test_driver(skip=NULL, ctx)
DBItest::test_connection(skip = c("disconnect_invalid_connection"), ctx)
DBItest::test_transaction(skip = c("begin_commit_invalid", "with_transaction_error_invalid",
                                   "begin_write_rollback", "begin_write_disconnect",
                                   "with_transaction_failure", "with_transaction_break",
                                   # NEW: need to investigate data frame failure
                                   "reading_special_values_NaN_Infinity"), ctx)
DBItest::test_result(skip = c("send_query_invalid_connection", "get_query_invalid_connection",
                              "send_statement_invalid_connection", "execute_invalid_connection",
                              "read_table_invalid_connection", "write_table_invalid_connection",
                              "list_tables_invalid_connection", "exists_table_invalid_connection",
                              "send_query_only_one_result_set", "send_statement_only_one_result_set",
                              "fetch_n_more_rows", "fetch_n_zero_rows", "get_query_n_zero_rows",
                              "data_timestamp_current",
                              "data_date_typed", "data_date_current_typed", "data_timestamp_typed", "data_timestamp_current_typed",
                              "data_time", "data_timestamp", "roundtrip_date", "roundtrip_time", "roundtrip_timestamp",
                              "data_64_bit_numeric", "data_64_bit_numeric_warning", "data_64_bit_lossless",
                              # NEW: need to investigate
                              "send_statement_result_valid"), ctx)
DBItest::test_sql(skip = c("quote_string_roundtrip", "roundtrip_integer", "roundtrip_numeric",
                           "roundtrip_raw", "roundtrip_blob",
                           "roundtrip_quotes", "roundtrip_field_types", "roundtrip_keywords",
                           "roundtrip_character", "roundtrip_character_empty", "roundtrip_factor",
                           "roundtrip_64_bit_numeric", "roundtrip_64_bit_character",
                           "roundtrip_date", "roundtrip_time", "roundtrip_timestamp",
                           "roundtrip_character_native", "roundtrip_logical",
                           "roundtrip_numeric_special", "roundtrip_null", "roundtrip_mixed",
                           "read_table_invalid_connection", "write_table_invalid_connection",
                           "write_table_error", "list_tables_invalid_connection", "exists_table_invalid_connection",
                           "list_tables", "exists_table_list", "remove_table_list", "remove_table_temporary",
                           "overwrite_table_missing", "table_visible_in_other_connection",
                           "temporary_table", "remove_table_invalid_connection", "remove_table_other_con",
                           "remove_table_closed_connection", "read_table_closed_connection",
                           "exists_table_error", "read_table_error",
                           # NEW: need to investigate
                           "append_table", "exists_table_name"
), ctx)
DBItest::test_meta(skip = c("bind_return_value", "bind_wrong_name", "bind_named_param_unnamed_placeholders",
                            "bind_named_param_empty_placeholders", "bind_named_param_na_placeholders", "bind_repeated",
                            "bind_multi_row", "bind_multi_row_zero_length", "bind_multi_row_statement",
                            "bind_integer", "bind_numeric", "bind_logical", "bind_null", "bind_character",
                            "bind_factor", "bind_date", "bind_timestamp", "bind_timestamp_lt",
                            "bind_repeated_untouched", "get_statement_statement", "get_info_result"), ctx)
DBItest::test_compliance(skip = c("ellipsis"), ctx)
DBItest::test_stress(skip = NULL, ctx)
