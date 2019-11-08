library(testthat)
library(RKinetica)

test_check("RKinetica", reporter = "summary", stop_on_failure = FALSE, stop_on_warning = FALSE)
