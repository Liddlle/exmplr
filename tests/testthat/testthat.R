library(testthat)
library(exmplr)

test_check("exmplr")

test_that("result is tibble", {
  df <- fars_summarize_years(2013)
  expect_is(df, "tbl_df")
})
