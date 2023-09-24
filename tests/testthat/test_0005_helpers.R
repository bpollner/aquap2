library(testthat)

test_that("roxygen helpers", { 
  charVec <- c("peter", "paul", "mary", "joshua", "francis")
  expect_type(r_itemize(charVec), "character")
  expect_type(r_listize(charVec), "character")
}) # EOT
