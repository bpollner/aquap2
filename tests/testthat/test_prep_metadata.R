library(testthat)

rett <- function() {return(TRUE)}

test_that("test2", {
	expect_true(rett())


}) # EOT