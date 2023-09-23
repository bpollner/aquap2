library(testthat) # for local line by line

rett <- function() {return(TRUE)}


test_that("test1", {
	expect_true(rett())


}) # EOT