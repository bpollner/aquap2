library(testthat)

rett <- function(a) {return(a)}

test_that("test3", {
	expect_true(rett(TRUE))


}) # EOT