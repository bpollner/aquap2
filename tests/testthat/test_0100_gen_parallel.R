library(testthat)

## file gen_parallel

# foreach::getDoParName()
test_that("Parallel things", { 
  expect_false(checkHaveParallel())
  expect_no_condition(checkNrOfWorkers())
  expect_type(getDesiredNrCPUs(), "integer")
  expect_no_condition(registerParallelBackend())
  expect_true(checkHaveParallel())
#  doParallel::stopImplicitCluster() # ? can not be stopped ?
#  expect_false(checkHaveParallel())
}) # EOT

