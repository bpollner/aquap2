library(testthat) # for local line by line

test_that("checkOnTest", {
  expect_false(checkOnTest())
}) # EOT

assign("get_settings_from_aquap2_package_root", TRUE, pos=.GlobalEnv)

test_that("checkOnTest", {
  expect_true(checkOnTest())
}) # EOT

test_that("checkOnLocal", {
  expect_no_condition(checkOnLocal())
}) # EOT

test_that("test_getLocalStn", {
  expect_type(test_getLocalStn(), "list")
}) # EOT

test_that("updateSettings", {
  expect_type(updateSettings(), "list")
}) # EOT

test_that("getstn", {
  expect_type(getstn(), "list")
}) # EOT


td <- tempdir()
rootF <- paste0(td, "/ap2Test_R")
dir.create(rootF)
expHomeF <- paste0(rootF, "/test@home")
dir.create(expHomeF)
setwd(expHomeF)
ptp <- path.package("aquap2")

remAll <- function(roF=rootF) {
  unlink(roF, recursive = T)
} # EOF

test_that("genFolderStr", {
  expect_output(genFolderStr(), "Folder structure created.")
  unlink(paste0(expHomeF, "/results"))
  expect_output(genFolderStr(), "Some folders were created.")
}) # EOT

test_that("autoUpS", {
  expect_type(autoUpS(), "list")
}) # EOT

test_that("aquap2_handover_to_uniset", {
  expect_type(aquap2_handover_to_uniset(), "list")
}) # EOT

### so far, we did test reading in the settings and creating the folder structure
### now go towards creating the sample list

test_that("exportSampleList", {
 # expect_type(exportSampleList(), "list")
}) # EOT


