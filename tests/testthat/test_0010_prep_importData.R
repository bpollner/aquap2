library(testthat)

## prepare (again,) the folders (to make this page independent)

td <- tempdir()
rootF <- paste0(td, "/ap2Test_R")
dir.create(rootF, showWarnings = FALSE)
# gpic: get package inst-folder content
gpic <- function() {
  tp <- path.package("aquap2")
  if (dir.exists(paste0(tp, "/inst"))) {
    ptpInst <- paste0(tp, "/inst")
  } else {
    ptpInst <- tp
  }
  return(ptpInst)
} # EOF
ptp <- gpic()
#
fn_inTempAsRenvSH <- "aquap2SH"
path_inTempAsRenvSH <- tePaSH <- paste0(rootF, "/", fn_inTempAsRenvSH)
dir.create(path_inTempAsRenvSH, showWarnings = FALSE)

setwd(rootF)


#### ### ### ### ### ### ### ### ### ### ### ### ### 
#### ### ### ### ### ### ### ### ### ### ### ### ### 
########## import Data ##########


#### ### ### ### ### ### ### ### ### ### ### ### ### 
  ### download and install example experiments ###
remRepName <- "aquap2_Data-main"
eNa <- "xsComp"
eWhere <- rootF
ptmd <- paste0(td, "/", remRepName)

if (dir.exists(ptmd)) {
  unlink(ptmd, recursive=TRUE)
}# end if
ptxsc <- paste0(rootF, "/", eNa, "@home")
if (dir.exists(ptxsc)) {
  unlink(ptxsc, recursive = TRUE)
}# end if

test_that("ap2dme", { 
  expect_error(ap2dme(paste0(eWhere, "/blabla"), eNa))
  expect_true(ap2dme(eWhere, eNa))
  expect_equal(ap2dme(eWhere, eNa), NULL)
  unlink(paste0(ptxsc, "/rawdata"), recursive=TRUE)
  expect_true(ap2dme(eWhere, eNa))
}) # EOT

test_that("ap2dme - forcing", { 
  expect_true(ap2dme(eWhere, eNa, ffs=T))
  expect_equal(ap2dme(eWhere, eNa, ffs=F, fdo=T), NULL)
  expect_true(ap2dme(eWhere, eNa, ffs=T, fdo=T))
  }) # EOT
# so, now we have a nice folder called "xsComp@home" where we can run tests on gfd etc


#### ### ### ### ### ### ### ### ### ### ### ### ### 
              ### import data ###
xsCompHome <- "xsComp@home"
setwd(paste0(rootF, "/", xsCompHome))

test_that("gfd basic", { 
  msg <- "Aligning temp. and rel.hum"
  expect_output(gfd(trhLog = "ESPEC", ttl=F), msg)
  expect_s4_class(gfd(), "aquap_data")
}) # EOT

# gfd(trhLog = "ESPEC", ttl=F)



# now get fancy: 
    # import from other data sources / formats, 
    # make custom import
    # --> go through the functions in prep_importData.R
