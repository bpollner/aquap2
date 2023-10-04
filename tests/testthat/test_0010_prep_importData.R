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

test_that("gfd - no data", { 
  expect_error(gfd(ttl=F))
}) # EOT


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
            ### sl, trhLog, multRows ###

xsCompHome <- "xsComp@home"
setwd(paste0(rootF, "/", xsCompHome))

test_that("gfd basic", { 
  msg <- "Aligning temp. and rel.hum"
  expect_output(gfd(), msg)
  expect_s4_class(gfd(), "aquap_data")
}) # EOT

test_that("gfd - outliers", { 
  expect_output(gfd(ttl=F, dol=FALSE), "Done.")
}) # EOT

test_that("gfd - sl types", { 
  expect_error(gfd(ttl=F, slType=NULL))
  expect_output(gfd(ttl=F, slType="xls"), "Done.")
  expect_error(gfd(ttl=F, slType="blabla"))
}) # EOT

test_that("gfd - trhLog", { 
  expect_output(gfd(ttl=F, trhLog = "ESPEC"), "Done.")
  expect_error(gfd(ttl=F, trhLog = "blabla"))
  expect_error(gfd(ttl=F, trhLog = TRUE))
  expect_output(gfd(ttl=F, trhLog = FALSE), "Dataset saved")
}) # EOT


test_that("gfd - multiply Rows", { 
  expect_error(gfd(ttl=F, multiplyRows = "blabla"))
  expect_output(gfd(ttl=F, multiplyRows = "auto"), "Done.")
  expect_output(gfd(ttl=F, multiplyRows = TRUE), "Done.")
  expect_error(gfd(ttl=F, multiplyRows = FALSE))
}) # EOT

# now we manually multiply the sample list
test_that("sampleList_multiplyRows", { 
  expect_true(sampleList_multiplyRows())
  expect_false(sampleList_multiplyRows())
}) # EOT

test_that("gfd - multiply Rows, again", { 
  expect_output(gfd(ttl=F), "Done.")
  expect_output(gfd(ttl=F, multiplyRows = "auto"), "Done.")
  expect_output(gfd(ttl=F, multiplyRows = FALSE), "Done.")  
  expect_error(gfd(ttl=F, multiplyRows = TRUE))
}) # EOT

# now install a fake sample list with error values in it
aa <- "xsComp-in.xlsx"
fsl_from <- paste0(ptp, "/testHelpers/sl_in/", aa)
fsl_to <- paste0(rootF, "/", xsCompHome, "/", "sampleLists/sl_in/", aa)
file.copy(fsl_from, to=fsl_to, overwrite = TRUE)

test_that("gfd - wrong err vals", { 
  expect_error(gfd(ttl=F, multiplyRows = FALSE))
  expect_error(gfd(ttl=F, multiplyRows = "auto"))
  expect_error(gfd(ttl=F, multiplyRows = TRUE))
}) # EOT

# again, manually multiply the now fake sample list
test_that("sampleList_multiplyRows", { 
  expect_output(sampleList_multiplyRows(), "7 samples show aberrant number")
  expect_false(sampleList_multiplyRows())
}) # EOT

test_that("gfd - sample list multiplied, wrong number of rows", { 
  expect_error(gfd(ttl=F, multiplyRows = TRUE))
  expect_error(gfd(ttl=F, multiplyRows = "auto"))
  expect_error(gfd(ttl=F, multiplyRows = TRUE))
}) # EOT

# get back the original xsComp
test_that("ap2dme - forcing #2", { 
  expect_true(ap2dme(eWhere, eNa, ffs=T))
}) # EOT
setwd(paste0(rootF, "/", xsCompHome)) 
# because when forcing the home folder got deleted


#### ### ### ### ### ### ### ### ### ### ### ### ### 
            ### import data ### 
            ### customImport ###
# custom import is tested via the experiment LBWB





# now get fancy: 
    # import from other data sources / formats, 
    # --> go through the functions in prep_importData.R
