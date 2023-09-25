library(testthat)

## prepare (again,) the folders (to make this page independent)

td <- tempdir()
rootF <- paste0(td, "/ap2Test_R")
dir.create(rootF, showWarnings = FALSE)
ptp <- path.package("aquap2")
#
fn_inTempAsRenvSH <- "aquap2SH"
path_inTempAsRenvSH <- tePaSH <- paste0(rootF, "/", fn_inTempAsRenvSH)
dir.create(path_inTempAsRenvSH, showWarnings = FALSE)

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

## use xsComp for data import etc.
# first copy from examples/experiments/xsComp@home
exs <- "examples/experiments"
xsCompHome <- "xsComp@home"
xsCompFrom <- paste0(gpic(), "/", exs, "/", xsCompHome)
cpxs <- function() {
  ok <- file.copy(xsCompFrom, to=rootF, recursive = TRUE)
  if (!ok) {stop()}
} # EOF
cpxs()
setwd(paste0(rootF, "/", xsCompHome))

#### now import fine nice data 
test_that("gfd basic", { 
  msg <- "Aligning temp. and rel.hum"
  expect_output(gfd(trhLog = "ESPEC", ttl=F), msg)
  expect_s4_class(gfd(), "aquap_data")
  print(getwd())
}) # EOT

# now get fancy: 
    # import from other data sources / formats, 
    # make custom import
    # --> go through the functions in prep_importData.R
