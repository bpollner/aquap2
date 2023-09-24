library(testthat)

## prepare (again,) the folders (to make this page independent)

td <- tempdir()
rootF <- paste0(td, "/ap2Test_R")
dir.create(rootF, showWarnings = FALSE)
expHomeF <- paste0(rootF, "/test@home")
dir.create(expHomeF, showWarnings = FALSE)
setwd(expHomeF)
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

## now we need to copy everything we need to import data.