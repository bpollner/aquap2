library(testthat)

glvar <- "get_settings_from_aquap2_package_root"
assign(glvar, TRUE, pos=.GlobalEnv)

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
  expect_error(gfd(ttl=F, trhLog = 3)) # XXX new
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

# now install a fake sample list wiht both error and conSNr column
erFiNa <- "xsComp-in-bothCols.xlsx"
file.copy(paste0(ptp, "/testHelpers/sl_in/", erFiNa), 
          paste0(rootF, "/", xsCompHome, "/sampleLists/sl_in/xsComp-in.xlsx"), overwrite = TRUE)

test_that("gfd - both error and conSNr column", { 
  expect_error(gfd(ttl=F))
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

# first set it up
LBWBhome <- "LBWB@home"
eNa <- "LBWB"
test_that("ap2dme - setup LBWB", { 
  expect_true(ap2dme(eWhere, eNa, sh=tePaSH))
  expect_null(ap2dme(eWhere, eNa, sh=tePaSH))
}) # EOT
setwd(paste0(rootF, "/", LBWBhome)) 

# now test custom data import
test_that("gfd, custom import - all is good", { 
  expect_output(gfd(sh=tePaSH), "detecting outliers")
  expect_output(gfd(sh=tePaSH), "was loaded")
}) # EOT

custImpFile <- "Buechi_Ibk_a4t6e2_dx.R"
file.copy(paste0(tePaSH, "/", custImpFile), rootF)
unlink(paste0(tePaSH, "/", custImpFile))

test_that("gfd, custom import: missing file", { 
  expect_error(gfd(ttl=F, sh=tePaSH))
}) # EOT
# put it back
file.copy(paste0(rootF, "/", custImpFile), paste0(tePaSH, "/", custImpFile))
unlink(paste0(rootF, "/", custImpFile))

test_that("gfd, custom import - all is good again", { 
  expect_output(gfd(sh=tePaSH), "was loaded")
}) # EOT

test_that("gfd - wrong things", { 
  expect_error(gfd(filetype="blabla", ttl=F, sh=tePaSH))
  expect_error(gfd(filetype=c("bla", 2), ttl=F, sh=tePaSH))
  expect_error(gfd(naString = c("bla", 4), ttl=F, sh=tePaSH))  
  expect_error(gfd(md="blabla", ttl=F, sh=tePaSH))  
  }) # EOT

si <- sibup <- readSpectra(sh=tePaSH)
test_that("gfd_check_imports", { 
  si$sampleNr <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$conSNr <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$timePoints <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$ecrm <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$repl <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$group <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$temp <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$relHum <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$C_cols <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$Y_cols <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$timestamp <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$NIR <- 4
  expect_error(gfd_check_imports(si)); si <- sibup
  si$info$nCharPrevWl <- "ablabla"
  expect_error(gfd_check_imports(si)); si <- sibup
  dimnames(si$NIR)[[2]][1] <- "www1000"
  expect_error(gfd_check_imports(si)); si <- sibup
  ns <- names(si)
  ns[1] <- "blabla"
  names(si) <- ns
  expect_error(gfd_check_imports(si)); si <- sibup
  names(si$info) <- "blabla"
  expect_error(gfd_check_imports(si)); si <- sibup  
}) # EOT

# gfd(filetype="blabla", ttl=F, sh=tePaSH)

test_that("gfd_makeNiceColumns", { 
  len <- nrow(si$NIR)
  si$sampleNr <- data.frame(sn=rep(1, len))
  si$conSNr <- data.frame(csn=rep(4, len))
  si$timePoints <- data.frame(tp=rep("T0", len))
  si$ecrm <- data.frame(ecrm=rep("ec", len))
  si$repl <- data.frame(reps=rep("R1", len))
  si$group <- data.frame(grp=rep("Cont", len))
  si$temp <- data.frame(temp=rep(25, len))
  si$relHum <- data.frame(temp=rep(55, len))
  si$C_cols <- data.frame(c1=rep("aa", len), c2=rep("bb", len))
  si$Y_cols <- data.frame(y1=rep(2, len), y2=rep(5, len))
  expect_null(gfd_check_imports(si))
  expect_type(gfd_makeNiceColumns(si), "list")
}) # EOT
si <- sibup
gfd_check_imports(si)
si <- gfd_makeNiceColumns(si)

# now we want to check for double columns.
# we will use a fake sample list for that
# move the old,the good one, to root
ok <- file.copy(paste0(rootF, "/", LBWBhome, "/sampleLists/sl_in/LBWB-in.xlsx"), to=rootF)
if (!ok) {stop("File copy error")}
ok <- file.copy(paste0(ptp, "/testHelpers/sl_in/LBWB-in.xlsx"), 
          paste0(rootF, "/", LBWBhome, "/sampleLists/sl_in/LBWB-in.xlsx"), overwrite=TRUE)
if (!ok) {stop("File copy error")}

# we now have an erronous sample list file in place that has double columns
test_that("gfd - double column", { 
  expect_error(gfd(ttl=F, sh=tePaSH))
}) # EOT

# copy back the good one
ok <- file.copy(paste0(rootF, "/LBWB-in.xlsx"), 
                paste0(rootF, "/", LBWBhome, "/sampleLists/sl_in/LBWB-in.xlsx"), overwrite=TRUE)
if (!ok) {stop("File copy error")}
# all should be good again
test_that("gfd - all good again", { 
  expect_output(gfd(ttl=F, sh=tePaSH, stf = FALSE), "not saved")
}) # EOT


test_that("gfd - small things", { 
  expect_error(gfd(ttl=F, sh=tePaSH, stf="Yes"))
  expect_error(gfd(ttl=F, sh=tePaSH, dol="Yes"))
  fd <- gfd()
  fd@version <- "0.0.2"
  expect_error(checkDatasetVersion(fd))
}) # EOT

# rempove the rawdata file
file.copy(paste0(rootF, "/", LBWBhome, "/rawdata/LBWB.dx"), 
          paste0(rootF))
unlink(paste0(rootF, "/", LBWBhome, "/rawdata/LBWB.dx"))
test_that("checkForPresenceOfData", { 
  expect_error(checkForPresenceOfData())
}) # EOT
# copy rawdata file back
file.copy(paste0(rootF, "/LBWB.dx"), 
          paste0(rootF, "/", LBWBhome, "/rawdata/LBWB.dx") )
# gfd(ttl=F, sh=tePaSH)

test_that("save & load", { 
  expect_error(saveAQdata(rootF))
  expect_message(loadAQdata(getmd(expName="bla")), "does not seem to exist")
  expect_output(loadAQdata(getmd(expName="LBWB")), "loaded")
}) # EOT

test_that("more readHeader_checkDefaults", { 
  expect_no_condition(readHeader_checkDefaults(slType="def", "xls", getmd(), "auto"))
  expect_error(readHeader_checkDefaults(slType="bla", "xls", getmd(), "auto"))
  expect_error(readHeader_checkDefaults(slType="bla", "xls", md=4, "auto"))
  expect_no_condition(readHeader_checkDefaults(slType="def", "xls", getmd(), "def"))
  expect_error(readHeader_checkDefaults(slType="def", "xls", getmd(), "yesPlease"))
  }) # EOT

test_that("check_sl_existence", { 
  expect_error(check_sl_existence("bla", ".xls"))
}) # EOT

test_that("check_conScanColumn", { 
  fd <- gfd()
  header <- fd$header
  header <- header[,-3] # remove the conSNr column  
  slfp <- "sl_in/slFilePath"
  rdfp <- "rawdata/spectFilePath"
  cft <- "custom@blaFile.dx"
  expect_error(check_conScanColumn(header, slfp, rdfp, slType = NULL, cft))
  expect_error(check_conScanColumn(header, slfp, rdfp, slType = "xls", cft))
}) # EOT

test_that("imp_searchAskColumns", { 
  nr <- 4
  cvars <- data.frame(gr=rep("ng", nr), ti=rep("nt", nr), ecrm=rep("ec", nr), repl=rep("Rx", nr), 
                      cbla=rep("blax", nr), cbla2=rep("bla2", nr)) 
  
  yvars <- data.frame(snr=rep(1, nr), consnr=rep(4, nr), temp=rep(4, nr), rh=rep(4, nr), 
                      ybla=rep(4, nr),  ybla2=rep(4, nr))
  impFunc <- function(cvars, yvars, oT=TRUE) {
    imp_searchAskColumns(cvars, yvars, "xls", oT)
    return(ls())
  } # EOIF 
  expect_output(impFunc(cvars, yvars, oT=TRUE), "Please enter")
}) # EOT

test_that("export_ap2_ToXlsx", { 
  fd <- gfd()
  expect_true(export_ap2_ToXlsx(fd, TRUE))
  expect_true(export_ap2_ToXlsx(fd))
}) # EOT




# now get fancy: 
    # import from other data sources / formats, 
