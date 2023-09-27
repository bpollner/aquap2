library(testthat) # for local line by line

glvar <- "get_settings_from_aquap2_package_root"

if (exists(glvar, where = .GlobalEnv)) {
  rm(list=glvar, pos=.GlobalEnv)
} # end if




########## Uniset - Settings ########## 
#### ### ### ### ### ### ### ### ### ### ### ### ### 
#### ### ### ### ### ### ### ### ### ### ### ### ### 

test_that("checkOnTest", {
  expect_false(checkOnTest())
}) # EOT

assign(glvar, TRUE, pos=.GlobalEnv)

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

test_that("aquap2_handover_to_uniset", {
  expect_type(aquap2_handover_to_uniset(), "list")
}) # EOT

# we can NOT test ap2_settings_setup
# DONE testing uniset functions / settings things




#### ### ### ### ### ### ### ### ### ### ### ### ### 
#### ### ### ### ### ### ### ### ### ### ### ### ### 
########## generate folder structure ##########
td <- tempdir()
rootF <- paste0(td, "/ap2Test_R")
dir.create(rootF)
expHomeF <- paste0(rootF, "/test@home")
dir.create(expHomeF)
setwd(expHomeF)
ptp <- path.package("aquap2")
#
fn_inTempAsRenvSH <- "aquap2SH"
path_inTempAsRenvSH <- tePaSH <- paste0(rootF, "/", fn_inTempAsRenvSH)
dir.create(path_inTempAsRenvSH)



remAll <- function(roF=rootF) {
  unlink(roF, recursive = T)
} # EOF

test_that("checkForExperimentFolderStructure", { # that one is checking the folder structure
  expect_error(checkForExperimentFolderStructure())
}) # EOT

test_that("genFolderStr", {
  expect_output(genFolderStr(), "Folder structure created.")
  unlink(paste0(expHomeF, "/results"))
  expect_output(genFolderStr(), "Some folders were created.")
}) # EOT

test_that("autoUpS", { # that one is checking the folder structure
  expect_type(autoUpS(), "list")
}) # EOT

test_that("printStdColnames", { # that one is checking the folder structure
  expect_output(printStdColnames(), "Y_RelHum")
}) # EOT



#### ### ### ### ### ### ### ### ### ### ### ### ### 
#### ### ### ### ### ### ### ### ### ### ### ### ### 
########## check get metadata and analysis procedure ########## 
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

#### ### ### ### ### ### ### ### ### ### ### ### ### 
# first copy all material from the metadata folder in testHelpers
th <- "testHelpers"
md <- "metadata"
sh <- "aquap2SH"
fr0 <- paste0(gpic(), "/", th, "/", md, "/md0")
fr1 <- paste0(gpic(), "/", th, "/", md, "/md1")
fr2_sh <- paste0(gpic(), "/", th, "/", sh)
fr_md2ap <- paste0(gpic(), "/", th, "/", md, "/md2ap")
to <- paste0(expHomeF, "/metadata")
#
cpa <- function() {
  allFi0 <- list.files(fr0, full.names = TRUE)  # for testing adding keys to files
  ok <- file.copy(allFi0, to, overwrite = TRUE) # for testing adding keys to files
  if (!all(ok)) {stop()}
  #
  allFi1 <- list.files(fr1, full.names = TRUE)  # for testing the sample list creation
  ok <- file.copy(allFi1, to, overwrite = TRUE) # for testing the sample list creation
  if (!all(ok)) {stop()}
  #
  fil2_sh <- list.files(fr2_sh, full.names = TRUE) # in aquap2SH
  ok <- file.copy(fil2_sh, to=path_inTempAsRenvSH, overwrite = TRUE) # copy into td/aquap2SH
  if (!all(ok)) {stop()}
  #
  fil3_ap <- list.files(fr_md2ap, full.names = TRUE) # the anprocs to add and delete keys
  ok <- file.copy(fil3_ap, to, overwrite = TRUE) # copy into td/aquap2SH
  if (!all(ok)) {stop()}
} ## EOF
cpa()
### end copy things##
#### ### ### ### ### ### ### ### ### ### ### ### ### 


#### ### ### ### ### ### ### ### ### ### ### ### ### 
                ### Metadata ###
test_that("getmd - baiscs", { 
  expect_no_condition(getmd()) # all default
  expect_equal(getmd(expName="blabla")$meta$expName, "blabla")
  expect_error(getmd("blaName"))
  expect_error(getmd(c("blaName", 1)))
  expect_error(getmd(c("blaName", "oth")))
}) # EOT

test_that("getmd - additions", { 
  tio <- "2 keys were added"
  mdf <- "md_ad_1.R"
  expect_output(getmd(mdf), tio)
  tio <- "4 keys were added"
  mdf <- "md_ad_2.R"
  expect_output(getmd(mdf), tio)
  tio <- "8 keys were added"
  mdf <- "md_ad_3.R"
  expect_output(getmd(mdf), tio)
  tio <- "6 keys were added"
  mdf <- "md_ad_4.R"
  expect_output(getmd(mdf), tio)
}) # EOT

test_that("getmd - deletions", { 
  tio <- "1 key was deleted"
  mdf <- "md_del_1.R"
  expect_output(getmd(mdf), tio)
  tio <- "3 keys were deleted"
  mdf <- "md_del_2.R"
  expect_output(getmd(mdf), tio)
  tio <- "5 keys were deleted"
  mdf <- "md_del_3.R"
  expect_output(getmd(mdf), tio)
}) # EOT

test_that("getmd - deletions & additions", { 
  tio <- "7 keys were added"
  mdf <- "md_del_ad_1.R"
  expect_output(getmd(mdf), tio)
  tio <- "8 keys were added"
  mdf <- "md_del_ad_2.R"
  expect_output(getmd(mdf), tio)
}) # EOT

# now all should be ok
fnv <- c("md_ad_1.R", "md_ad_2.R", "md_ad_3.R", "md_ad_4.R", "md_del_1.R", "md_del_2.R", 
         "md_del_3.R", "md_del_ad_1.R", "md_del_ad_2.R")
test_that("getmd - now all good", { 
  for (i in seq_along(fnv)) {
    expect_no_condition(getmd(fnv[i]))
  } # end for i
}) # EOT

# now produce errors and template copying
test_that("getmd - errors, template", { 
  mdf <- "md_err_1.R"
  expect_error(getmd(mdf)) # "could not be"
  mdf <- "md_err_2.R"
  expect_error(getmd(mdf)) # "unsavoury punctuation"
}) # EOT


#### ### ### ### ### ### ### ### ### ### ### ### ### 
      ## mow test default values ##
fndef <- paste0(expHomeF, "/metadata/metadata.R") # here all the pre-check values
tf <- function(argm="z", val=0, fn=fndev) {
  e <- new.env()
  sys.source(fndef, envir=e)
  assign(argm, val, envir = e)
  return(e)
} # EOF

test_that("check_mdDefaultValues - all good", { 
  expect_no_condition(check_mdDefaultValues(tf()))
}) # EOT

test_that("check_mdDefaultValues - make some errors", { 
  arg <- "commonValue"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- 55
  expect_error(check_mdDefaultValues(tf(arg, val)))
  #
  arg <- "envControlLabel"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- 55
  expect_error(check_mdDefaultValues(tf(arg, val)))
  #
  arg <- "realMeasurementLabel"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- 55
  expect_error(check_mdDefaultValues(tf(arg, val)))
  #
  arg <- "filetype"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- 55
  expect_error(check_mdDefaultValues(tf(arg, val)))
  # the custom thing
  val <- "custom@isNotHere.R"
  expect_error(check_mdDefaultValues(tf(arg, val), tePaSH))
  val <- "custom@importFromFormatX.R"
  expect_no_condition(check_mdDefaultValues(tf(arg, val), tePaSH))
  #
  arg <- "noiseFileName"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- 55
  expect_error(check_mdDefaultValues(tf(arg, val)))
  #
  arg <- "tempCalibFileName"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- 55
  expect_error(check_mdDefaultValues(tf(arg, val)))
  #
  arg <- "spacing"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- c(TRUE, 5)
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- TRUE
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- FALSE
  expect_no_condition(check_mdDefaultValues(tf(arg, val)))
  val <- 5
  expect_no_condition(check_mdDefaultValues(tf(arg, val)))
  #
  arg <- "sl_classes"
  val <- c("a", "b")
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- 55
  expect_error(check_mdDefaultValues(tf(arg, val)))
  val <- "notHereFile"
  expect_error(check_mdDefaultValues(tf(arg, val)))
}) # EOT


#### ### ### ### ### ### ### ### ### ### ### ### ### 
            ### Analysis Procedure ###
test_that("getap - basics", { 
  expect_no_condition(getap()) # all default
  expect_no_condition(getap("anproc.R")) # all default
  expect_error(getap("blaName"))
  expect_error(getap(c("blaName", 1)))
  expect_error(getap(c("blaName", "oth")))
}) # EOT

# now for adding and deleting things, just a little bit
test_that("getap - additions", { 
  tio <- "46 keys were added"
  apf <- "ap_add_1.r"
  expect_output(getap(apf), tio)  
  #
  tio <- "37 keys were added"
  apf <- "ap_add_2.r"
  expect_output(getap(apf), tio)
}) # EOT

test_that("getap - deletions", { 
  tio <- "4 keys were deleted"
  apf <- "ap_del_1.r"
  expect_output(getap(apf), tio) 
  #
  tio <- "10 keys were deleted"
  apf <- "ap_del_2.r"
  expect_output(getap(apf), tio) 
}) # EOT

test_that("getap - additions & deletions", { 
  tio <- "4 keys were deleted"
  apf <- "ap_del_add_1.r"
  expect_output(getap(apf), tio) 
  #
  tio <- "8 keys were deleted"
  apf <- "ap_del_add_2.r"
  expect_output(getap(apf), tio) 
}) # EOT

# now all should be ok
fnv <- c("ap_add_1.r", "ap_add_2.r", "ap_del_1.r", "ap_del_2.r", "ap_del_add_1.r", "ap_del_add_2.r")
test_that("getap - now all good", { 
 for (i in seq_along(fnv)) {
   expect_no_condition(getap(fnv[i]))
 } # end for i
}) # EOT

test_that("getap - wrong arg", { 
  expect_error(getap(do.pcapca=T))
}) # EOT


#### ### ### ### ### ### ### ### ### ### ### ### ### 
        ### Modify Analysis Procedure ###
test_that("modify ap", { 
  # spl.var
  expect_no_condition(getap(spl.var="C_Group"))
  # spl.do.csAvg
  expect_no_condition(getap(spl.do.csAvg=TRUE))
  expect_no_condition(getap(do.pca=T))
  expect_no_condition(getap(do.sim=T))
  expect_no_condition(getap(do.pls=T))
  expect_no_condition(getap(do.aqg=T))
  #
  expect_no_condition(getap(do.da=T))
  expect_no_condition(getap(do.rnf=T))
  expect_no_condition(getap(do.svm=T))
  expect_no_condition(getap(do.nnet=T))
  #
  expect_no_condition(getap(pg.where=""))
}) # EOT



########## check create Sample List ########## 
#### ### ### ### ### ### ### ### ### ### ### ### ### 
#### ### ### ### ### ### ### ### ### ### ### ### ### 

# gpic: get pathToPackage Inst content
makePure <- function(sali) { # kicks out all the MQ samples, easier to count
  ind <- which(sali$C_ECRM == "MQ")
  out <- sali[-ind,]
#  print(paste0(nrow(out), " rows"))
  return(out)
} # EOF
nrr <- function(sali) {
  pu <- makePure(sali)
  return(nrow(pu))  
} # EOF

test_that("esl - wrong xlsx structure", { # that one is checking the folder structure
  mdf <- "md_a_0err.r"
  expect_error(esl(getmd(mdf), rnd = F, showFirstRows = F))
}) # EOT

test_that("esl - all errors", { # that one is checking the folder structure
  mdf <- "blabla.R"
  expect_error(esl(md=getmd(mdf), rnd = F, showFirstRows = F))
  mdf <- "md_a_2.r"
  expect_error(esl(md=getmd(mdf), form="aa", rnd = F, showFirstRows = F))
  mdf <- "md_a_1err.R"
  expect_error(esl(md=getmd(mdf), rnd = F, showFirstRows = F))
}) # EOT

test_that("esl - all good numbers", { 
  mdf <- "md_a_1.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows =T)), 9)
  mdf <- "md_a_2.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F)), 21)
  mdf <- "md_a_3.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = T, showFirstRows = F)), 33)
  mdf <- "md_b_1.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F)), 42)
  mdf <- "md_b_2.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F, timeEstimate = T)), 105)
  mdf <- "md_b_3.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F)), 105)
  mdf <- "md_c_1.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F)), 432)
  mdf <- "md_c_3.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F, timeEstimate = TRUE)), 2688)
  mdf <- "md_KCl.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F)), 240)
  mdf <- "md_d_1.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F, timeEstimate = TRUE)), 1728)
  mdf <- "md_d_2.r"
  expect_equal(nrr(esl(md=getmd(mdf), rnd = F, showFirstRows = F)), 1296)
}) # EOT

test_that("esl - multiply rows", { 
  mdf <- "md_a_2.r"
  expect_equal(nrr(esl(multiplyRows=TRUE, md=getmd(mdf), rnd = F, showFirstRows = F)), 105)
}) # EOT



##################### clean up ###################### 
## delete all files in td metadata
allinMd <- list.files(paste0(expHomeF, "/metadata"), full.names = TRUE)
ok <- file.remove(allinMd)
