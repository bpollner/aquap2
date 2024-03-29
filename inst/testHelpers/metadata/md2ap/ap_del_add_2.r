####################################################################
############### aquap2 analysis procedure file #####################
####################################################################

### do NOT change the names of the variables !!  ####

################################################################
##################### split dataset ############################
################################################################
spl.var <- NULL							## by which variables should the dataset be split? leave empty c() or set to NULL for *no* splitting by class variables, or provide one or more valid class variables to split along these variables; e.g. "C_Group", or c("C_Group", "C_Mode", ...)
spl.wl <- NULL							## which wavelengths to use? leave empty c() or set to NULL for using all available wavelengths. Provide a character vector in the format e.g. c("1300-to-1600"), or c("1000-to-2000", "1300-to-1600", ...) to split along these wavelengths.
#######
dpt.pre <- NULL							## Character vector, which of the available modules of data pre-treatments to apply. Leave at NULL for no additional data pre-treatment. For possible values see ?dpt_modules.
#######
spl.do.csAvg <- FALSE					## if all the consecutive scans of a single sample should be reduced, i.e. averaged into a single spectrum
spl.csAvg.raw <- TRUE					## if, should the consecutive scans of a single sample be reduced, an other dataset containing every single consecutive scan should be kept as well
#

#########################
### PLSR
do.pls <- FALSE							## if PLSR models of the given datasets should be calculated
pls.ncomp <- NULL						## number of components, leave at NULL for automatic detection of optimal number of components
pls.regOn <- NULL						## which variables should be used to regress on? Leave empty c() or set to NULL for using all numerical variables, or provide a character vector with valid numerical variable names to regress on these variables; e.g. "Y_Temp", or c("Y_Temp", "Y_FooBar", ...)
pls.valid <- "def"						## 'def' to get the default value from the settings file (parameter 'plsr_calc_typeOfCrossvalid'); a number to perform this n-fold crossvalidation, 'LOO' for a leave-one-out crossvalidation, or a valid name of a class variable for performing a crossvalidation based on the grouping defined by this variable. Can be of equal length as the vector in 'pls.regOn' to perform the corresponding type of CV for each element in 'pls.regOn'.
pls.exOut <- "def"						## 'def' to get the default value from the settings file (parameter 'plsr_calc_excludePlsrOutliers'); TRUE or FALSE for deciding whether to perform a plsr-specific outlier-detection algorithm or not.  Can be of equal length as the vector in 'pls.regOn' to perform the corresponding outlier-detection (or not) for each element in 'pls.regOn'.

# plotting
pls.colorBy <- NULL						## What class-variable should be used for coloring in the RMSEP and RMSECV plots? Leave empty c() or set to NULL for no coloring, or provide a character length one with a valid class variable name to color along this variable; e.g. "C_Group"
pls.what <- "both"						## what types of plots to plot. Possible values are "regression" for the regression vector, "errors" for the error plots, or "both" for both of them.
pls.rdp <- FALSE						## if errors should be given in RDP
#########################




#########################
### Aquagram
do.aqg <- FALSE							## if Aquagrams of the given datasets should be calculated
aqg.vars <- NULL						## which variables should be use to group the data? Provide a character vector with valid class variable names to group along these variables; e.g. "C_Group", or c("C_Group", "C_FooBar", ...)
aqg.nrCorr <- "def"						## please see the help for ?calc_aqg_args and ?plot_aqg_args for further information regarding these arguments.
aqg.spectra <- FALSE
aqg.minus <- NULL
aqg.mod <- "def"  
aqg.TCalib <- "def"
aqg.Texp <- "def"
aqg.bootCI <- "def"
aqg.R <- "def"
aqg.smoothN <- 21
aqg.selWls <- "def"
aqg.msc <- TRUE
aqg.reference <- NULL

# plotting
aqg.fsa <- 	"only"						## 'Fix scale for Aquagram'. Logical, numeric or character.
aqg.fss <- 	"only"						## 'Fix scale for spectra'. Logical, numeric or character.
aqg.ccol <- NULL						## custom color vector 
aqg.clt <- "def"						## custom line-type
aqg.pplot <- "def" 						## if peak plot should be added to the spectra
aqg.plines <- "def"						## if "lines" XXX should be added to the peak plot
aqg.discr <- "def"						## Logical or character 'def'. If set to TRUE, negative (resp. positive) peaks can be only found in peak-heights below (resp. above) zero.
#########################	




####################################################################




####################################################################
########## classification & classif. plotting options ##############
####################################################################

#########################
### Discriminant Analysis
do.da <- FALSE							## if DA should be performed
da.type <- c("fda")						## the type of discriminant analysis to perform. Provide a single character or a character vector, for possible values please see ?calc_discrimAnalysis_args.
da.classOn <- c("foo")					## what class-variables (one or more) to use for grouping the data
da.testCV <- TRUE						## if the errors of the test-data should be crossvalidated. If set to true, CV and testing is repeated in alternating datasets. See below.
da.percTest <- 30						## The percentage of the dataset that should be set aside for testing the models; these data are never seen during training and crossvalidation.
da.cvBootCutoff <- 15					## The minimum number of observations that should be in the smallest subgroup (as defined by the classification grouping variable) *AFTER* the split into ".valid" crossvalidation segments (below). Please see ?calc_discrimAnalysis_args for further information.
da.cvBootFactor <- 1					## The factor used to multiply the number of observations within the smallest subgroup defined by the classification grouping variable with, resulting in the number of iterations of a possible bootstrap crossvalidation -- see above.
da.valid <- 8							## The number of segments the training data should be divided into in case of a traditional crossvalidation of the training data; see above.
da.pcaRed <- TRUE						## If variable reduction via PCA should be applied; if TRUE, the subsequent classifications are performed on the PCA scores, see below.
da.pcaNComp <- c("max")					## Provide the character "max" to use the maximum number of components (i.e. the number of observations minus 1), or an integer vector specifying the components resp. their scores to be used for DA.




#########################
### Random Forest
do.rnf <- FALSE							## if random forests should be performed
rnf.classOn <- c("foo")					## what class-variables (one or more) to use for grouping the data
rnf.testCV <- TRUE						## if the errors of the test-data should be crossvalidated. If set to true, CV and testing is repeated in alternating datasets. See below.
rnf.percTest <- 30						## The percentage of the dataset that should be set aside for testing the models; these data are never seen during training and crossvalidation.
rnf.cvBootCutoff <- 15					## The minimum number of observations that should be in the smallest subgroup (as defined by the classification grouping variable) *AFTER* the split into ".valid" crossvalidation segments (below). Please see ?calc_randomForest_args for further information.
rnf.cvBootFactor <- 1					## The factor used to multiply the number of observations within the smallest subgroup defined by the classification grouping variable with, resulting in the number of iterations of a possible bootstrap crossvalidation -- see above.
rnf.valid <- 8							## The number of segments the training data should be divided into in case of a traditional crossvalidation of the training data; see above.
rnf.pcaRed <- TRUE						## If variable reduction via PCA should be applied; if TRUE, the subsequent classifications are performed on the PCA scores, see below.
rnf.pcaNComp <- c("max")				## Provide the character "max" to use the maximum number of components (i.e. the number of observations minus 1), or an integer vector specifying the components resp. their scores to be used for DA.



#########################
### Support Vector Machine
do.svm <- FALSE							## if svm classification should be performed
svm.classOn <- c("foo")					## what class-variables (one or more) to use for grouping the data
svm.testCV <- TRUE						## if the errors of the test-data should be crossvalidated. If set to true, CV and testing is repeated in alternating datasets. See below.
svm.percTest <- 30						## The percentage of the dataset that should be set aside for testing the models; these data are never seen during training and crossvalidation.
svm.cvBootCutoff <- 15					## The minimum number of observations that should be in the smallest subgroup (as defined by the classification grouping variable) *AFTER* the split into ".valid" crossvalidation segments (below). Please see ?calc_SVM_args for further information.
svm.cvBootFactor <- 1					## The factor used to multiply the number of observations within the smallest subgroup defined by the classification grouping variable with, resulting in the number of iterations of a possible bootstrap crossvalidation -- see above.
svm.valid <- 8							## The number of segments the training data should be divided into in case of a traditional crossvalidation of the training data; see above.
svm.pcaRed <- TRUE						## If variable reduction via PCA should be applied; if TRUE, the subsequent classifications are performed on the PCA scores, see below.
svm.pcaNComp <- c("max")				## Provide the character "max" to use the maximum number of components (i.e. the number of observations minus 1), or an integer vector specifying the components resp. their scores to be used for DA.



#########################
### Neural Network
do.nnet <- FALSE						## if svm classification should be performed
nnet.classOn <- c("foo")				## what class-variables (one or more) to use for grouping the data
nnet.testCV <- TRUE						## if the errors of the test-data should be crossvalidated. If set to true, CV and testing is repeated in alternating datasets. See below.
nnet.percTest <- 30						## The percentage of the dataset that should be set aside for testing the models; these data are never seen during training and crossvalidation.
nnet.cvBootCutoff <- 15					## The minimum number of observations that should be in the smallest subgroup (as defined by the classification grouping variable) *AFTER* the split into ".valid" crossvalidation segments (below). Please see ?calc_NNET_args for further information.
nnet.cvBootFactor <- 1					## The factor used to multiply the number of observations within the smallest subgroup defined by the classification grouping variable with, resulting in the number of iterations of a possible bootstrap crossvalidation -- see above.
outout <- 4
nnet.valid <- 8							## The number of segments the training data should be divided into in case of a traditional crossvalidation of the training data; see above.
nnet.pcaRed <- TRUE						## If variable reduction via PCA should be applied; if TRUE, the subsequent classifications are performed on the PCA scores, see below.
nnet.pcaNComp <- c("max")				## Provide the character "max" to use the maximum number of components (i.e. the number of observations minus 1), or an integer vector specifying the components resp. their scores to be used for DA.
gogo <- 2



####################################################################


whyHere <- TRUE
dontLIke <- 3
notNIce <- 22




hasToBo <- TRUE
theLastAlso <- 5
final <- FALSE


####################################################################

### do NOT change the names of the variables !!  ####
####################################################################
