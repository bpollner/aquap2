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
################################################################
		
		


################################################################
########## statistics & specific plotting options ##############
################################################################





#########################
### Aquagram
do.aqg <- FALSE							## if Aquagrams of the given datasets should be calculated
aqg.vars <- NULL						## which variables should be use to group the data? Provide a character vector with valid class variable names to group along these variables; e.g. "C_Group", or c("C_Group", "C_FooBar", ...)
aqg.nrCorr <- "def"						## please see the help for ?calc_aqg_args and ?plot_aqg_args for further information regarding these arguments.
aqg.spectra <- FALSE
aqg.minus <- NULL
aqg.mod <- "def"  
aqg.TCalib <- "def"
toGo <- FALSE
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
nnet.valid <- 8							## The number of segments the training data should be divided into in case of a traditional crossvalidation of the training data; see above.
nnet.pcaRed <- TRUE						## If variable reduction via PCA should be applied; if TRUE, the subsequent classifications are performed on the PCA scores, see below.
nnet.pcaNComp <- c("max")				## Provide the character "max" to use the maximum number of components (i.e. the number of observations minus 1), or an integer vector specifying the components resp. their scores to be used for DA.
isOnTheBlockEnd <- TRUE
andOnemore <- TRUE
andOneOneMore <- FALSE 



####################################################################




####################################################################
################## general plotting options ########################
####################################################################
pg.where <- "pdf"						## If left at the default 'def', the value from the settings.r file is read in (parameter gen_plot_pgWhereDefault). For plotting PDFs provide "pdf", for plotting to graphics device provide anything but "pdf".
pg.main <- ""							## the additional text on the title of each single plot
pg.sub <- ""							## the additional text on the subtitle of each single plot
pg.fns <- ""							## 'filename suffix', the additional text in the filename of the pdf.
####################################################################

### do NOT change the names of the variables !!  ####
####################################################################
