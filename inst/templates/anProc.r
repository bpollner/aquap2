####################################################################
############### aquap2 analysis procedure file #####################
####################################################################

### do NOT change the names of the variables !!  ####

	################################################################
	##### split dataset #####
	splitByVariable <- NULL					## by which variables should the dataset be split? leave empty c() or set to NULL for *no* splitting by class variables
#	splitByVariable <- c("C_Group", "C_Mode")

	splitByWavelength <- NULL				## which wavelengths to use? leave empty c() or set to NULL for using all available wavelengths
#	splitByWavelength <- "1300-to-1600"
#	splitByWavelength <- c("1000-to-2000", "1300-to-1600")
	 
	do_smooth <- FALSE						## if smoothing of dataset should be performed
	smooth_useRaw <- TRUE					## if, should smoothing be performed, the raw unsmoothed data should be used as well

	do_noiseTest <- FALSE					## if artificial noise should be added to the dataset
	noiseTest_useRaw <- TRUE				## if, should the noise-test be performed, the raw original data should be used as well. Recommended value = TRUE
	################################################################
			


	################################################################
	##### statistics ########
	#########################
	## PCA
	do.pca <- FALSE							## if PCA of the given datasets should be performed
	pca.colorBy <- NULL						## which variables to use for coloring the score plots. leave empty c() or set to NULL for using all available class-variables for coloring 
#	pca.colorBy <- c("C_Group", "C_Temp", "C_RelHum")
	#########################
	 

	#########################
	## SIMCA
	do.sim <- FALSE							## if SIMCA models of the given datasets should be calculated
	sim.vars <- NULL						## which variables should be use to group the data? Leave empty c() or set to NULL for using all available class-variables
#	sim.vars <- c("C_Group")
	sim.K <- 0								## The number of components. In mode "robust", leave at 0 for automatic detection. (it is a capital "K")
	#########################


	#########################
	## PLSR
	do.pls <- FALSE							## if PLSR models of the given datasets should be calculated
	pls.regOn <- NULL						## which variables should be used to regress on? Leave empty c() or set to NULL for using all numerical variables
#	pls.regOn <- c("Y_Temp")
	pls.ncomp <- NULL						## number of components, leave at NULL for automatic detection of optimal number of components
	pls.valid <- "CV"						## "CV" will perform a 10-fold crossvalidation, "LOO" a leave-one-out crossvalidation
	pls.colorBy <- NULL						## What class-variable should be used for coloring in the RMSEP and RMSECV plots? Leave empty c() or set to NULL for no coloring
#	pls.colorBy <- "C_Temp"
	#########################


	#########################
	## Aquagram
	do.aqg <- FALSE							## if Aquagrams of the given datasets should be calculated
	aqg.vars <- NULL						## which variables should be use to group the data? Leave empty c() or set to NULL if you want to decide later
#	aqg.vars <- c("C_Group")
	aqg.nrCorr <- "def"						## please see the help for ?anproc_file (and ?gdmm) for further information regarding these arguments.
	aqg.spectra <- FALSE
	aqg.minus <- NULL
	aqg.mod <- "def"  	## new def here!!!
	aqg.TCalib <- "def"
	aqg.Texp <- "def"
	aqg.bootCI <- "def"
	aqg.R <- "def"
	aqg.smoothN <- 21
	aqg.selWls <- "def"
	aqg.msc <- TRUE
	aqg.reference <- NULL
	#########################	
	################################################################


### do NOT change the names of the variables !!  ####
####################################################################
