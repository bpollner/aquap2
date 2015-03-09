####################################################################
############### aquap2 analysis procedure file #####################
####################################################################

### do NOT change the names of the variables !!  ####

	###########################################
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
	###########################################
			
			
	#########################
	## PCA
	do_PCA <- FALSE
	pca_colorByVariable <- NULL				## which variables to use for coloring the score plots. leave empty c() or set to NULL for using all available class-variables for coloring 
#	pca_colorByClass <- c("C_Group", "C_Temp", "C_RelHum")
	#########################
	 

	#########################
	## SIMCA
	do_SIMCA <- FALSE
	simca_variables <- NULL					## which variables should be use to group the data? Leave empty c() or set to NULL for using all available class-variables
#	simca_variables <- c("C_Group")
	simca_K <- 0							## The number of components. In mode "robust", leave at 0 for automatic detection.
	#########################


	#########################
	## PLSR
	do_PLSR <- FALSE
	plsr_regressOn <- NULL					## which variables should be used to regress on? Leave empty c() or set to NULL for using all numerical variables
#	plsr_regressOn <- c("Y_Temp")
	plsr_ncomp <- NULL						## number of components, leave at NULL for automatic detection of optimal number of components
	plsr_valid <- "CV"						## "CV" will perform a 10-fold crossvalidation, 
#	plsr_valid <- "LOO" 					## "LOO": leave-one-out crossvalidation
	plsr_classForColoring <- NULL			## What class-variable should be used for coloring in the RMSEP and RMSECV plots? Leave empty c() or set to NULL for no coloring
#	plsr_classForColoring <- "C_Temp"
	#########################


	#########################
	## Aquagram
	aquagram_variables <- NULL				## which variables should be use to group the data? Leave empty c() or set to NULL if you want to decide later
#	aquagram_variables <- c("C_Group")
	#########################

### do NOT change the names of the variables !!  ####
####################################################################
