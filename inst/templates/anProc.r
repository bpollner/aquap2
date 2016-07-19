####################################################################
############### aquap2 analysis procedure file #####################
####################################################################

### do NOT change the names of the variables !!  ####

	################################################################
	##################### split dataset ############################
	################################################################
	spl.var <- NULL							## by which variables should the dataset be split? leave empty c() or set to NULL for *no* splitting by class variables
#	spl.var <- c("C_Group", "C_Mode")

	spl.wl <- NULL							## which wavelengths to use? leave empty c() or set to NULL for using all available wavelengths
#	spl.wl <- "1300-to-1600"
#	spl.wl <- c("1000-to-2000", "1300-to-1600")
	#######
	dpt.pre <- NULL							## Character vector, which of the available modules of data pre-treatments to apply. Leave at NULL for no additional data pre-treatment. For possible values see XXX.
	#######
	spl.do.csAvg <- FALSE					## if all the consecutive scans of a single sample should be reduced, i.e. averaged into a single spectrum
	spl.csAvg.raw <- TRUE					## if, should the consecutive scans of a single sample be reduced, an other dataset containing every single consecutive scan should be kept as well

	spl.do.noise <- FALSE					## if artificial noise should be added to the dataset
	spl.noise.raw <- TRUE					## if, should the noise-test be performed, the raw original data should be used as well. Recommended value = TRUE

	spl.do.exOut <- FALSE					## if exclusion of outliers should be performed
	spl.exOut.raw <- TRUE					## if, should exclusion of outliers be performed, the raw original data should be used as well; if set to TRUE, outliers will be flagged in the dataset in any case.
	spl.exOut.var <- c("C_Group", "C_Water")## the variables (one or more) to be used for the grouping defining the scope for outlier detection.
	#######
	dpt.post <- NULL						## Character vector, which of the available modules of data pre-treatments to apply.  Leave at NULL for no additional data pre-treatment. For possible values see XXX.
	################################################################
			





	################################################################
	########## statistics & specific plotting options ##############
	################################################################

	#########################
	### PCA 
	do.pca <- FALSE							## if PCA of the given datasets should be performed
	
	# plotting 
	pca.colorBy <- NULL						## which variables to use for coloring the score plots. leave empty c() or set to NULL for using all available class-variables for coloring 
#	pca.colorBy <- c("C_Group", "C_Temp", "C_RelHum")
	pca.elci <- "def"						## the confidence interval for the ellipse to draw around groups in score plots; leave at 'def' to read in the default from the settings.r file; provide a numeric length one (e.g. 0.95); or set to NULL for not drawing ellipses at all
	pca.elcolorBy <- NULL					## which variables to use for plotting additional confidence intervall ellipses. Set to NULL for not drawing additional CI-ellipses. Provide one variable (gets recycled) or a vector with equal length as pca.colorBy to have the additional ci-ellipses along these variables.
#	pca.elcolorBy <- "C_RelHum"
	pca.what <- "both"						## can be either 'both' for plotting both PCA scores and loadings, or 'scores' or 'loadings' for plotting only one of them.
	pca.sc <- c(1, 2)						## Numeric length 2, the two PCs to be plotted against each other in the score plots.
	pca.sc.pairs <- 1:6 					## Numeric sequence of length >=2, indicating what PC components to plot in the score pairs plot. Set to NULL for not plotting the pairs plot.
	pca.lo <- 1:6							## Numeric sequence of length >=2, indicating what PC components to plot in the loadingplot.
	#########################
	 
	 
	#########################
	### SIMCA
	do.sim <- FALSE							## if SIMCA models of the given datasets should be calculated
	sim.vars <- NULL						## which variables should be use to group the data? Leave empty c() or set to NULL for using all available class-variables
#	sim.vars <- c("C_Group")
	sim.K <- 0								## The number of components. In mode "robust", leave at 0 for automatic detection. (it is a capital "K")
	
	# plotting
	
	#########################


	#########################
	### PLSR
	do.pls <- FALSE							## if PLSR models of the given datasets should be calculated
	pls.regOn <- NULL						## which variables should be used to regress on? Leave empty c() or set to NULL for using all numerical variables
#	pls.regOn <- c("Y_Temp")
	pls.ncomp <- NULL						## number of components, leave at NULL for automatic detection of optimal number of components
	pls.valid <- "CV"						## "CV" will perform a 10-fold crossvalidation, "LOO" a leave-one-out crossvalidation
	
	# plotting
	pls.colorBy <- NULL						## What class-variable should be used for coloring in the RMSEP and RMSECV plots? Leave empty c() or set to NULL for no coloring
#	pls.colorBy <- "C_Temp"
	#########################


	#########################
	### Aquagram
	do.aqg <- FALSE							## if Aquagrams of the given datasets should be calculated
	aqg.vars <- NULL						## which variables should be use to group the data? Leave empty c() or set to NULL if you want to decide later
#	aqg.vars <- c("C_Group")
	aqg.nrCorr <- "def"						## please see the help for ?anproc_file (and ?gdmm) for further information regarding these arguments.
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
	
	################################################################
	


	################################################################
	################ general plotting options ######################
	################################################################
	pg.where <- "def"						## If left at the default 'def', the value from the settings.r file is read in (parameter gen_plot_pgWhereDefault). For plotting PDFs provide "pdf", for plotting to graphics device provide anything but "pdf".
	pg.main <- ""							## the additional text on the title of each single plot
	pg.sub <- ""							## the additional text on the subtitle of each single plot
	pg.fns <- ""							## 'filename suffix', the additional text in the filename of the pdf.
	################################################################

### do NOT change the names of the variables !!  ####
####################################################################
