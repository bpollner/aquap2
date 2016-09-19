noi_calculateNoiseDistribution <- function(noiseDataset, noiseFileName) { # is called from gdmm; if no noise, the noiseDataset comes in as NULL
	if (!is.null(noiseDataset)) { # so we want to do some noise
		if (!exists("noiseDist", where=.ap2)) {
			if (!.ap2$stn$allSilent) {cat(paste0("Calculating specific noise distribution from noise-data file '", noiseFileName, "'..."))}	
			###
			hullValues <- apply(noiseDataset$NIR, 2, range) # gives back a matrix with 2 rows and ncol(NIR) columns with the lower values in the first row
			rownames(hullValues) <- c("lower", "higher")
			wls <- getWavelengths(noiseDataset)
			noiseDist <- list(hull=hullValues, wls=wls)
			###
			assign("noiseDist", noiseDist, pos=.ap2) # we could hand it over in the functions, but we require this to be done only once the first time
			if (!.ap2$stn$allSilent) {cat("ok\n")}
		} # end if !exists noiseDist
	} # end if !is.null noiseDataset
} # EOF


### CORE ### is called in file: cube_generateDatasets.r
noi_performNoise <- function(dataset)	{ # is working on a single dataset, i.e. within each single element of the cube
	if (!exists("noiseDist", where=.ap2)) {
		stop("Sorry, an error with calculating the noise distribution appeared. Please restart the R-process.", call.=FALSE)
	}
	aa <- .ap2$noiseDist
	noiseHull <- aa$hull
	noiseWls <- aa$wls
	dataWls <- getWavelengths(dataset)
	if (!all(dataWls %in% noiseWls)) { # noiseWls is always ALL of them
		stop(paste0("The wavelengths of the dataset do not match the available wavelengths in the noise-data. \nPlease check applicability of the noise-data."), call.=FALSE)
	}
	selInd <- which(noiseWls %in% dataWls) # probably we need to cut down the noise-distribution data to match the wavelengths in the current dataset
	noiseHull <- noiseHull[, selInd]
	#
	nr <- nrow(dataset$NIR)
	nc <- ncol(dataset$NIR)
	noiseMat <- matrix(NA, nr, nc)
	for (i in 1: nrow(noiseMat)) {	
		noiseMat[i,] <-apply(noiseHull, 2, function(x) x[sample(c(1,2),1)]) # select from columns from noiseHull and fill into noise matrix
	}
#	print("----"); 	print(noiseHull[,1:5]); print(noiseMat[1:50, 1:5]) ; wait()
	dataset$NIR <- dataset$NIR + noiseMat
	return(dataset)
} # EOF


#' @title Generate noise recording experiment
#' @description Generate the folder structure for a new experiment and populate 
#' it with the metadata suggested for recording then the noise-spectra.
#' @details This generates the folder structure for a standard experiment and is 
#' adapting the metadata to record 12 noise-spectra with each 3 consecutive scans.
#' For a possible workflow please see examples.
#' @examples
#' \dontrun{
#' genNoiseRecExp() # generate the folder structure in the current working 
#' # directory
#' esl() # export the sample list
#' #### now record the noise-spectra #### (move sample list to folder 'sl_in')
#' gfd <- gfd() # imports noise raw-data and saves noise-data file in the 
#' # R-data folder, from where you take it and move it into your 
#' # AQUAP2SH folder
#' }
#' @seealso \code{\link{noise_procedures}}, \code{\link{genFolderStr}}
#' @family Helper Functions
#' @family Noise procedures
#' @export
genNoiseRecExp <- function() {
	genFolderStr()
	fn_metadata <- .ap2$stn$fn_metadata # folder name for metadata
	fn_mDataDefFile <- .ap2$stn$fn_mDataDefFile
	deleteCol <- .ap2$stn$p_deleteCol
	clPref <- .ap2$stn$p_ClassVarPref
	pathMd <- paste(fn_metadata, fn_mDataDefFile, sep="/")
	con <- file(pathMd, open="rt")
	txt <- readLines(con)
	close(con)
	txt[grep("expName", txt)] <- "\texpName <- \"NoiseData\""
	txt[grep("noiseFileName", txt)] <- "\tnoiseFileName <- \"not_applicable\""
	txt[grep("TimePoints", txt)] <- "\tTimePoints <- FALSE"	
	txt[grep("spacing", txt)] <- "\tspacing <- FALSE"
	txt[grep("columnNamesL1", txt)] <- paste0("\tcolumnNamesL1 <- \"", clPref, deleteCol,"\"")
	txt[grep("columnNamesL2", txt)] <- paste0("\tcolumnNamesL2 <- \"", clPref, deleteCol,"\"")
	txt[grep("L1  <-", txt)] <- "\tL1 <- list(list(\"\"))"
	txt[grep("L2  <-", txt)] <- "\tL2 <- list(list(\"\"))"
	txt[grep("Repls", txt)] <- "\tRepls <- 12"
	txt[grep("Group", txt)] <- "\tGroup <- \"noise\""
	con <- file(pathMd, open="wt")
	writeLines(txt, con)
	close(con)
#	esl()
	return(invisible(NULL))
} # EOF


#' @title Record / add noise
#' @description Record a special noise-data file on your spectroscopy device, and 
#' use this file to calculate data that can then be used to specifically add 
#' 'custom' noise to your datasets.
#' @details The noise data file is an R-data file generated / imported via the 
#' \code{\link{gfd}}) function, but containing only noise-spectra recorded 
#' following the recommendations below. This R-data file (where the factory-default 
#' filename is 'NoiseData') is then moved \strong{by the user} in to the AQUAP2SH 
#' folder. When generating datasets (a 'cube') using the function 
#' \code{\link{gdmm}}, it depends on whether you choose to add noise to the 
#' datasets or not (parameter \code{spl.do.noise} in the analysis procedure resp. 
#' your input. If, and only if, you choose to add noise, first (and only once per 
#' R-session) the noise R-data file is read in and used to calculate the specific 
#' noise distribution. This noise distribution is then used to specifically add 
#' noise to each single wavelength in the individual dataset within the cube.
#' @section Procedure: The procedure to work with noise-data files and use them to 
#' obtain a wavelength-specific noise distribution consists of the following 
#' steps:
#' \describe{
#' \item{Record noise spectra}{Use the function XXX to generate a folder structure
#' for an experiment, then record some noise-spectra, for what the following 
#' procedure is suggested: 
#' XXX. 
#' Finally, use \code{\link{gfd}} to import the noise-data and create the R-data 
#' file (in the folder 'R-data' in the working directory of the experiment).}
#' \item{Move R-data file}{Move the resulting R-data file containing the noise-
#' spectra from the R-data folder into your \code{AQUAP2SH} folder, i.e. the 
#' folder also containing e.g. the settings.r file.}
#' \item{Choose to add noise to dataset}{In your actual experiment, in order to 
#' add noise to the dataset(s) you have to set the parameter \code{spl.do.noise} 
#' in the analysis procedure to \code{TRUE}. Alternatively, you can override the 
#' value from 'spl.do.noise' in the analysis procedure via the \code{...} 
#' argument in \code{\link{getap}} in \code{\link{gdmm}} - see examples.}
#' \item{Specify noise-data file}{In your actual experiment, specify the name of 
#' the file (residing in the folder \code{AQUAP2SH}) containing the noise-spectra 
#' either in the metadata or at the argument \code{noiseFile} in the function 
#' \code{\link{gdmm}} - see examples.}
#' \item{Evaluate difference}{Use any of the provided methods to check if there 
#' is a difference between the datasets without and with added noise.}
#' }
#' @section Noise distribution and adding noise -- the math: The noise distribution is 
#' calculated automatically once should it be required, and stored in the object
#' \code{noiseDist} in the environment \code{.ap2}; (look at it via 
#' \code{str(.ap2$noiseDist)}). It simply represents the range of absorbance 
#' values within each single wavelength over all observations of the noise dataset.
#' If noise should be added now to a dataset, for each single observation and 
#' there for each single wavelength a random process is selecting one of the two 
#' values from the corresponding wavelength in the noise distribution to be added 
#' to the original NIR data. In other words, for each single wavelength the 
#' original NIR-data absorbance values get increased or reduced by the maximum 
#' of (positive or negative) noise occuring at this specific wavelength.
#' @seealso \code{\link{gdmm}}, \code{\link{settings_file}}, 
#' \code{\link{genFolderStr}}
#' @family Noise procedures
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' cu <- gdmm(fd, getap(spl.do.noise=TRUE))
#' cu <- gdmm(fd, getap(spl.do.noise=TRUE), noiseFile="FooBar") # instead of the 
#' # default noise-data filename resp. the filename specified in the metadata 
#' # file, use instead the noise-data file named 'FooBar'.
#' plot_pca(cu)
#' }
#' @name noise_procedures
NULL
