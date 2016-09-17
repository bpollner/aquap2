noi_calculateNoiseDistribution <- function(noiseDataset) { # is called from gdmm; if no noise, the noiseDataset comes in as NULL
	if (!is.null(noiseDataset)) { # so we want to do some noise
		if (!exists("noiseDist", where=.ap2)) {
			if (!.ap2$stn$allSilent) {cat("Calculating specific noise distribution...")}	
			## calculate noise distribution
			hullValues <- apply(noiseDataset$NIR, 2, range) # gives back a matrix with 2 rows and ncol(NIR) columns with the lower values in the first row


			
#			setClass("aquap_noiseDist", slots=c(timestamp="POSIXct", version="character"), contains="matrix")			
			noiseDist <- c(1,2,3,4)
#			noiseDist <- new("aquap_noiseDist", XX)
			assign("noiseDist", noiseDist, pos=.ap2) 
			if (!.ap2$stn$allSilent) {cat("ok\n")}
		} # end if !exists noiseDist
	} # end if !is.null noiseDataset
} # EOF


### CORE ### is called in file: cube_generateDatasets.r
noi_performNoise <- function(dataset)	{

	noiseLevel <- .ap2$stn$noi_noiseLevel
	nr <- nrow(dataset$NIR)
	nc <- ncol(dataset$NIR)
	noise <- matrix(rnorm(nr*nc, mean=1, sd=noiseLevel), nr, nc)
	dataset$NIR <- dataset$NIR * noise
	return(dataset)
} # EOF



#' @title Record and apply noise
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
#' noise to each individual dataset within the cube.
#' @seealso \code{\link{gdmm}}
#' @family Noise procedures
#' @name noise_procedures
NULL
