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
