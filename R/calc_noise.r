noi_calculateNoiseDistribution <- function(noiseDataset, noiseFileName) { # is called from gdmm; if no noise, the noiseDataset comes in as NULL
	stn <- getstn()
	# noiseDataset also comes in as NULL when we are having the static noise mode, is checked in the checking function when calling gdmm
	if (!is.null(noiseDataset)) { # so we want to do some noise, and do not have the static mode
		noiseDistName <- paste0(pv_noiseDistPrefix, noiseFileName)
		if (!exists(noiseDistName, where=gl_ap2GD)) {
			noiMode <- stn$noi_addMode 		## pv_noiseAddModes <- c("sdNorm", "sdUnif", "extrema", "static")
			if (!stn$allSilent) {cat(paste0("Calculating specific noise distribution from noise-data file '", noiseFileName, "' with ",noiMode , " method..."))}	
			aa <- pv_noiseAddModes
			if (noiMode == aa[1] | noiMode == aa[2]) { # so we want the sd method
				meanV <- apply(noiseDataset$NIR, 2, mean)
				sdV <- apply(noiseDataset$NIR, 2, sd)
				lower <- meanV - sdV
				higher <- meanV + sdV
				confInt <- matrix(c(lower, higher), nrow=2, byrow=TRUE)
				colnames(confInt) <- colnames(noiseDataset$NIR)
				rownames(confInt) <- c("lower", "higher")
				hullValues <- apply(noiseDataset$NIR, 2, range) # gives back a matrix with 2 rows and ncol(NIR) columns with the lower values in the first row
				names(meanV) <- names(sdV) <- NULL
			} else {	# so it must be the extrama method
				hullValues <- apply(noiseDataset$NIR, 2, range) # gives back a matrix with 2 rows and ncol(NIR) columns with the lower values in the first row
				meanV <- sdV <- confInt <-  NULL
			} # end if
			###
			rownames(hullValues) <- c("lower", "higher")
			wls <- getWavelengths(noiseDataset)
			noiseDist <- list(hull=hullValues, confInt=confInt, meanV=meanV, sdV=sdV, wls=wls)
			###
			assign(noiseDistName, noiseDist, pos=gl_ap2GD) # we could hand it over in the functions, but we require this to be done only once the first time
			if (!stn$allSilent) {cat("ok\n")}
		} # end if !exists noiseDist
	} # end if !is.null noiseDataset
} # EOF

test_plotNoiseData <- function(doTest=FALSE, noiseFile) {
	stn <- getstn()
	if (doTest) {
		C_outlier_all <- NULL
		ss <- stn$noi_sampleSize
		modus <- stn$noi_addMode
		pathSH <- Sys.getenv("AQUAP2SH")
		noisePath <- paste(pathSH, noiseFile, sep="/")
		noiDataset <- eval(parse(text=load(noisePath)))
		noiDataset@metadata$meta$expName <- paste0("Noise Data (", noiseFile, ")")
		if (modus == "sdNorm") {
			halfSdAdd <- paste0("halfSD = ", stn$noi_sdNormHalfOnly)
		} else {
			halfSdAdd <- ""
		}
		if (modus == "extrema") {
			ssAdd <- ""
		} else {
			ssAdd <- paste0("sample size = ", ss, ", ")
		}
		subT <- paste0("mode = ", modus, ssAdd, halfSdAdd)
#		print(subT)
		plot_spectra(ssc(noiDataset, C_outlier_all == FALSE), pg.where="", pg.sub="outliers excluded ")
		mtext(subT, side=1, line=2)
		assign(".poolCnt", 0, pos=gl_ap2GD) # for testing the pool data
		#
	}
} # EOF

test_plotMaxima <- function(doTest=FALSE, wls, noiseHull) {
	if (doTest) {
		lines(wls, noiseHull[1,], col="red", lty=1)
		lines(wls, noiseHull[2,], col="blue", lty=1)
	}
} # EOF

test_plotSDValues <- function(doTest=FALSE, nodi) {
	if (doTest) {
		wls <- nodi$wls
		meanV <- nodi$meanV
		confInt <- nodi$confInt
		lines(wls, confInt[1,], col="purple", lty=1, lwd=2)
		lines(wls, confInt[2,], col="purple", lty=1, lwd=2)
		lines(wls, meanV, col="green", lty=1, lwd=2)
	}
} # EOF

test_plotSinglePool <- function(doTest=FALSE, swl, pool, nc, out) {
	stn <- getstn()
	if (doTest) {
		if (get(".poolCnt", pos=gl_ap2GD) < nc) {
			n <- length(pool)
			x <- rep(swl, n)
			y <- pool
			points(x, y, col="gray")
		#	points(swl, out, col="red", pch=3)
			assign(".poolCnt", get(".poolCnt", pos=gl_ap2GD)+1, pos=gl_ap2GD)
		}
	} # end if toTest
} # EOF

test_plotSingleResult <- function(doTest=FALSE, wls, siRow) {
	if (doTest) {
		lines(wls, siRow, col="red", lwd=0.7)
	}
} # EOF

test_plotSingleResult_static <- function(doTest=FALSE, wls, siRow) {
	stn <- getstn()
	if (doTest) {
		statSd <- stn$noi_staticValue
		lines(wls, siRow, col="red", lwd=0.7)
		abline(h=0, col="black", lwd=0.7)
		abline(h=c(-statSd, statSd), col="black", lty=2, lwd=0.7)
	}
} # EOF

test_plotStaticNoise <- function(doTest=FALSE, nc) {
	stn <- getstn()
	if (doTest) {
		statSd <- stn$noi_staticValue
		ss <- stn$noi_sampleSize
		ti <- 3
		subT <- paste0("sample size = ", ss, ", static sd = ", statSd)
		plot(0, xlim=c(0, nc), ylim=c(-(ti*statSd), ti*statSd), ylab="random noise value", col="white", sub=subT, main="Static Noise")
		assign(".poolCnt", 0, pos=gl_ap2GD) # for testing the pool data
	}
}

### CORE ### is called in file: cube_generateDatasets.r
noi_performNoise <- function(dataset, noiseFile)	{ # is working on a single dataset, i.e. within each single element of the cube
	stn <- getstn()
	noiMode <- stn$noi_addMode 
	sampSize <- stn$noi_sampleSize
	pvnad <- pv_noiseAddModes  # ## c("sdNorm", "sdUnif", "extrema", "static")
	nr <- nrow(dataset$NIR)
	nc <- ncol(dataset$NIR)
	noiseMat <- matrix(NA, nr, nc)
	doTest <- FALSE
	######
	if (noiMode == pvnad[4]) { # the static method
		statSd <- stn$noi_staticValue
		iw_static <- function(x) {
			pool <- rnorm(sampSize, mean=0, sd=statSd)
			out <- sample(pool, 1)
			test_plotSinglePool(doTest, swl=x, pool, nc, out)
			return(out)
		} # EOF
		test_plotStaticNoise(doTest, nc)
		for (i in 1: nrow(noiseMat)) {	
		#	noiseMat[i,] <- sapply(1:ncol(noiseMat), function(x) sample(rnorm(sampSize, mean=0, sd=statSd), 1) )
			noiseMat[i,] <- sapply(1:ncol(noiseMat), iw_static )
		}
		test_plotSingleResult_static(doTest, wls=1:ncol(noiseMat), siRow=noiseMat[1,])
		dataset$NIR <- dataset$NIR + noiseMat
		return(dataset) # exit here
	} # end if static
	#######
	test_plotNoiseData(doTest, noiseFile)
	#
	noiseDistName <- paste0(pv_noiseDistPrefix, noiseFile)
	if (!exists(noiseDistName, where=gl_ap2GD)) {
		stop("Sorry, an error with calculating the noise distribution appeared. Please restart the R-process.", call.=FALSE)
	}
	nodi <- get(noiseDistName, pos=gl_ap2GD) ##### get the noise distribution ####
	noiseHull <- nodi$hull
	noiseWls <- nodi$wls
	dataWls <- getWavelengths(dataset)
	if (!all(dataWls %in% noiseWls)) { # noiseWls is always ALL of them
		stop(paste0("\nThe wavelengths of the dataset do not match the available wavelengths in the noise-data. \nPlease check applicability of the noise-data."), call.=FALSE)
	}
	test_plotMaxima(doTest, noiseWls, noiseHull)
	test_plotSDValues(doTest, nodi)
	selInd <- which(noiseWls %in% dataWls) # probably we need to cut down the noise-distribution data to match the wavelengths in the current dataset
	noiseHull <- noiseHull[, selInd]
	noiseWls <- noiseWls[selInd]
	##
	halfOnly <- stn$noi_sdNormHalfOnly; 	if (!is.logical(halfOnly)) { halfOnly <- TRUE} ; 	if (halfOnly) {	div <- 2 } else { div <- 1 }
	#
	if (noiMode == pvnad[1] | noiMode == pvnad[2]) {  ## so we want the sd method	
		meanV <- matrix(nodi$meanV[selInd], nrow=1) # possibly have to cut them down
		sdV <- matrix(nodi$sdV[selInd], nrow=1)
		confInt <- nodi$confInt[, selInd] # is already a matrix
		nwls <- matrix(nodi$wls[selInd], nrow=1) # !! only for the testing functions !!
		noiseCompound <- rbind(noiseHull, confInt, meanV, sdV, nwls) ### a matrix with: 1st row the lower hull, 2nd the higher hull, 3rd the lower confInt, 4th the higher confInt, 5th the mean, 6th the sd; and 7th the wavelengths ####
		#
		iw_rnorm <- function(x, ss=sampSize, di=div) {
			pool <- rnorm(ss, mean=x[5], sd=(x[6]/di))
			ind <- which(pool < x[1] | pool > x[2] )
			if (length(ind) > 0) {
				pool <- pool[-ind] # kick out those that are beyond the maxima (in the first and second row of the compound) of our noise-data
			}
			out <- sample(pool, 1)
			test_plotSinglePool(doTest, swl=x[7], pool, nc, out)			
			return(out)
		} # EOIF
		iw_runif <- function(x, ss=sampSize) {
			pool <- runif(ss, min=x[3], max=x[4])
			out <- sample(pool, 1)
			test_plotSinglePool(doTest, swl=x[7], pool, nc, out)
			return(out)
		} # EOIF
		#
		if (noiMode == pvnad[1]) { # the sdNorm
			for (i in 1: nrow(noiseMat)) {
				noiseMat[i,] <- apply(noiseCompound, 2, iw_rnorm)
			}
		} else { # the sdUnif
			for (i in 1: nrow(noiseMat)) {
			#	noiseMat[i,] <- apply(noiseCompound, 2, function(x) sample(runif(sampSize, min=x[3], max=x[4]), 1) ) 
				noiseMat[i,] <- apply(noiseCompound, 2,  iw_runif) 
			}		
		} # end ifelse
	test_plotSingleResult(doTest, nwls, siRow=noiseMat[1,])
	} # end sd modes
	###
	if (noiMode == pvnad[3])	{ # the extrema method
		noiseCompound <- rbind(noiseHull, matrix(noiseWls, nrow=1)) # need the wls only for testing
		iw_extrema <- function(x) {
				out <- x[sample(c(1,2),1)]
		#		test_plotSinglePool(doTest, swl=x[3], pool=out, nc, out=NULL)
				return(out)
			} # EOIF
		for (i in 1: nrow(noiseMat)) {
		#	noiseMat[i,] <- apply(noiseHull, 2, function(x) x[sample(c(1,2),1)]) # select from columns from noiseHull and fill into noise matrix
			noiseMat[i,] <- apply(noiseCompound, 2, iw_extrema) # select from columns from noiseHull and fill into noise matrix
		}
		test_plotSingleResult(doTest, wls=noiseWls, siRow=noiseMat[1,])	
	} # end if extrema
	###	
#	print("----"); 	print(noiseHull[,1:5]); print(noiseMat[1:50, 1:5]) ; wait()
	dataset$NIR <- dataset$NIR + noiseMat
	return(dataset)
} # EOF


#' @title Add Noise to Dataset
#' @description Manually add noise to a provided dataset.
#' @details Select the noise-mode in the settings.r file (parameter 
#' \code{noi_addMode}). You need to have an R-data file with noise-spectra in 
#' your settings-home folder, please refer to \code{\link{noise_procedures}} for 
#' further information.
#' @inheritParams gdmm
#' @param md The metadata from where the name of the noise-file is taken.
#' @return The dataset with added noise.
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' fdNoise <- do_addNoise(fd)
#' }
#' @seealso \code{\link{noise_procedures}}
#' @family Noise procedures
#' @family Data pre-treatment functions 
#' @export
do_addNoise <- function(dataset, noiseFile="def", md=getmd()) {
	stn <- autoUpS()
	ap <- getap()
	ap$dpt$noise$useNoise <- TRUE
	dsName <- deparse(substitute(dataset))
	#
	noiMode <- stn$noi_addMode 
	if (!stn$allSilent) {cat(paste0("Adding ", noiMode, " noise to dataset '", dsName, "'... "))}
	#
	noiseDataset <- checkLoadNoiseFile(dataset$header, max(dataset$NIR), ap, md, noiseFile) # only if noise is added; if not returns NULL; is assigning noiseFile !!
	noi_calculateNoiseDistribution(noiseDataset, noiseFile) # only if noise: calculate noise distribution, save as global variable in gl_ap2GD
	noiDat <- noi_performNoise(dataset, noiseFile)
	if (!stn$allSilent) {cat("ok.\n")}
	return(noiDat)
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
#' @seealso \code{\link{genTempCalibExp}}, \code{\link{genFolderStr}}
#' @family Helper Functions
#' @family Noise procedures
#' @export
genNoiseRecExp <- function() {
	genFolderStr()
	stn <- autoUpS()
	fn_metadata <- stn$fn_metadata # folder name for metadata
	fn_mDataDefFile <- stn$fn_mDataDefFile
	deleteCol <- stn$p_deleteCol
	clPref <- stn$p_ClassVarPref
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
	txt[grep("Repls", txt)] <- "\tRepls <- 24"
	txt[grep("Group", txt)] <- "\tGroup <- \"noise\""
	con <- file(pathMd, open="wt")
	writeLines(txt, con)
	close(con)
	return(invisible(NULL))
} # EOF

mod_md_txt <- function(where, fill, target) {
	target[grep(where, target)] <- paste0("\t", where, " <- \"", fill, "\"")
	return(target)
} # EOF

mod_md_logic <- function(where, logVal, target) {
	target[grep(where, target)] <- paste0("\t", where, " <- ", logVal)
	return(target)
} # EOF

mod_md_num <- function(where, numVal, target) {
	target[grep(where, target)] <- paste0("\t", where, " <- ", numVal)
	return(target)
} # EOF


#' @title Record / add noise
#' @description Record a special noise-data file on your spectroscopy device, and 
#' use this file to calculate data that can then be used to specifically add 
#' 'custom' noise to your datasets. Alternatively, if you do not want to record 
#' a specific noise file, you can also add static noise (with mean=0) to your 
#' datasets. Use the parameter \code{noi_addMode} in the settings file to choose 
#' from one of the modes of adding noise, please see section 'Modes for noise 
#' calculation' for details.
#' @details The noise data file is an R-data file generated / imported via the 
#' \code{\link{gfd}}) function, but containing only noise-spectra recorded 
#' following the recommendations below. This R-data file (where the factory-default 
#' filename is 'NoiseData') is then moved \strong{by the user} in to the AQUAP2SH 
#' folder. When generating datasets (a 'cube') using the function 
#' \code{\link{gdmm}}, it depends on whether you choose to add noise to the 
#' datasets or not (parameter \code{spl.do.noise} in the analysis procedure resp. 
#' your input. If, and only if, you choose to add noise, first (and only once per 
#' R-session) the noise R-data file is read in and used to calculate the specific 
#' noise distribution. It is stored in an object where the name is starting with 
#' \code{nd_} (for 'noise distribution') in the search-path object 
#' \code{aquap2_globalData}; (\code{ls(aquap2_globalData, all.names=T)}). 
#' This noise distribution is then used to specifically add, 
#' according to the selected mode (see section 'Modes for noise calculation' 
#' below), noise to each single wavelength in each single observation in the 
#' individual dataset within the cube. If you do not want to use a specific 
#' noise-data file, you can set the noise mode to 'static' (parameter 
#' \code{noi_addMode} in the settings file). 
#' @section Procedure: The procedure to work with noise-data files and use them to 
#' obtain a wavelength-specific noise distribution consists of the following 
#' steps:
#' \describe{
#' \item{Record noise spectra}{Use the function \code{\link{genNoiseRecExp}} to 
#' generate a folder structure for an experiment, then record the noise-spectra, 
#' for what the following procedure is suggested: 
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
#' @template mr_noiseModes
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
