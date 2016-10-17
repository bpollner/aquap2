#' @title Perform Smoothing and / or m-th Derivative
#' @description Performs a smoothing and / or the calculation of the m-th 
#' derivative on the given dataset using the Savitzky-Golay filter.
#' @details The underlying function is \code{\link[signal]{sgolayfilt}}.
#' @param dataset An object of class 'aquap_data" as produced by 
#' \code{\link{gfd}} or as can be extracted from a 'cube' via 
#' \code{\link{getcd}}.
#' @param p Numeric length one, the order of the filter. Default = 2.
#' @param n Numeric length one, the filter length, must be odd. Default = 21.
#' @param m Numeric length one, Return the m-th derivative of the filter 
#' coefficients. Default = 0.
#' @return Returns the dataset with the NIR-data smoothed or transformed.
#' @examples
#' \dontrun{
#' fd <- gfd() # get the full dataset
#' fd <- selectWls(fd, 1100, 1800)
#' fd_avg <- do_avg(fd, n=71) # apply strong smoothing
#' plot(fd - fd_avg, pg.where="", pg.main="| smoothed subtracted")
#' }
#' @family Data pre-treatment functions 
#' @family dpt modules documentation
#' @export
do_sgolay <- function(dataset, p=2, n=21, m=0) {
	autoUpS()
 	cns <- colnames(dataset$NIR)
	rns <- rownames(dataset$NIR)
	NIR <- t(apply(dataset$NIR, 1, signal::sgolayfilt, p=p, n=n, m=m))
	colnames(NIR) <- cns
	rownames(NIR) <- rns
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF

### for baseline removal
#' @title Calculate Standard Normal Variation SNV
#' @description Calculate the standard normal variation (SNV) by autoscaling the 
#' transformed NIR-data
#' @inheritParams do_sgolay
#' @return Returns the dataset with the transformed NIR data.
#' @examples
#' \dontrun{
#' fd <- gfd() # get the full dataset
#' fd <- selectWls(fd, 1100, 1800)
#' fd_snv <- do_snv(fd)
#' plot(fd - fd_snv, pg.where="", pg.main="| snv subtracted")
#' }
#' @family Data pre-treatment function
#' @family dpt modules documentation
#' @export
do_snv <- function(dataset) {
	autoUpS()
	NIR <- t(scale(t(dataset$NIR),center=TRUE,scale=TRUE))
	colnames(NIR) <- colnames(dataset$NIR)
	rownames(NIR) <- rownames(dataset$NIR)
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF
 
#' @title Perform MSC
#' @description Performs MSC (multiplicative scatter correction) on the 
#' provided dataset.
#' @details If no reference is provided, the average of all spectra of the provided
#' dataset is used as a reference for the baseline correction. Provide a dataset 
#' with a single spectrum (as e.g. produced by \code{\link{do_avg}}) to use this 
#' as a reference for baseline correction. Internally, the function 
#' \code{\link[pls]{msc}} is used.
#' @inheritParams do_sgolay
#' @param ref An object of class 'aquap_data' containing a single spectrum,
#' i.e. a single row (as e.g. produced by \code{\link{do_avg}}, or by 
#' subscripting via '[]'). If no reference is provided, the average of all spectra 
#' of the provided dataset is used as a reference for the baseline correction.
#' @return Returns the dataset with the transformed NIR data.
#' @seealso \code{\link{ssc}} for sub-selecting in a datset using a specific 
#' criterion.
#' @examples
#' \dontrun{
#' fd <- gfd() # get the full dataset
#' fd <- selectWls(fd, 1100, 1800)
#' fd_msc <- do_msc(fd)
#' plot(fd - fd_msc, pg.where="", pg.main="| msc subtracted")
#' fd_msc_ref <- do_msc(fd, ref=fd[4]) # use the 4th row of fd as a reference
#' plot(fd - fd_msc_ref, pg.where="", pg.main="| 4th row as reference")
#' fd_msc_mq <- do_msc(fd, ref=do_avg(ssc(fd, C_Group=="MQ"))) # is assuming the 
#' # existence of a column name "C_Group" and one or more of its values being "MQ"
#' plot(fd - fd_msc_mq, pg.where="", pg.main="| average of MQ as reference")
#' }
#' @seealso \code{\link{getcd}}
#' @family Data pre-treatment functions 
#' @family dpt modules documentation
#' @export
do_msc <- function(dataset, ref=NULL) {
	autoUpS()
	if (!is.null(ref)) {
		if (class(ref) != "aquap_data") {
			stop("Please provide an object of class 'aquap_data' to the argument 'ref'.", call.=FALSE)
		}
		if (nrow(ref) != 1) {
			stop("Please provide a dataset with only one single row, i.e. only one single spectrum, to the argument 'ref'.", call.=FALSE)
		}
		if (ncol(ref$NIR) != ncol(dataset$NIR)) {
			stop("Please provide a dataset containing the same number of wavelenghts to the argument 'ref'", call.=FALSE)
		}
		refInput <- as.numeric(ref$NIR)
	} else {
		refInput <- NULL
	}
 	NIR <- pls::msc(dataset$NIR, refInput)
	colnames(NIR) <- colnames(dataset$NIR)
	rownames(NIR) <- rownames(dataset$NIR)
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF

#' @title Average spectra in a dataset
#' @description Calculate the average spectrum of all the given spectra in a 
#' dataset.
#' @details The average spectrum is returned in a single row with the 
#' header-data taken from the first row of the original dataset.
#' @inheritParams do_sgolay
#' @return A standard dataset with a single row.
#' @examples
#' \dontrun{
#' fd <- gfd() # get the full dataset
#' fd <- selectWls(fd, 1100, 1800)
#' fd_avg <- do_avg(fd)
#' fd_avg2 <- do_avg(ssc(fd, C_water=="MQ")) # assumes the existence of column "C_water"
#' plot(fd_avg, pg.where="", pg.main="| averaged")
#' plot(fd - fd_avg, pg.where="", pg.main="| avg subtracted")
#' }
#' @family Data pre-treatment functions 
#' @seealso \code{\link{getcd}}
#' @export
do_avg <- function(dataset) {
	autoUpS()
	NIR <- matrix(apply(dataset$NIR, 2, mean), nrow=1)
	colnames(NIR) <- colnames(dataset$NIR)
	dsNew <- dataset[1,]
	dsNew$NIR <- I(NIR)
	return(dsNew)	
} # EOF

### input is a data frame with one or 2 loading vectors or one regression vector
calc_emsc <- function(dataset, input) { ## this one possibly used "external"
	autoUpS()
	cnsWls <- colnames(dataset$NIR)
	rns <- rownames(dataset$NIR)
	NIRdata <- dataset$NIR
	class(NIRdata) <- "matrix"
	wls <- getWavelengths(dataset)
	ColMeansNIR <- colMeans(NIRdata)
	if (ncol(input) == 1) {
		Xcal1 <- cbind(rep(1, ncol(NIRdata)), ColMeansNIR, input[1])
		Ycor2_tmp <- t(rep(NA, ncol(NIRdata)))
		for (i in 1: nrow(NIRdata)) {
			Ycal <- as.data.frame(matrix(NIRdata[i,], ncol=1))
			tmp <- lm(Ycal$V1 ~ Xcal1[,1] + Xcal1[,2] + Xcal1[,3])
			b <- tmp$coefficients[-2]
			Ycor2_tmp <- rbind(Ycor2_tmp, t(((Ycal-b[1])-b[3]*Xcal1[,3])/b[2]))	## here things are happening !!
		} # end for i
		Ycor2 <- out <- Ycor2_tmp[-1,]		
	} else {
		Xcal1 <- cbind(rep(1, ncol(NIRdata)), ColMeansNIR, input[1], input[2])
		Ycal <- NA
		Ycor2_tmp <- t(rep(NA, ncol(NIRdata)))
		for ( i in 1: nrow(NIRdata)) {
			Ycal <- as.data.frame(matrix(NIRdata[i,], ncol=1))
  		    tmp <-  lm(Ycal$V1 ~ Xcal1[,1] + Xcal1[,2] + Xcal1[,3] + Xcal1[,4] )
			b <- tmp$coefficients[-2]
    		Ycor2_tmp <- rbind(Ycor2_tmp, t(((Ycal-b[1]) - b[3]*Xcal1[,3]-b[4]*Xcal1[,4]) / b[2]) )
		} # end for i
		 Ycor2 <- out <- Ycor2_tmp[-1,]
    } # all is done
	colnames(Ycor2) <- cnsWls
	rownames(Ycor2) <- rns
	out <- as.data.frame(Ycor2)
	return(out)
} # EOF

### input the vecLoad directly from the model via: pcaModel$loadings[, c(x,y)]
#' @title Perform EMSC
#' @description Performs EMSC with up to two signals on the provided dataset
#' @details A data frame with one or two loadings or with one regression vector
#' can be used as input to (try to) remove the effect on the data described by 
#' said loadings / regr. vector. For example, from a pca-model the first and 
#' second loading vector can be extracted with \code{pcaModel$loadings[, c(1,2)]}.
#' @inheritParams do_sgolay
#' @param vecLoad  A data frame x (\code{ncol(x) <= 2}) with one or two loading 
#' vectors or one regression vector.
#' @return Returns the dataset with the transformed NIR data.
#' @seealso \code{\link{getcm}} for easy extraction of single models where 
#' loading vectors or a regression vector can be obtained.
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' cu <- gdmm(fd, getap(do.pca=TRUE)) # assumes no split
#' loadings <- getcm(cu, 1, what="pca")$loadings[, c(1,2)]
#' fd_emsc <- do_emsc(fd, loadings)
#' }
#' @family Data pre-treatment functions 
#' @family dpt modules documentation
#' @export
do_emsc <- function(dataset, vecLoad=NULL) {
	autoUpS()
	input <- as.data.frame(vecLoad)
	if (ncol(input) > 2) {
		stop("At the moment, not more than 2 effects can be removed from the data. Please be a bit more content.", call.=FALSE)
	}
	if (is.null(vecLoad)) {
		stop("Please provide a data frame with one or two loading vectors or one regression vector to the argument 'vecLoad' (do_emsc).", call.=FALSE)
	}
	NIR <- as.matrix(calc_emsc(dataset, input))
	rownames(NIR) <- rownames(dataset)
	colnames(NIR) <- colnames(dataset$NIR)
	dataset$NIR <- I(NIR)
	return(dataset)	
} #EOF

do_scale <- function(dataset) {
	NIR <- som::normalize(dataset$NIR, byrow=FALSE)
	colnames(NIR) <- colnames(dataset$NIR)
	rownames(NIR) <- rownames(dataset$NIR)
	dataset$NIR <- NIR
	return(dataset)
} # EOF

do_scale_fc <- function(dataset, calibAvgTable) { # used in aquagram norm foreign center
	avg <- apply(calibAvgTable, 2, mean)
	NIR <- scale(as.matrix(dataset$NIR), center=avg, scale=TRUE)
	colnames(NIR) <- colnames(dataset$NIR)
	rownames(NIR) <- rownames(dataset$NIR)
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF

#' @title Perform gap-segment derivatives
#' @description Performs gap-segment derivatives on the provided dataset. The 
#' behaviour of the filter can be modified via its arguments, please see also 
#' the documentation for \code{\link[prospectr]{gapDer}}.
#' @details The first column of the wavelengths and the last one get doubled in 
#' order to have the same number of wavelengths in the resulting dataset as in 
#' the original dataset. The underlying function is \code{\link[prospectr]{gapDer}}.
#' @inheritParams do_sgolay
#' @param m Numeric length one. The order of the derivative, can be between 1 and 
#' 4. Default is 1.
#' @param w Numeric length one. The filter length (should be odd), ie. the spacing 
#' between points 
#' over which the derivative is computed. Default is 1.
#' @param s Numeric length one. The segment size, i.e. the range over which the 
#' points are averaged.Default is 1.
#' @param deltaW Numeric length one. The sampling interval / band spacing. Default 
#' is 1.
#' @section Note:
#' The documentation for the parameters was mostly taken from 
#' \code{\link[prospectr]{gapDer}} by Antoine Stevens.
#' @seealso \code{\link[prospectr]{gapDer}}
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' fd_gsd <- do_gapDer(fd)
#' fd_gsd2 <- do_gapDer(fd, 1,11,13,1)
#' plot(fd_gsd - fd_gsd2)
#' }
#' @family Data pre-treatment functions 
#' @family dpt modules documentation
#' @export
do_gapDer <- function(dataset, m=1, w=1, s=1, deltaW) {
	nir <- getNIR(dataset)
	NIR <- prospectr::gapDer(nir, m=m, w=w, s=s, delta.wav=deltaW) # gives back a matrix with one columne less at the beginning and end, so in total two wavelengths missing !!
#	NIR <- as.matrix(NIR)
#	wlsGap <- as.numeric(gsub("w", "", colnames(NIR)))
#	wlsD <- getWavelengths(dataset)
#	indLow <- which(wlsD == min(wlsGap))
#	indHigh <- which(wlsD == max(wlsGap))
#	nrLostLow <- length(1:indLow)
#	nrLostHigh <- length(indHigh: length(wlsD))
#	cat(paste("Low wls lost:", nrLostLow, "\n"))
#	cat(paste("High wls lost:", nrLostHigh, "\n"))	
#	return(NIR)	
#	first <- as.data.frame(NIR[,1])
#	last <- as.data.frame(NIR[, ncol(NIR)])
#	NIR <- cbind(first, NIR, last)
#	colnames(NIR) <- colnames(dataset$NIR)
#	rownames(NIR) <- rownames(dataset$NIR)
#	NIR <- as.matrix(NIR) # because otherwise the "AsIs" is behaving strange
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF

checkDeTrendSrcTrgInput <- function(dataset, src=NULL, trg="src") {
	addInfo <- "\n(For the latter, please see ?dpt_modules for further information.)"
	checkNumLengthTwo <- function(arg) {
		argName <- deparse(substitute(arg))
		if (!all(is.numeric(arg)) | length(arg) != 2) {
		stop(paste0("Please provide a numeric length two to the argument '", argName, "' resp. in the de-trend argument in the analysis procedure / your input.", addInfo), call.=FALSE)
		}
	} # EOIF
	##
	checkWls <- function(arg, allWls= getWavelengths(dataset)) {
		argName <- deparse(substitute(arg))
		if (min(arg) < min(allWls) | max(arg) > max(allWls)) {
			stop(paste0("Sorry, the range in the argument '", argName, "' is not within the wavelength-range of the provided dataset [", min(allWls) , " to ", max(allWls), "]. Please check your input at the argument '", argName, "' resp. in the de-trend argument in the analysis procedure / your input.", addInfo), call.=FALSE)
		} 
	} # EOIF
	checkRange <- function(arg) {
		argName <- deparse(substitute(arg))
		if (diff(arg) <= 0) {
			stop(paste0("Please provide a wavelength range greater than zero for the argument '", argName, "' resp. in the de-trend argument in the analysis procedure / your input.", addInfo), call.=FALSE)
		}	
	}
	if (!is.null(src)) {
		checkNumLengthTwo(src)
		checkWls(src)
		checkRange(src)
	}
	if (any(is.character(trg))) {
		if (!trg %in% c("src", "all")) {
			stop(paste0("Please provide one of 'src', 'all' or a numeric length two to the argument 'trg' resp. in the de-trend argument in the analysis procedure / your input.", addInfo), call.=FALSE)
		}
	}
	if (all(trg == "src")) {
		trg <- src
	}
	if (all(trg == "all")) {
		trg <- range(getWavelengths(dataset))
	}
	if (!is.null(trg)) {
		checkNumLengthTwo(trg)
		checkWls(trg)
		checkRange(trg)
	}
	assign("trg", trg, pos=parent.frame(n=1))
	##
} # EOF

#' @title Perform De-Trend
#' @description Perform de-trending of the given dataset. For each observation, 
#' the linear model \code{lm(absorbance ~ wavelength)} is calculated. The 
#' coefficients of the resulting model are then used to modify the absorbance 
#' values after the formula 
#' \code{newAbsorbance = absorbance - intercept - k*wavelength}. It is possible 
#' to separately specify the source and the target of the  de-trend operation. 
#' @details Via the arguments \code{src} ('source') and \code{trg} ('target') 
#' it is possible to specify separately from which wavelength-range the values 
#' for the de-trend should be taken resp. to which wavelength-range the 
#' resulting de-trend should be applied. Please see documentation for the 
#' arguments \code{src} and \code{trg} for further details. If the target 
#' wavelengths are only a part of the whole wavelength-range in the dataset, 
#' the de-trend will be applied only to this wavelengths, and the rest of the 
#' wavelengths will remain unchanged. Abrupt steps in the resulting spectra can 
#' be the result of this. If both arguments \code{src} and \code{trg} are left 
#' at their defaults, the full range of wavelengths in the dataset is used for 
#' calculating the de-trend values, i.e. the linear models, and the resulting 
#' de-trend is also applied to the full range of wavelengths present in the 
#' dataset.
#' @param dataset An object of class 'aquap_data' as produced e.g. by 
#' \code{\link{gfd}}.
#' @param src 'source'; the wavelength-range from where the values for de-trend 
#' should be calculated (i.e. the linear models). Leave at the default NULL 
#' to use the full range of wavelengths in the provided dataset, or provide 
#' a numeric length two to use this wavelength range for calculating the values 
#' for de-trend.
#' @param trg 'target'; character length one or numeric length two. The wavelengths
#' where the de-trend should be applied. If left at the default 'src' the same 
#' wavelength as specified in \code{src} is used. Possible values are: 
#' \describe{
#' \item{src}{Apply the de-trend to the same wavelength-range as specified in 
#' argument \code{src} (the default).}
#' \item{all}{Apply the de-trend to all the wavelengths in the provided dataset.}
#' \item{Numeric length two}{Provide a numeric length two to the argument 
#' \code{trg} to apply the de-trend only to this wavelength range.}
#' }
#' @return The transformed dataset
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' fdDT <- do_detrend(fd) # use the whole wavelength range of 'fd' as source and
#' # target for the de-trend
#' plot(fd)
#' plot(fdDT)
#' ###
#' fdc <- selectWls(fd, 1300, 1600)
#' plot(fdc)
#' plot(do_detrend(fdc)) # whole range as source and target
#' plot(do_detrend(fdc, src=c(1400, 1500))) # same target as soruce
#' plot(do_detrend(fdc, src=c(1400, 1500), trg="all")) # apply to full range
#' plot(do_detrend(fdc, src=c(1400, 1500), trg=c(1300, 1600))) # same as above 
#' plot(do_detrend(fdc, src=c(1300, 1400), trg=c(1380, 1580))) 
#' }
#' @family Data pre-treatment functions 
#' @family dpt modules documentation
#' @export
do_detrend<- function(dataset, src=NULL, trg="src") { 
	checkDeTrendSrcTrgInput(dataset, src, trg) # is assigning trg
	# source
	if (is.null(src)) {
		absSrc <- getNIR(dataset)
		wlsSrc <- getWavelengths(dataset)
	} else {
		ds <-  selectWls(dataset, src[1], src[2])
		absSrc <- getNIR(ds)
		wlsSrc <- getWavelengths(ds)
	}
	mods <- apply(absSrc, 1, function(x) lm(x ~ wlsSrc)$coefficients)  ### calculate the models !! one for every observation ### gives back a matrix with two rows and one column for every observation
	# target
	if (is.null(trg)) {
		nirTrg <- getNIR(dataset)
		wlsTrg <- getWavelengths(dataset)
	} else {
		if (is.numeric(trg)) {
			ds <- selectWls(dataset, trg[1], trg[2])
			nirTrg <- getNIR(ds)
			wlsTrg <- getWavelengths(ds)
		} else {
			
		}
	}	
#	NIR <- matrix(NA, nrow(nirTrg), ncol(nirTrg))
#	for (i in 1: nrow(nirTrg)) {
#		NIR[i,] <- as.numeric(nirTrg[i,]) - mods[1,i] - mods[2,i]*wlsTrg
#	}
	NIRnew <- t(sapply(1:nrow(nirTrg), function(i) as.numeric(nirTrg[i,]) - mods[1,i] - mods[2,i]*wlsTrg))
	colnames(NIRnew) <- cnsNew <- colnames(nirTrg)
	cnsOld <- colnames(dataset$NIR)
	indHere <- which(cnsOld %in% cnsNew)
	dataset$NIR[, indHere] <- NIRnew
	return(dataset)	
} # EOF
