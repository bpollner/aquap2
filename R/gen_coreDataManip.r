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
#' @param exportModel Logical. If a possible model should be stored in the set,
#' (leave at \code{FALSE}).
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
do_sgolay <- function(dataset, p=2, n=21, m=0, exportModel=FALSE) {
	autoUpS()
 	cns <- colnames(dataset$NIR)
	rns <- rownames(dataset$NIR)
	NIR <- t(apply(dataset$NIR, 1, signal::sgolayfilt, p=p, n=n, m=m))
	exportAdditionalModelToAp2Env(doExport=exportModel, thisMod=NULL, thisType=pv_dptModules[1]) # sgol
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
do_snv <- function(dataset, exportModel=FALSE) {
	autoUpS()
	NIR <- t(scale(t(dataset$NIR),center=TRUE,scale=TRUE))
	exportAdditionalModelToAp2Env(doExport=exportModel, thisMod=NULL, thisType=pv_dptModules[2]) # snv
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
#' @param extMscModel Provide an external msc model to use this for predicting 
#' the data instead of running msc on the data. Experimental feature.
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
do_msc <- function(dataset, ref=NULL, extMscModel=NULL, exportModel=FALSE) {
	autoUpS()
	if (!is.null(ref)) {
		if (class(ref) != "aquap_data") {
			stop("Please provide an object of class 'aquap_data' to the argument 'ref'.", call.=FALSE)
		}
		if (nrow(ref) != 1) {
			stop("Please provide a dataset with only one single row, i.e. only one single spectrum, to the argument 'ref'.", call.=FALSE)
		}
		if (ncol(ref$NIR) != ncol(dataset$NIR)) {
			stop("Please provide a dataset containing the same number of wavelengths to the argument 'ref'", call.=FALSE)
		}
		refInput <- as.numeric(ref$NIR)
	} else {
		refInput <- NULL
	}
	if (is.null(extMscModel)) {
	 	NIR <- pls::msc(dataset$NIR, refInput)
	} else {
		NIR <- predict(extMscModel, as.matrix(dataset$NIR))
	}
	exportAdditionalModelToAp2Env(doExport=exportModel, thisMod=NIR, thisType=pv_dptModules[3]) # msc
	colnames(NIR) <- colnames(dataset$NIR)
	rownames(NIR) <- rownames(dataset$NIR)
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF

#' @title Average spectra in a dataset
#' @description Calculate the average spectrum of all the given spectra in a 
#' dataset, or define groups to be averaged by providing one or more 
#' class-variable.
#' @details The total and group-wise average spectrum is returned each in a 
#' single row with the header-data taken from the first row of the subset of 
#' the original dataset as defined by the grouping. If parameter \code{clv} is 
#' left at the default NULL, all the spectra in the dataset will be averaged 
#' together into a single row.
#' @inheritParams do_sgolay
#' @param clv Character vector, one or more valid class-variables defining the 
#' subsets of the dataset to be averaged.
#' @param inParallel Logical, if the averaging of spectra should be done in 
#' parallel. Defaults to TRUE.
#' @return The transformed dataset.
#' @examples
#' \dontrun{
#' fd <- gfd() # get the full dataset
#' fd <- selectWls(fd, 1100, 1800)
#' fd_avg <- do_avg(fd)
#' fd_avg2 <- do_avg(ssc(fd, C_water=="MQ")) # assumes the existence of column "C_water"
#' plot(fd_avg, pg.where="", pg.main="| averaged")
#' plot(fd - fd_avg, pg.where="", pg.main="| avg subtracted")
#' #######
#' fd_avg_group <- do_avg(fd, "C_Group") # averaging within groups
#' fd_avg_time_group <- do_avg(fd, c("C_Time", "C_Group")) # averaging within a 
#' # subset defined by "C_Time" and "C_Group"
#' }
#' @family Data pre-treatment functions 
#' @seealso \code{\link{getcd}}
#' @export
do_avg <- function(dataset, clv=NULL, inParallel=TRUE) {
	autoUpS()
	if (is.null(clv)) {
		NIR <- matrix(apply(dataset$NIR, 2, mean), nrow=1)
		colnames(NIR) <- colnames(dataset$NIR)
		dsNew <- dataset[1,]
		dsNew$NIR <- I(NIR)
		return(dsNew)		
	} # end is.null(clv)
	#
	cns <- colnames(dataset$header)
	if (!all(clv %in% cns)) {
		ind <- which(!clv %in% cns)
		stop(paste0("Sorry, the class-variables '", paste(clv[ind], collapse="', '"), "' seem not to be present in the provided dataset."), call.=FALSE)
	}
	fdf <- makeFlatDataFrameMulti(dataset, clv)
	if (inParallel) {
		registerParallelBackend()
	} else {
		registerDoSEQ()
	}
	##
	fdfMean <- plyr::ddply(fdf, .variables=clv, .fun=plyr::colwise(mean), .parallel=inParallel) ##### CORE #####
	NIR <- as.matrix(fdfMean[,-(1:length(clv))])
	smh <- fdfMean[, 1:length(clv)]
	##
	header <- as.data.frame(matrix(rep(NA, ncol(dataset$header)), nrow=1))
	colnames(header) <- colnames(dataset$header)
	colRep <- as.data.frame(matrix(rep(NA, ncol(dataset$colRep)), nrow=1))
	colnames(colRep) <- colnames(dataset$colRep)
	ds <- dataset
	for (i in 1: nrow(smh)) {
		for (k in 1: ncol(smh)) {
			ds <- ssc_s(ds, clv[k], smh[i,k]) # search the matching data to construct the header
		} # end for k
		ds <- ds[1,] # only take the first row
		siH <- ds$header
		class(siH) <- "data.frame"
		header <- rbind(header, siH)
		siCol <- ds$colRep
		class(siCol) <- "data.frame"
		colRep <- rbind(colRep, siCol)
		ds <- dataset
	} # end for i
	header <- header[-1,] # leave out all the NAs
	colRep <- colRep[-1,]
	rownames(NIR) <- rownames(header)
	newData <- data.frame(I(header), I(colRep), I(NIR))
	dataset@.Data <- newData
	dataset@row.names <- rownames(header)
	for (i in 1: ncol(dataset$header)) {
		if (all(is.character(dataset$header[,i]))) {
			dataset$header[i] <- factor(dataset$header[,i])
		}
	} 
	return(dataset)
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
do_emsc <- function(dataset, vecLoad=NULL, exportModel=FALSE) {
	autoUpS()
	input <- as.data.frame(vecLoad)
	if (ncol(input) > 2) {
		stop("At the moment, not more than 2 effects can be removed from the data. Please be a bit more content.", call.=FALSE)
	}
	if (is.null(vecLoad)) {
		stop("Please provide a data frame with one or two loading vectors or one regression vector to the argument 'vecLoad' (do_emsc).", call.=FALSE)
	}
	NIR <- as.matrix(calc_emsc(dataset, input))
	exportAdditionalModelToAp2Env(doExport=exportModel, thisMod=NULL, thisType=pv_dptModules[4]) # emsc
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
do_gapDer <- function(dataset, m=1, w=1, s=1, deltaW, exportModel=FALSE) {
	autoUpS()
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
	exportAdditionalModelToAp2Env(doExport=exportModel, thisMod=NULL, thisType=pv_dptModules[7]) # gapDer
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
#' @inheritParams do_sgolay
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
do_detrend<- function(dataset, src=NULL, trg="src", exportModel=FALSE) { 
	autoUpS()
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
	exportAdditionalModelToAp2Env(doExport=exportModel, thisMod=NULL, thisType=pv_dptModules[6]) # deTrend
	colnames(NIRnew) <- cnsNew <- colnames(nirTrg)
	cnsOld <- colnames(dataset$NIR)
	indHere <- which(cnsOld %in% cnsNew)
	dataset$NIR[, indHere] <- NIRnew
	return(dataset)	
} # EOF

#' @title Perform Data-Pretreatment Sequence
#' @description Manually perform a sequence of data pre-treatments defined in a 
#' string.
#' @details For internal use.
#' @param dataset An object of class 'aquap_data'.
#' @param dptSeq A character holding at least one valid string for data pre-treatment.
#' @param extraModelList A list of possible external models to hand over to the 
#' data pre-treatment process.
#' @param silent Logical. If status info should be printed or not.
#' @return The transformed dataset.
#' @export
do_dptSeq <- function(dataset, dptSeq, extraModelList=NULL, silent=TRUE) {
	autoUpS(cfs=FALSE)
	.ap2$stn$allSilent <- silent
	return(performDPT_Core(dataset, dptSeq, extraModelList, allExportModels=FALSE))
} # EOF


#' @title Resample data to new Wavelengths
#' @description Resample the data in the provided dataset to new, possibly 
#' evenly spaced, wavelengths.
#' @details If nothing is provided for the argument \code{targetWls}, an evenly 
#' spaced vector from the lowest to the highest available wavelength 
#' is automatically generated as the target wavelengths to be resampled to.
#' @param dataset An object of class 'aquap_data' as produced e.g. by 
#' \code{\link{gfd}}.
#' @param targetWls Numeric vector, the target wavelengths. If left at the 
#' default \code{NULL}, an evenly spaced vector from the lowest to the 
#' highest available wavelength is automatically generated as the target 
#' wavelengths to be resampled to.
#' @param tby Numeric length one, the spacing in nm of the automatically 
#' generated target wavelength. Only applies if \code{targetWls} is left at 
#' \code{NULL}.
#' @param method The resampling method. For details see 
#' \code{\link[pracma]{interp1}}.
#' @return The resampled dataset.
#' @examples
#' \dontrun{
#' fd <- gfd()
#' fdR <- do_resampleNIR(fd)
#' }
#' @family Data pre-treatment functions 
#' @export
do_resampleNIR <- function(dataset, targetWls=NULL, tby=0.5, method="linear") {
	ncpwl <- dataset@ncpwl
	charPrevWls <- substr(colnames(dataset$NIR)[1], 1, (ncpwl))
	#
    x <- getWavelengths(dataset)
	if (is.null(targetWls)) {
    	xNew <- seq(ceiling(x[1]/2) * 2, floor(x[length(x)]/2) * 2, tby)
	} else {
		xNew <- targetWls
	}	
	NIR <- t(apply(dataset$NIR, 1, pracma::interp1, x = x, xi = xNew, method = method))
    colnames(NIR) <- paste0(charPrevWls, xNew)
    dataset$NIR <- NIR
    return(dataset)
} # EOF


checkBlowUpInput_grouping <- function(header, grp) {
	cns <- colnames(header)
	cPref <- .ap2$stn$p_ClassVarPref
	cns <- cns[grep(cPref, cns)]
	###
	checkClassExistence <- function(X, char, ppv=TRUE) {
		if (!all(is.character(X))) {
			stop(paste0("Please only provide characters as input for the argument '", char, "'. Please check your input."), call.=FALSE)
		}
		for (i in 1: length(X)) {
			if (!X[i] %in% cns) {
				msg1 <- paste0("Sorry, it seems that the class-variable `", X[i], "` as grouping variable (", char, ") does not exist in the provided dataset.")
				msg2 <- "";  if (ppv) { msg2 <- paste0("\nPossible values are: '", paste(cns, collapse="', '"), ".") }
				stop(paste0(msg1, msg2), call.=FALSE)
			}
		} # end for i
	} # EOIF
	checkClassExistence(grp, "grp")
} # EOF

checkTransformBlowUpInput_TnAn <- function(minPart, tn, an) {
	if (is.character(tn)) {
		if (substr(tn,1,1) != "x") {
			stop(paste0("Please provide `tn` in the format `xN`, with N being a positive integer"), call.=FALSE)
		}
		options(warn=-1)
		tn <- round(as.numeric(substr(tn, 2, nchar(tn)))) * minPart # multiply with minPart, as we want to have it xFold
		options(warn=0)
		if (!is.numeric(tn)) {
			stop(paste0("Please provide an integer after the character `x`in the argument `tn`."), call.=FALSE)
		}
	} else {
		if (!is.numeric(tn)) {
			stop(paste0("Please provide either an integer or a character in the format `xN`, with N being a positive integer, to the argument `tn`."), call.=FALSE)			
		}
		rn <- round(tn)
	} # end else
	if (tn < minPart) {tn <- minPart}
	#
	if (is.character(an)) {
		if (substr(an, nchar(an), nchar(an)) != "%") {
			stop(paste0("Please provide `an` in the format `N%`, with N being a positive integer"), call.=FALSE)			
		}
		options(warn=-1)
		an <- round(as.numeric(substr(an, 1, nchar(an)-1)))
		options(warn=0)
		if (an > 100) { an <- 100}
		an <- round((an * minPart) / 100)  # as we want to have a percentage
		if (!is.numeric(an)) {
			stop(paste0("Please provide an integer before the character `%`in the argument `an`."), call.=FALSE)
		}
	} else {
		if (!is.numeric(an)) {
			stop(paste0("Please provide either an integer or a character in the format `N%`, with N being a positive integer, to the argument `an`."), call.=FALSE)						
		}
		an <- round(an)
		if (an < 1) {an <- 1}
	} # end else
	###
	return(list(tn=tn, an=an))
} # EOF

lotto_loop <- function(tn, an, size, n=2000) {
   	lotto <- function(tn, an=3, size=8) {
    	nums <- lapply(1:tn, function(x) sample(1:size, an, replace = TRUE))
	    nums <- lapply(nums, sort)
    	numsChar <- unlist(lapply(nums, function(x) paste(x, collapse="-")))
	    out <- vector("logical", length(numsChar))
   		 for (i in 1: length(numsChar)){
        	out[i] <- numsChar[i] %in% numsChar[-i]
	    }
    	le <- length(which(out))
	    perc <- round((le / tn)*100,0)
 	 	return(invisible(perc))
 	 } # EOIF
    percOut <- vector("numeric", n)
    for (i in 1:n) {
        percOut[i] <- lotto(tn, an, size)
    }
    percOut <- round(mean(percOut),1)
#    cat(paste0("total average: ", out))
    return(invisible(percOut))
}

#' @title Increase the Numbers of Observation in Dataset
#' @description Use random observations of the provided dataset (within a possible 
#' grouping), calculate their average and add the resulting spectrum as new 
#' observation to the dataset.
#' @details The random observations are sampled with replacement. The provenience 
#' of each observation is marked in an additional column in the header named 
#' `blowup`, where the character `orig` denotes an original observation, while the 
#' character `artif` is marking an artificially generated observation.
#' @param dataset An object of class 'aquap_data' as produced e.g. by 
#' \code{\link{gfd}}.
#' @param tn Numeric or character length one. If numeric, \code{tn} is denoting 
#' the target-number, i.e. the desired number of observations in the expanded 
#' dataset. If a grouping is provided via \code{grp}, the target number is 
#' defining the desired number of observations within each subgroup obtained 
#' by the grouping. If \code{tn} is a character it hast to be in the format 
#' \code{xN}, with \code{N} being a positive integer. In this case it means 
#' an N-fold increase of the dataset resp. of each subgroup as defined by the 
#' grouping in \code{grp}. Defaults to \code{x10}.
#' @param an Numeric or character length one. If numeric, \code{an} is denoting 
#' the "average-number", i.e. the number of observations resp. their spectra that 
#' should be drawn (with replacement) from the dataset resp. from the respective 
#' subgroup. These drawn samples then are averaged together into a new spectrum. 
#' If \code{an} is a character it has to be in the format \code{N\%}, with 
#' \code{N} being a positive integer. In this case it denotes the percentage of 
#' observations in the dataset resp. in each of the subgroups possibly obtained 
#' via \code{grp} that should be drawn (with replacement) and averaged together 
#' into a new observation. Defaults to \code{100\%}, what is the recommended value.
#' @param grp Character. One ore more valid class-variable names that should be 
#' used to from subgroups within the dataset. Similar to the \code{spl.var} 
#' argument in the analysis procedure.
#' @param cst Logical. If consecutive scans should always be kept together. 
#' NOT YET IMPLEMENTED - value is ignored, and consec. scans are NOT kept together.
#' @param conf Logical. If numbers should be presented and confirmation requested 
#' before commencing the actual calculations.
#' @section Warning: Do take care of a correct grouping, otherwise the inclusion 
#' of observations into the same group that do not belong together will destroy
#' any information.
#' @param replace Logical. If the sample drawing should be done with replacement.
#' Recommended value is TRUE.
#' @param pred Logical. If an estimation of the number of identical spectra should 
#' be made. Only presented if \code{conf} is TRUE.
#' @return The dataset with increased numbers of observations.
#' @examples
#' \dontrun{
#' fd <- gfd()
#' fdPlus <- do_blowup(fd, tn="x4", an=4)
#' fdPlus <- do_blowup(fd, tn="x8", an="10%", grp=c("C_Foo", "C_Bar")) 
#' fdPlus <- do_blowup(fd, tn=1000, an=5)
#' }
#' @family Data pre-treatment functions 
#' @export
do_blowup <- function(dataset, grp=NULL, tn="x10", an="100%", cst=TRUE, conf=TRUE, pred=TRUE, replace=TRUE) {
	cPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	snColSet <- .ap2$stn$p_sampleNrCol
	snCol <- paste0(yPref, snColSet)
	txtOrig <- "orig"
	txtBlow <- "artif"
	colNameBlow <- "blowup"
	colOrig <- 1
	colBlow <- 2
	lottoLoopN <- .ap2$stn$cl_extDatPred_N_lottoLoop
	#
	header <- headerFac <- getHeader(dataset) # headerFac only used for grouping
	colRep <- getColRep(dataset)
	blowupDf <- data.frame(X=rep(txtOrig, nrow(header))) ## add a new column to the header telling us what samples are new and what are old
	colnames(blowupDf) <- paste0(cPref, colNameBlow)
	header <- cbind(header, blowupDf)
	blowCol <- data.frame(X=rep(colOrig, nrow(header)))
	colnames(blowCol) <- paste0(cPref, colNameBlow)
	colRep <- cbind(colRep, blowCol)
	#
	# first get out the factors
	facInd <- vector("logical", length=ncol(header))
	for (i in 1: ncol(header)) {
		facInd[i] <- isFac <-  is.factor(header[,i])
		if (isFac) {
			header[,i] <- as.character(header[,i])
		}
	} # end for i
	#
	minPart <- nrow(header)
	splitList <- list(header)
	NIR <- getNIR(dataset)
	NIRsplitList <- list(NIR)
	colRepSplitList <- list(colRep)
	if (!is.null(grp)) {
		checkBlowUpInput_grouping(header, grp) # stops if wrong or so
		splitList <- split(header, headerFac[,grp]) # returns a list with a dataframe in each element
		NIRsplitList <- split(NIR, headerFac[,grp])
		colRepSplitList <- split(colRep, headerFac[,grp])
		minPart <- min(unlist(lapply(splitList, nrow))) # get the minium number of participants from all the splitelements
	}
	aa <- checkTransformBlowUpInput_TnAn(minPart, tn, an)
	tn <- aa$tn
	an <- aa$an
#	print(tn); print(an); print(minPart); print("-----")
	# now we have the correct target and average number
	requiredN <-  tn - minPart
	if (requiredN == 0) {
		message("No dataset expansion has been performed.")
		return(dataset)
	}
	eachNPart <- lapply(splitList, function(x) 1: nrow(x)) # gives a list with a vector from 1:N, with N being the number of participants from each element of the splitList
	indList <- newNirList <- newHeaderList <- newColRepList <- vector("list", length=length(splitList))
	for (i in 1: length(splitList)) { # now for every element within the splitList (could be only 1) we perform tn iterations of the resampling
		###### CORE ######
		indList[[i]] <- lapply(1:requiredN, function(x, npa, ana) sample(npa[[i]], ana, replace=replace), npa=eachNPart, ana=an) # gives a list for each element of the splitList
		###### CORE ######
		newNirList[[i]] <- matrix(NA, nrow=requiredN , ncol=ncol(NIR))
		newHeaderList[[i]] <- data.frame(matrix(NA, nrow=requiredN, ncol=ncol(header)))
		newColRepList[[i]] <- data.frame(matrix(NA, nrow=requiredN, ncol=ncol(colRep)))
	} # end i
	if (conf) {
		avgDoubleMsg <- ""
		if (pred) {
			avgDoubles <- lotto_loop(tn=requiredN, an=an, size=minPart, n=lottoLoopN)
			avgDoubleMsg <- paste0("The expected percentage of identical spectra is ~", avgDoubles, "%.")
		}
		cat(paste0(requiredN, " observations will be added to each of ", length(indList), " subgroups (the smallest containing ", minPart, " observations) by drawing from ", an, " observations.\n", avgDoubleMsg, "\n\nPress enter to continue or escape to abort:"))		
		scan(file = "", n = 1, quiet = TRUE)
	}
	# future: check here the IndList for doubles !!
	# now we have indList containing the header segments, NIRsplitList containing the NIR segments, and the indList that contains the indices to be averaged for each segment
	if (!.ap2$stn$allSilent) {cat(paste0("Calculating (draw from ", an, ") and adding ", requiredN, " new observations to each of ", length(indList), " subgroups:\n"))}
	for (i in 1: length(indList)) { # i is the number of the splitSegment as defined by the grouping
		for (k in 1: length(indList[[i]])) { # k is the number of spectra we will add to each subgroup  (k can be very large)   ######(parallelize this, NOT above !!)
			inds <- indList[[i]][[k]]
			newNirList[[i]][k,] <- apply(NIRsplitList[[i]][inds,], 2, mean) # subselect from the NIR segment and calculate colwise average
			newHeaderList[[i]][k,] <- splitList[[i]][inds[1],] # (splitList contains the header segments) just get the first of the obervations to be averaged
			newColRepList[[i]][k,] <- colRepSplitList[[i]][inds[1],] # same
		} # end for k
		if (!.ap2$stn$allSilent) {cat(".")}
	} # end for i
	# now we have to get the new elements out of the list into each one object
	allNewHeader <- do.call("rbind", newHeaderList)
	allNewHeader[,ncol(allNewHeader)] <- rep(txtBlow, nrow(allNewHeader))
	colnames(allNewHeader) <- colnames(header)
	allNewColRep <- do.call("rbind", newColRepList)
	allNewColRep[,ncol(allNewColRep)] <- rep(colBlow, nrow(allNewColRep))	
	colnames(allNewColRep) <- colnames(colRep)
	allNewNir <- do.call("rbind", newNirList)
	colnames(allNewNir) <- colnames(NIR)
	# (? maybe a problem with rownames?)
	# now fuse together with the original data
	tsi <- which(colnames(header) == "Timestamp")
	if (length(tsi) != 0) {
		allNewHeader[,tsi] <- as.POSIXct(allNewHeader[,tsi], origin="1970-01-01")
	}
	header <- rbind(header, allNewHeader)
	snrs <- range(header[,snCol]) 
	header[,snCol] <- seq(min(snrs), max(snrs), length.out=nrow(header))
	colRep <- rbind(colRep, allNewColRep)
	NIR <- as.matrix(rbind(NIR, allNewNir))
	for (i in 1: ncol(header)) {
		if (facInd[i]) {
			header[,i] <- as.factor(header[,i]) # re-factorize if necessary
		}
	} # end for i
	out <- new("aquap_data", data.frame(I(header), I(colRep), I(NIR)), metadata=dataset@metadata, anproc=dataset@anproc, ncpwl=dataset@ncpwl, version=dataset@version)
	if (!.ap2$stn$allSilent) {cat(" ok.\n")}
	return(out)
	## XXX still required:
	# include consec. scans together
} # EOF
