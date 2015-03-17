#' @title Perform Smoothing or m-th Derivative
#' @description Performs a smoothing and / or the calculation of the 
#' m-th derivative.
#' on the given dataset using the Savitzky-Golay filter.
#' @details The underlying function is \code{\link[signal]{sgolayfilt}}.
#' @param dataset An object of class 'aquap_data'
#' @param p Numeric length 1, the order of the filter.
#' @param n Numeric length 1, the filter length, must be odd.
#' @param m Numeric length 1, the m-th derivative.
#' @return Returns the dataset with the NIR-data smoothed or transformed.
#' @family Data pre-treatment functions
#' @export
do_sgolay <- function(dataset, p=2, n=21, m=0) {
	cns <- colnames(dataset$NIR)
	rns <- rownames(dataset$NIR)
	dataset$NIR <- t(apply(dataset$NIR, 1, signal::sgolayfilt, p=p, n=n, m=m))
	colnames(dataset$NIR) <- cns
	rownames(dataset$NIR) <- rns
	return(dataset)
} # EOF


#' @title Perform MSC
#' @description Performs MSC on the provided data
#' @details The underlying function is \code{\link[pls]{msc}}.
#' @param dataset An object of class 'aquap_data'
#' @param reference XXX
#' @return Returns the transformed dataset
#' @family Data pre-treatment functions
#' @export
do_msc <- function(dataset, reference=NULL) {
	NIR <- pls::msc(dataset$NIR, reference)
	colnames(NIR) <- colnames(dataset$NIR)
	dataset$NIR <- NIR
	return(dataset)
} # EOF



do_scale <- function(dataset) {
	NIR <- som::normalize(dataset$NIR, byrow=FALSE)
	colnames(NIR) <- colnames(dataset$NIR)
	rownames(NIR) <- rownames(dataset$NIR)
	dataset$NIR <- NIR
	return(dataset)
} # EOF


do_scale_fc <- function(dataset, calibAvgTable) {
	avg <- apply(calibAvgTable, 2, mean)
	NIR <- scale(as.matrix(dataset$NIR), center=avg, scale=TRUE)
	colnames(NIR) <- colnames(dataset$NIR)
	rownames(NIR) <- rownames(dataset$NIR)
	dataset$NIR <- NIR
	return(dataset)
} # EOF
