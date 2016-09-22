plotSpectra_outer <- function(dataset, colorBy, onMain, onSub, idString="") {
	if (!is.null(colorBy)) {
		if (any(colorBy == "all")) {	
			cPref <- .ap2$stn$p_ClassVarPref
			le <- nchar(cPref)
			cns <- colnames(dataset$header)
			cnsM <- substr(cns, 1, le)
			ind <- which(cnsM == cPref)
			colorBy <- cns[ind] # get here all the classvariables
		}
	}
	cns <- colnames(dataset$header)
	if (!all(colorBy %in% cns)) {
		a <- which(! colorBy %in% cns)
		stop(paste("Sorry, the class-variable '", paste(colorBy[a], collapse=", "), "' does not exist. Please check your input.", sep=""), call.=FALSE)
	}	
	if (is.null(colorBy)) {
		plotSpectra_inner(dataset, singleColorBy=NULL, onMain, onSub, idString)
	} else {
		for (i in 1: length(colorBy)) {
			plotSpectra_inner(dataset, colorBy[i], onMain, onSub, idString)
		} # end for i	
	} 
} # EOF

plotSpectra_inner <- function(dataset, singleColorBy, onMain, onSub, idString="") {
	if (is.null(singleColorBy)) {
		colorData <- 1
		msg <- ""
		makeLegend <- FALSE
#		lty <- 1
	} else {
		makeLegend <- TRUE
		cns <- colnames(dataset$colRep)
		colInd <- which(cns == singleColorBy)
		color <- dataset$colRep[,colInd]
		msg <- paste("Color by ", cns[colInd], sep="")
		aa <- extractColorLegendValues(dataset, singleColorBy)  # color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping
		colorData <- aa$color_data
		colorLegend <- aa$color_legend
		legendTextExt <- aa$txtE
	} # end else
	dptInfo <- adaptIdStringForDpt(getAnproc(dataset), idString)
	onMain <- paste(onMain, dptInfo, sep="")
	wls <- getWavelengths(dataset)
	onSub <- paste(onSub, msg, sep=" ")
	matplot(wls, t(dataset$NIR), type="l", xlab="Wavelengths", ylab="Absorbance", main=onMain, sub=onSub, col=colorData, lty=1)
	abline(h=0, col="gray")
	if (makeLegend) {
		legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
#		legPosition <- getCheckLegendPosition(as.numeric(matrix(rep(wls, ncol(t(dataset$NIR))), nrow=1)), as.numeric(matrix(t(dataset$NIR), nrow=1, byrow=TRUE)))
		legPosition <- "topright"
#		print(legPosition)
		Cex <- 0.8
		nlc <- 1
		if (length(legendTextExt) > 18) {
			Cex <- 0.5
			nlc <- 2
		}
		legend(legPosition, legend=legendTextExt, col=colorLegend, lty=1, bg=legBgCol, cex=Cex, ncol=nlc)
	}
} # EOF

plot_spectra_Data <- function(x, colorBy=NULL, ...) {
	autoUpS()
	dataset <- x
	ap <- getap(...)
	md <- getmd()
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	#
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting raw spectra ... ")}
#	expName <- getExpName(md)
	expName <- getExpName(dataset)
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- "rawSpectra"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & (Sys.getenv("RSTUDIO") != 1) & (!names(dev.cur()) == "quartz") ) {dev.new(height=height, width=width)}	
	plotSpectra_outer(dataset, colorBy, onMain, onSub)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

plot_spectra_Data_nottoGutto_notInUse <- function(x, colorBy=NULL, ...) {
	# x is a dataset
	autoUpS()
	ap <- getap(fn="def")
	ap <- setAllSplitVarsToNull(ap)
	cube <- gdmm(x, ap=ap) ##### generate the cube !!! ######
	prev <- .ap2$stn$allSilent
	.ap2$stn$allSilent <<- TRUE
	origAp <- getAnproc(x)
	origDptMods <- origAp$dpt$dptModules
	cube@anproc$dpt$dptModules <- origDptMods # put the dpt info that we got from the dataset into the cube AFTER the gdmm, so that we do not do the treatment again!!
	.ap2$stn$allSilent <<- prev
	plot_spectra_Cube(cube, colorBy, ...)	
} # EOF

plot_spectra_Cube <- function(x, colorBy=NULL, ...) {
	# the incoming x is the cube
	autoUpS()
	ap <- getap(...)
	md <- getmd()
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	#
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting raw spectra ...  \n")}
#	expName <- getExpName(md)
	expName <- getExpName(x)
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- "rawSpectra"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & (Sys.getenv("RSTUDIO") != 1)) {dev.new(height=height, width=width)}	
	for (i in 1: length(x)) {
		dataset <- getDataset(x[[i]]) # the sets are in the list within the cube
		idString <- getIdString(x[[i]])
		if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste("   working on #", i, " of ", length(x), "\n", sep=""))}
		plotSpectra_outer(dataset, colorBy, onMain, onSub, idString)
	} # end for i
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

#' @title Plot Raw Spectra
#' @description Plot the raw spectra contained in the dataset or in the cube.
#' @details If the provided object is of class \code{\link{aquap_cube}} and is 
#' containing more than one dataset, a single graphic with raw-spectra is 
#' produced for every dataset contained within the cube.
#' @param x The standard dataset as produced by \code{\link{gfd}}, or a data-cube
#' as produced by \code{\link{gdmm}}.
#' @param colorBy Character vector, possible values are the class variables in 
#' the dataset. Provide a character vector with length > 1 to color along all 
#' these class variables; provide 'all' to use all available class variables for 
#' coloring - see examples.
#' @param ... Optional general plotting options as defined in 
#' \code{\link{plot_pg_args}}.
#' @seealso \code{\link{plot_all_modells}}
#' @examples
#' \dontrun{
#' plot_spectra(dataset)
#' plot_spectra(dataset, colorBy="all")
#' plot(dataset) # the same as above
#' plot(dataset, colorBy=c("C_Group"))
#' plot_spectra(dataset, "C_Group")
#' plot_spectra(dataset, c("C_Group", "C_Repl")) # produces two plots
#' plot(dataset, pg.where="") # for plotting to graphic device
#' plot_spectra(dataset, c("C_Group", "C_Repl"), pg.where="", pg.main="foo")
#' cube <- gdmm(gfd())
#' plot_spectra(cube)
#' }
#' @family Plot functions
#' @family Plot arguments
#' @family Core functions
#' @name plot_spectra
NULL
