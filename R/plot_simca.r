simca_plotClassDistances <- function(SimcaPredObj, SimcaModel, distMat, where="", onMain="", onSub="", distType="od", dataset, grouping) {
	obj <- SimcaPredObj
	m <- ncol(SimcaPredObj@ct)
	XSimca <- substr(as.character(SimcaModel@call)[1], 1,6)
	if (XSimca == "CSimca") {
		version <- "classic"
	} else {
		if (XSimca == "RSimca") {
			version <- "robust"
		} else {
			version <- "?version?"
		}
	}
	if (distType == "od") {
		score <- obj@odsc
	} else {
		if (distType == "sd") {
			score <- obj@sdsc
		} else {
			stop("Please provide either 'od' or 'sd' for the argument 'distType'. I hope you have a good day.", call.=FALSE)
		}
	} # end else
	###
	aa <- extractColorLegendValues(dataset, groupBy=grouping)  # color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
	classColors <- aa$color_data
	legendText <- aa$txtE
	legendColors <- aa$color_legend
	sumPart <- aa$sumPart
	###
	mainText <- paste0("grp: ", grouping, "   ", onMain)
	for (i in 1:(m-1)) {
		for (k in (i+1):m) {
		#	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new()}	
				Dist <- round(distMat[i,k], .ap2$stn$simca_nrDigitsRoundDist)
				xLab <- colnames(distMat)[i]
				yLab <- rownames(distMat)[k]
				subText <- paste0(onSub, "   all N=", sumPart, "  (", version, ") ", " class distance (", strsplit(xLab, "@")[[1]][[1]], ", ", yLab, ") = ", Dist)
				plot(score[,i], score[,k], col=classColors, xlab=xLab, ylab=yLab, main=mainText, sub=subText)
				abline(h=1, v=1, col="gray")
				legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
				legend("topright", legend=legendText, pch=1, col=legendColors, bg=legBgCol)
		} # end for k
	} # end for i
} # EOF

simca_barplotClassDistances <- function(classDistanceMatrix, where="", onMain="", onSub="", dataset, grouping, icdRan) {
	cdm <- classDistanceMatrix
	###
	aa <- extractColorLegendValues(dataset, groupBy=grouping)  # color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
	legendText <- aa$txt
	legendColors <- aa$color_legend	
	### 
	legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
	mainText <- paste0("grp: ", grouping, "   ", onMain)
	oldMai <- par()$mai
	par(mai=c(2.2, 0.82, 0.82, 0.42))
	insetVal <- c(0, -0.37)
	subDownVal <- 6.5
	roPerc <- 10 # the percents to add to the range of the object for the ylim
	icHorizon <- .ap2$stn$simca_BarBlot_horizontalLine
	if (!all(is.numeric(icHorizon)) | length(icHorizon) != 1) {icHorizon <- NULL}
	plotBarsPlusLegend <- function(obj, legTxt, ran=icdRan) {
		if (!is.null(ran)) {
			ranObj <- range(obj)
			if (max(ranObj) > ran) {
				ran <- max(ranObj) + ((max(ranObj)/100)*roPerc)
			}
		} # end !is.null
		if (is.null(ran)) { ranUse <- NULL	} else { ranUse <- c(0, ran) }
		barplot(obj, beside=T, ylim=ranUse, ylab="Interclass Distance", cex.names=0.8, las=2, main=mainText, sub="", col=legendColors)
		abline(h=icHorizon, col="gray", lwd=0.7)
		title(sub=onSub, line=subDownVal)
		if (length(legTxt) != 0) {
#			legend("bottom", legend=legendText, text.col=legendColors, xpd=TRUE, horiz=TRUE, inset=insetVal, bty="n", bg=legBgCol)
			legend("bottom", legend=legTxt, text.col=legendColors, xpd=TRUE, horiz=FALSE, ncol=6, cex=0.9,  inset=insetVal, bty="n", bg=legBgCol)	
		} # end if
	}# EOIF
	##
	plotBarsPlusLegend(cdm, legTxt=legendText)
	##
	mx <- .ap2$stn$simca_maxBarsPerGraph
	if (ncol(cdm) > mx) {
		a <- floor(ncol(cdm)/mx)
		prevInd <- 0
		for (i in 1:a) {
			cdmPart <- cdm[,(prevInd+1):(i*mx)]
			prevInd <- i*mx
			plotBarsPlusLegend(cdmPart, legTxt=rownames(cdmPart))
		}
		rest <- ncol(cdm) - (a*mx)
		cdmRest <- cdm[, (ncol(cdm)-rest+1) : ncol(cdm)]
		plotBarsPlusLegend(cdmRest, legTxt=rownames(cdmRest))		
	} # end if
	par(mai=oldMai) # back to before
} # EOF

makeSimcaClassDistPlots_inner <- function(set, ap, where, onMain, onSub, con_simcaTable=NULL, distType, icdRan) { # is working within a single set, we can have more than one grouping variable !!
	groupingVec <- getCorrectSimcaClasses(set)
	leng <- length(groupingVec)
	mods <- getSIMCAModels(set)
	preds <- getSIMCAPredictions(set)
	icDists <- getSIMCAicDists(set)
#	mods_cv <- getSIMCAModels_cv(demarMod)
	preds_cv <- getSIMCAPredictions_cv(set)
	dataset <- getDataset(set)
	idString <- adaptIdStringForDpt(ap, getIdString(set))
	onMain <- paste0(onMain, idString)
	for (i in 1: leng) {
#		colRepSimcOn <- numRep[, groupingVec[i]]
		simca_barplotClassDistances(icDists[[i]], where, onMain, onSub, dataset, groupingVec[i], icdRan)
		simca_plotClassDistances(preds[[i]], mods[[i]], icDists[[i]], where, onMain, onSub, distType, dataset, groupingVec[i])
#		mainText <- paste(groupingVec[i], " - ", onMain, sep="")
		mainText <- paste0("grp: ", groupingVec[i], "   ", onMain)
		if (.ap2$stn$simca_tablesToTxt == TRUE) {	
			if (where == "pdf") {
				sink(file=con_simcaTable, append = TRUE, type = c("output"), split = FALSE)
			}
			cat("\n\n\n")
			cat("-----------------------------------------\n")
			cat("-----------------------------------------\n")
			cat(mainText); cat("\n\n")
			cat("Interclass Distances:\n")
			print(icDists[[i]])
			print(preds[[i]])
			cat("------------------\n\n")
			cat(paste("Predictions based on ", .ap2$stn$simca_percNewData_CV, "% new data:\n", sep=""))
			print(preds_cv[[i]])
			cat("-----------------------------------------\n")
			if (where == "pdf") {
				sink()
			}
		} # end if	
	} # end for i
} # EOF

makeSimcaClassDistPlots <- function(cube, ap, where, onMain, onSub, con_simcaTable, distType, icdRan) { # is going through the cube
	for (i in 1: length(cube)) {
		makeSimcaClassDistPlots_inner(cube[[i]], ap, where, onMain, onSub, con_simcaTable, distType, icdRan) # sending down a single set
	} # end for i
} # EOF

checkSimcaPlottingValues <- function(sim.distType, sim.icdRan) {
	distType <- sim.distType
	if (!all(is.character(distType)) | length(distType) != 1) {
		stop("Please provide a character length one to the argunent 'sim.distType'.", call.=FALSE)
	}
	if (! distType %in% c("od", "sd")) {
		stop("Please provide either 'od' or 'sd' to the argument 'sim.distType'.", call.=FALSE)
	}
	assign("distType", distType, pos=parent.frame(n=1))	
	##
	icdRan <- sim.icdRan
	if (!is.null(icdRan)) {
		if (all(icdRan == "def")) {
			icdRan <- .ap2$stn$simca_rangeForDistBarPlots
		}
		if (!is.null(icdRan)) {
			if (!all(is.numeric(icdRan)) | length(icdRan) != 1) {
				stop("Please provide a numeric length one for the argument 'sim.icdRan'.", call.=FALSE)
			}
	#		if (icdRan > pv_warningIcDistRange) {
	#			message(paste0("Alert: The fixed range for the SIMCA interclass-distance in the bar-plots is currently set to ", icdRan))
	#		}
		} # end !is.null
	} # end !is.null
	assign("icdRan", icdRan, pos=parent.frame(n=1))
	##
} # EOF


### CORE ###
plot_simca_cube <- function(cube, aps="def", sim.distType="od", sim.icdRan="def", ...) {
	autoUpS()
	aps <- checkApsChar(aps)
	distType <- icdRan <- NULL
	checkSimcaPlottingValues(sim.distType, sim.icdRan) # !! is assigning here: distType, icdRan
	if (aps == "cube") {
		ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)			 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
	} else {
		check_apDefaults(fn=aps)
		ap <- getap(fn=aps, ...) # load from file, possibly modify via ...
	}
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	if (is.null(ap$simca)) {
		return(cat("*** SIMCA model not available or not selected for plotting \n"))
	}
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	expName <- getExpName(cube)
	height <-.ap2$stn$pdf_Height_sq
	width <- .ap2$stn$pdf_Width_sq
	path <- .ap2$stn$fn_results
	suffix <- "SimcaDist"
	con_simcaTable <- NULL
	#
	if (.ap2$stn$simca_tablesToTxt) { add <- "and CV tables" } else { add <- "" }
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste0("Plotting SIMCA plots ", add, "... ")) }
	filename <- filenameTables <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	fnSimcaClassTables <- paste(path, "/", filenameTables, fns, "_tables", ".txt", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { 
		pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12)
		if (.ap2$stn$simca_tablesToTxt == TRUE) {	
			con_simcaTable <- file(fnSimcaClassTables, "wt")   # open connection to text file
		}
	}
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	makeSimcaClassDistPlots(cube, ap, where, onMain, onSub, con_simcaTable, distType, icdRan)
	if (where == "pdf") {
		dev.off()
		if (.ap2$stn$simca_tablesToTxt == TRUE) {	
			close(con_simcaTable)
		}	
	}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste("ok\n")) }
} # EOF


#' @title Plot SIMCA
#' @description Plot SIMCA interclass distances.
#' @details The width and height of the resulting pdf can be set in the
#'   settings.
#' @param object An object of class 'aquap_cube' as produced by
#'   \code{\link{gdmm}}.
#' @param ... Optional 'simca' plotting parameters to override the values in the
#'   analysis procedure - for possible arguments see \code{\link{plot_sim_args}}
#'   and here below: 
#'   \describe{ 
#'   \item{aps}{Character length one. The default way
#'   to obtain the analysis procedure. Defaults to "def". Possible values are: 
#'   \describe{ 
#'   \item{"def"}{The default from the settings.r file is taken.
#'   (Argument \code{gen_plot_anprocSource})} 
#'   \item{"cube"}{Take the analysis
#'   procedure from within the cube, i.e. the analysis procedure that was used
#'   when creating the cube via \code{\link{gdmm}} is used.} 
#'   \item{"defFile"}{Use the analysis procedure with the default filename as 
#'   specified in the settings.r file in \code{fn_anProcDefFile}.} 
#'   \item{Custom filename}{Provide any valid filename for an analysis procedure 
#'   to use as input for specifying the plotting options.}}} 
#'   \item{sim.distType}{The type
#'   of distance to be calculated, can be either 'od' or 'sd'. Defaults to 'od',
#'   the standardized orthogonal distances (see 
#'   \code{\link[rrcovHD]{PredictSimca-class}} in package 'rrcovHD')}
#'   \item{sim.icdRan}{Can be 'def', NULL, or a numeric length one. The range for 
#'   all the interclass-distance barplots. Set to NULL for no pre-defined range 
#'   at all, or provide a numeric length one to specify the upper limit on the 
#'   Y-axis of the interclass distances to be plotted. If values higher than 
#'   specified appear, the range will be extended automatically. If left at the 
#'   default 'def', the value from the settings.r file (parameter 
#'   \code{simca_rangeForDistBarPlots}) is read in.}
#'   }
#' @return A pdf or graphic device.
#' @family Plot functions
#' @family SIMCA documentation
#' @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_simca(cube)
#'  plot(cube, what="sim", aps="foo.r") # take the analysis procedure from the 
#'  # file 'foo.r'
#'  plot_simca(cube, aps="cube", pg.main="FooBar") # Take the plotting parameter 
#'  from the cube itself, and add 'FooBar' to the main of every graphic.
#'  plot_simca(cube, aps="cube", pg.main="FooBar", pg.where="pdf") # same as 
#'  # above, but have the graphics in a PDF.
#'  plot_simca(cube, pg.main="Foo", pg.fns="_foo") # use the default for aps, 
#'  # add 'Foo' on each main, and add '_foo' to the filename of the possible pdf
#'  plot_simca(cube, sim.icdRan=2)
#'  plot(cube, sim.icdRan=2)
#'  plot_simca(cube, sim.icdRan=NULL) # to not have a pre-defined range
#' }
#' @name plot_simca
NULL


#' @title Plot SIMCA - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{plot}} and \code{\link{plot_simca}} to override the values 
#' in the analysis procedure file and so to modify the graphics - see examples.
#' 
#' \code{plot(cube, ...)}
#' 
#' \code{plot_simca(cube, ...)}
#' 
#' @template mr_details_allParams
#' @template mr_sim_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_simca}}
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube, pg.sub="bar") # adds 'bar' to the subtitle of each single plot
#' plot_simca(cube, pg.where="pdf", pg.sub="bar", pg.fns="_bar") # adds '_bar' 
#' # to the filename of 
#' # the pdf
#' }
#' @family Plot arguments
#' @family SIMCA documentation
#' @name plot_sim_args
NULL



#' @title Calculate SIMCA - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of SIMCA models - see examples.
#' 
#' \code{getap(...)}
#'  
#' \code{gdmm(dataset, ap=getap(...))}
#' 
#' @section Note: Calculation of SIMCA models is done with the function 
#' \code{\link[rrcovHD]{RSimca}} /  \code{\link[rrcovHD]{CSimca}} and with code 
#' adapted from the manual-page of the Chemometrics-software "Pirouette" XXX. 
#' @template mr_details_allParams
#' @template mr_sim_calc_param
#' @seealso \code{\link{gdmm}}
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset, getap(do.sim=TRUE))
#'  cube <- gdmm(dataset, getap(do.sim=TRUE, sim.K=4))
#' }
#' @family Calc. arguments
#' @family SIMCA documentation
#' @name calc_sim_args
NULL
