getTruePercTestAndRounds <- function(percTest) {
	nrOuterLoops <- round(100/percTest, 0)
	testPerc <- round(100/nrOuterLoops, 1)
	return(list(perc=testPerc, nrep=nrOuterLoops))
} # EOF

makeClassifDataFrameForBarplot <- function(avgTable, sdTable, lte=NULL) {
	if (is.null(avgTable)) {
		return(NULL)
	}
	# both incoming tables (have to) have the same names and dimensions
	avgNum <- as.numeric(avgTable)
	sdNum <- as.numeric(sdTable)
	namesList <- attr(avgTable, "dimnames")
	namesTrue <- rep(namesList$true, each=nrow(avgTable))
	namesPred <- rep(namesList$predicted, nrow(avgTable))
	lteStrip <- unlist(lapply(strsplit(lte, " "), function(x) x[1])) # is stripping away the "N=x" part, because when using grep we could have double matches and only the first one is taken, now we can use which
	if (!is.null(lte)) {
		for (i in 1: length(namesPred)) {
			namesPred[i] <- lte[which(namesPred[i] == lteStrip)]
#			namesPred[i] <- lte[grep(namesPred[i], lte)]
		}
	}
	out <- data.frame(true=namesTrue, predicted=namesPred, avg=avgNum, SD=sdNum)
	return(out)	
} # EOF

makeAvgLegTxt <- function(naNum, char="~") { # the input is a named numeric
	out <- paste0(names(naNum), " N", char, naNum) # we MUST NOT have more than the one space " " in there !!!!!
	return(out)
} # EOF

secureTRUE_FALSE_inColnames <- function(tbl) {
	if (any(c("TRUE", "FALSE") %in% rownames(tbl)) ) {
		rownames(tbl) <- paste0(rownames(tbl), ".")
	}
	if (any(c("TRUE", "FALSE") %in% colnames(tbl)) ) {
		colnames(tbl) <- paste0(colnames(tbl), ".")
	}	
	return(tbl)
} # EOF

### CORE ###
plot_classif_typeClassOn <- function(type, classOn, cvSummaryList, testSummaryList, method, apCl, ap, expName, idString, colorLegValues, apClUser, nrCvTrain, nrCvPred, grpCvPred, nrTestCv, nrTestPred, grpTestPred, exDat) { # we are already in the PDF or not
#	cat(paste0("Type: ", type, "; classOn: ", classOn, " (", method, ")\n")) ; cat("CV Summary \n") ; print(cvSummaryList); cat("Test Summary \n"); print(testSummaryList); wait()
#	cat(paste0("nrCvTrain: ", nrCvTrain, "; nrCvPred: ", nrCvPred, "\ngrpCvPred:\n")); print(grpCvPred); cat("\n\n\n")
	# ap is the ap coming from where the user wants it (the aps trick)
	# apClUser is the classification specific part of the ap taken from the user-defined ap (above); this can be used for real-time plotting changes
	# apCl is the classification specific part taken from the master list, i.e. that was added to the calculation results
	####
	# read in from the settings
	cvTestInOnePage <- .ap2$stn$cl_plot_CVandTestInOnePage
	txtSizeTables <- .ap2$stn$cl_plot_baseTextSizeTables
	charCV <- .ap2$stn$cl_plot_CharForCV
	charTest <- .ap2$stn$cl_plot_CharForTest
	charIndPredExternal <- .ap2$stn$cl_plot_CharForIndepPredExtData
	charPcaRed <- .ap2$stn$cl_plot_CharForPcaReduction
	addConfTables <- .ap2$stn$cl_plot_addConfusionTables
	inclSDtable <- .ap2$stn$cl_plot_includeSDtables
	titTblAvg <- .ap2$stn$cl_plot_avgTableTitle
	titTblSD <- .ap2$stn$cl_plot_sdTableTitle	
	tlbPad <- .ap2$stn$cl_plot_confTablePadding
	useDataCols <- .ap2$stn$cl_plot_useColorsFromDataset
	colorErrorbar <- .ap2$stn$cl_plot_colorErrorBar
	rndNoo <- .ap2$stn$cl_gen_digitsRoundNrObservations
	SD <- avg <- predicted <- true <- NULL # to prevent warnings in checking (needed when creating the plots)
	tilde <- "~"
	#
	titFsize <- txtSizeTables + 2
	if (exDat) { charCV <- charIndPredExternal }
#	grpCvPred <- as.character(grpCvPred)
#	grpTestPred <- as.character(grpTestPred)
	########################
	# get the confusion tables from the lists, make data frame for barplot
	cvConfAvg <- cvSummaryList$errors$avg
	cvConfSD <- cvSummaryList$errors$SD
	cvCorrClassAvg <- cvSummaryList$corrClass$avg
	cvCorrClassSD <- cvSummaryList$corrClass$SD
	#
	testConfAvg <- testSummaryList$testErrors$avg
	testConfSD <- testSummaryList$testErrors$SD
	testCorrClassAvg <- testSummaryList$testCorrClass$avg
	testCorrClassSD <- testSummaryList$testCorrClass$SD
	###	
#	lte <- colorLegValues$txtE
	if (exDat) {nEqChar="="} else {nEqChar=tilde}
	cvDF <- makeClassifDataFrameForBarplot(cvConfAvg, cvConfSD, lte=makeAvgLegTxt(grpCvPred, char=nEqChar)) # transform the confusion tables into data frames that we can hand over to the barplot of ggplot
	testDF <- makeClassifDataFrameForBarplot(testConfAvg, testConfSD, lte=makeAvgLegTxt(grpTestPred))
	########################
	# prepare the confusion tables in grobs, add title etc.
	titAvgGr <- grid::textGrob(titTblAvg, gp = grid::gpar(fontsize=titFsize, fontface="italic"))
	titSDGr <- grid::textGrob(titTblSD, gp = grid::gpar(fontsize=titFsize, fontface="italic"))
	padding <- grid::unit(0.5,"line")
	tt <- gridExtra::ttheme_default(base_size=txtSizeTables, colhead=list(fg_params = list(parse=TRUE)), padding = grid::unit(c(tlbPad, tlbPad), "mm")) 
	#
	grobCvAvg <- gridExtra::tableGrob(secureTRUE_FALSE_inColnames(cvConfAvg), theme=tt) # because for unknown reasons "TRUE" in the colnames does not work (in the rownames it does)
	grobCvAvg <- gtable::gtable_add_rows(grobCvAvg, heights = grid::grobHeight(titAvgGr) + padding, pos=0)
	grobCvAvg <- gtable::gtable_add_grob(grobCvAvg, list(titAvgGr), t=c(1), l=c(1), r=ncol(grobCvAvg))
	#
	grobCvSD <- gridExtra::tableGrob(secureTRUE_FALSE_inColnames(cvConfSD), theme=tt) # because for unknown reasons "TRUE" in the colnames does not work (in the rownames it does)
	grobCvSD <- gtable::gtable_add_rows(grobCvSD, heights = grid::grobHeight(titSDGr) + padding, pos = 0)
	grobCvSD <- gtable::gtable_add_grob(grobCvSD, list(titSDGr), t=c(1), l=c(1), r=ncol(grobCvSD))
	if (inclSDtable) {
		cvTablesGrob <- gridExtra::arrangeGrob(grobCvAvg, grobCvSD, nrow=2, ncol=1) #### the final right part of the CV graphics
	} else {
		cvTablesGrob <- grobCvAvg
	}
	#
	testTablesGrob <- NULL # because the test data can be NULL
	haveTest <- FALSE # will be overwritten below
	if (!is.null(testConfAvg)) {
		grobTestAvg <- gridExtra::tableGrob(testConfAvg, theme=tt)
		grobTestAvg <- gtable::gtable_add_rows(grobTestAvg, heights = grid::grobHeight(titAvgGr) + padding, pos = 0)
		grobTestAvg <- gtable::gtable_add_grob(grobTestAvg, list(titAvgGr), t=c(1), l=c(1), r=ncol(grobTestAvg))
		#	
		grobTestSD <- gridExtra::tableGrob(testConfSD, theme=tt)
		grobTestSD <- gtable::gtable_add_rows(grobTestSD, heights = grid::grobHeight(titSDGr) + padding, pos = 0)
		grobTestSD <- gtable::gtable_add_grob(grobTestSD, list(titSDGr), t=c(1), l=c(1), r=ncol(grobTestSD))
		haveTest <- TRUE
		if (inclSDtable) {
			testTablesGrob <- gridExtra::arrangeGrob(grobTestAvg, grobTestSD, nrow=2, ncol=1, heights=c(1,1)) #### the final right part of the test graphics
		} else {
			testTablesGrob <- grobTestAvg
		}
	} # end !is.null(testConfAvg)
	########################
	# prepare additional text for title, subtitle, caption; colors etc.
	onMain <- paste(expName, ap$genPlot$onMain, idString, sep=" ")	
	onSub <- ap$genPlot$onSub;	if (onSub != "") {onSub <- paste0(" - ", onSub)} # if we wanted to add the onSub e.g. to the subtitle
	cvTitle <- paste0(onMain, "  |  ", classOn)
	testTitle <- paste0(onMain, "  |  ", classOn)
	aa <- getTruePercTestAndRounds(apCl$percTest)
		trueTestPerc <- aa$perc
		nRepeats <- aa$nrep
	pcaRedChar <- ""; if (apCl$pcaRed) {pcaRedChar <- paste0(" (", charPcaRed, ")")}
#	totN <- colorLegValues$sumPart
	totN <-  nrCvTrain + nrCvPred # Problem when bootstrap !! XXX
	grpCols <- colorLegValues$color_legend # ?$color_unique?;  $txtE
	#
	repeatsAdd <- "" ; capAvgAdd <- "avg. "; equalsCharSub <- equalsCharCap <- "="
	if (haveTest) { 
		equalsCharSub <- equalsCharCap <- tilde
	}
	real_nrCvPred <- round(sum(grpCvPred), rndNoo) # because in the case of bootstrap the incoming number (nrCvPred) is only an approximation, so we average the real numbers that we collected
	nrCvTrain_use <- nrCvTrain
	if (grepl("boot", method)) {
		if (!exDat) { # exDat: we provide external data for independent classification
#			nrCvTrain_use <- nrCvTrain - real_nrCvPred # in case of boot we give out the real number of observations that the models were built on # this was used before the appearance of the external data
			nrCvTrain_use <- nrCvTrain - 0 #  
			equalsCharSub <- equalsCharCap <- tilde
		} else {
			nrCvTrain_use <- nrCvTrain - 0 # XXX Wrong ###
			if(is.wholenumber(nrCvTrain_use)) {equalsCharCap <- "="} else {equalsCharCap <- tilde}
		}
	} else { # so we are in traditional CV
		equalsCharCap <- "="
		capAvgAdd <- ""
	}
	if (haveTest) {
		repeatsAdd <- paste0(capAvgAdd, "(", nRepeats, " repeats) ")		
	}
	subCV <- paste0(charCV, ": ", cvCorrClassAvg, "% (sd ", cvCorrClassSD, "%) avg. correct classification; ", repeatsAdd, "N.pred", equalsCharSub, real_nrCvPred, onSub)
	subTest <- paste0(charTest, ": ", testCorrClassAvg, "% (sd ", testCorrClassSD, "%) avg. correct classification; ", nRepeats, " x ", trueTestPerc, "% test data, avg. N.test", tilde, nrTestPred, onSub)
	comCap <-  paste0(type, ", total N=", totN, ", ", repeatsAdd, "N.mod", equalsCharCap, nrCvTrain_use, "  |  ", method, pcaRedChar)	
	xlab <- "true class"
	ylab <- "average predicted class [%]"
	errBarWidth <- 0.25; if (nrow(cvConfAvg) > 7) {errBarWidth <- 0.1} 
	########################
	# prepare the plots	
#	limits <- ggplot2::aes(ymax = avg + SD, ymin = avg - SD) ### !!! ### appears to be wrong, as there are cases where we got below 0 -- and that cant well be
	limits <- ggplot2::aes(ymax = avg + (SD/2), ymin = avg - (SD/2)) # SD is a colname in the dataframe
	geomBar <- ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge())
	geomErrBar <- ggplot2::geom_errorbar(limits, width=errBarWidth, position = ggplot2::position_dodge(width = 0.9), color=colorErrorbar)
	custColFil <- ggplot2::scale_fill_manual(values=as.character(grpCols)) # strange, but that is what is needed. yes.
	if (!useDataCols) {
		custColFil <- NULL
	}
	#
	yLim <- ggplot2::ylim(0, 100)
	cvPlot <- ggplot2::ggplot(cvDF, ggplot2::aes(fill=predicted, y=avg, x=true)) + geomBar + geomErrBar + yLim + custColFil + ggplot2::labs(title=cvTitle, subtitle=subCV, caption=comCap, x=xlab, y=ylab)
	testPlot <- testPlotAll <- NULL
	if (!is.null(testDF)) {
		testPlot <- ggplot2::ggplot(testDF, ggplot2::aes(fill=predicted, y=avg, x=true)) + geomBar + geomErrBar + yLim + custColFil + ggplot2::labs(title=testTitle, subtitle=subTest, caption=comCap, x=xlab, y=ylab)		
	}
	########################
	# arrange graphics and tables, check for NULL test, and finally plot

	if (addConfTables) {
		options(warn=-1)
		cvPlotAll <- gridExtra::arrangeGrob(cvPlot, cvTablesGrob, nrow=1, ncol=2, widths=c(3,1))
		options(warn=0)
		if (!is.null(testPlot)) {
			testPlotAll <- gridExtra::arrangeGrob(testPlot, testTablesGrob, nrow=1, ncol=2, widths=c(3,1))
		}
	} else {
		cvPlotAll <- cvPlot
		testPlotAll <- testPlot
	} # end if addConfTables

	if (cvTestInOnePage) {
		if (!is.null(testPlotAll)) {
			gridExtra::grid.arrange(cvPlotAll, testPlotAll, nrow=2, ncol=1) # "grid.arrange" includes plotting
		} else {
			plot(cvPlotAll)
		}
	} else {
		plot(cvPlotAll)
		if (!is.null(testPlotAll)) {
			plot(testPlotAll)
		}
	} # end if cvTestInOnePage
} # EOF
### CORE ###

plot_classif_CubeElement <- function(set, slotChar, ap, expName, apClUser) { # here we are cycling through the type and the classOn value
	# we want, for each type, and there for each classOn variable, the CV summary and the test summary
	masterList <- slot(set, slotChar)
	idString <- adaptIdStringForDpt(ap, getIdString(set))
	typeList <- vector("list", length(masterList$apCl$type))
	classOnList <- vector("list", length(masterList$apCl$classOn))
	for (i in 1: length(typeList)) { # going through the single types (mainly for DA)
		thisType <- masterList$apCl$type[i]
		siTypeCvSingleList <- masterList$cvBranch[[i]]$cvSingle
		siTypeCvSummaryList <- masterList$cvBranch[[i]]$cvSummary
		siTypeMethodList <- masterList$cvBranch[[i]]$method
		siTypeTestList <- masterList$testBranch[[i]]
		siTypeNumbersList <- masterList$numbers[[i]]
		for (k in 1: length(classOnList)) { # going through the single classOn values
			thisClassOn <- masterList$apCl$classOn[k]
			classOnCvList <- siTypeCvSingleList[[k]]
			classOnCvSummaryList <- siTypeCvSummaryList[[k]]
			thisMethod <- siTypeMethodList[[k]]
			classOnTestList <- siTypeTestList[[k]]
			colorLegValues <- extractColorLegendValues(getDataset(set), thisClassOn)
			#
			nrCvTrain <- siTypeNumbersList$nrsCvTrain[[k]]
			nrCvPred <- siTypeNumbersList$nrsCvPred[[k]]
			grpCvPred <- siTypeNumbersList$grpsCvPred[[k]]
			nrTestCv <- siTypeNumbersList$nrsTestCv[[k]]
			nrTestPred <- siTypeNumbersList$nrsTestPred[[k]]
			grpTestPred <- siTypeNumbersList$grpsTestPred[[k]]
			#
			plot_classif_typeClassOn(thisType, thisClassOn, classOnCvSummaryList, classOnTestList, thisMethod, masterList$apCl, ap, expName, idString, colorLegValues, apClUser, nrCvTrain, nrCvPred, grpCvPred, nrTestCv, nrTestPred, grpTestPred, exDat=FALSE)
		} # end for k (classOn)
	} # end for i (type)
} # EOF

plot_classif_generalHandover <- function(cube, ap, slotChar, apClCube, apClUser) { # cycling through the cube elements; the slotChar comes in from the specific function in file "plot_classif_spec.r"
	# ap is the ap coming from where the user wants it (the aps trick)
	# apClUser is the classification specific part of the ap taken from the user-defined ap (above)
	# apClCube is the classification specific part of the ap taken from the cube (where the status at the time of calculation is reflected)
	where <- ap$genPlot$where
	fns <- ap$genPlot$fns
	height <-.ap2$stn$pdf_Height_classif
	width <- .ap2$stn$pdf_Width_classif
	if (apClCube$percTest <= 0 ) {
		height <- height/2
	}
	path <- .ap2$stn$fn_results
	suffix <- paste0("classification_")
#	message <- paste0("classification")
	expName <- getExpName(cube)
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, "_", slotChar, ".pdf", sep="")
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste0("Plotting ", slotChar, " classification... "))}
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	for (i in 1: length(cube)) {
		plot_classif_CubeElement(set=cube[[i]], slotChar, ap, expName, apClUser)
	}		
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF


# plotting independent classification ----------------------

plot_classif_PredListElement <- function(siPredLi, expName, apCube, anpPlot) { # siPredLi is a single element of the  outermost list of the predList that is referring to a single element i.e. set in the cube
	# we are in a single element deducted from a single cube element
	# the first layer is going through the 4 available methods
	for (i in 1: length(siPredLi)) { # going through the classification methods
		if (!is.null(siPredLi[[i]])) { # if the slot is empty, do nothing
			for (ty in 1: length(siPredLi[[i]])) { # going through the types (mostly in xda)
				for (co in 1: length(siPredLi[[i]][[ty]])) {
					aa <- siPredLi[[i]][[ty]][[co]] # get here everything we need (and can) know about the particular case (type, method, classOn, ...)
					me <- aa$meta
					nr <- aa$numbers  #ist(nrCvTrain=nrCvTrain, nrCvPred=nrCvPred, grpCvPred=grpCvPred)
					slotName=me$slotName # where to put this? 
					##
					plot_classif_typeClassOn(me$type, me$classOn, aa$indPredSummaryList, testSummaryList=NULL, me$method, me$apCl, ap=anpPlot, expName, me$idString, me$colorLegValues, apClUser=NULL, nr$nrCvTrain, nr$nrCvPred, nr$grpIndPred, nrTestCv=NULL, nrTestPred=NULL, grpTestPred=NULL, exDat=TRUE) # exDat: from "external datat": showing if we are coming from an independent EXTERNAL dataset	
					# apClUser is not yet needed downstream, that is for user-modified plotting parameters	
					##
				} # end for co: going through the class-on variables
			} # end for ty: going through the types (mostly in xda)
		} # end !is.null(siPredLi[[i]])
	} # end for i (going through the classification methods
} # EOF

makeIndepClassifXValidationPlots <- function(predList, anpPlot) {
	apCube <- getAnproc(predList)
	expName <- getExpName(predList)
	where <- anpPlot$genPlot$where 
	fns <- anpPlot$genPlot$fns
	height <- (.ap2$stn$pdf_Height_classif) / 2
	width <- .ap2$stn$pdf_Width_classif
	path <- .ap2$stn$fn_results
	suffix <- paste0("indepClassif")
#	message <- paste0("classification")
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting classification for external data... ")}
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	for (i in 1: length(predList)) { # the outermost list in predList reflects the cube elements
		plot_classif_PredListElement(predList[[i]], expName, apCube, anpPlot)
	}		
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF


#' @title Plot Independent Predictions
#' @description Calculate and plot predictions from independent, manually provided 
#' data. One or more of the implemented classifier-models (see e.g. 
#' \code{\link{calc_discrimAnalysis_args}} and the links to other classifier 
#' functions there) have to be present in the cube. The manually provided data 
#' in \code{indepDataset} are then projected into each single classification 
#' model present in the cube, and the results are validated using either the class 
#' variable present in the independent dataset that has the exactly same name as the 
#' class variable used to generate the models, or a user-defined class variable 
#' (parameter \code{icv}) can be used for validation.
#' @details For every single element in the cube, i.e. for every split-variation 
#' of the original dataset (as produced in \code{\link{gdmm}} the according 
#' subgroups within the independent dataset are constructed. In case of a dataset 
#' resulting in no observations the process is aborted. Also, the data pre- and
#' post- treatments (see \code{\link{dpt_modules}}) as defined in the analysis 
#' procedure used to produce the cube are applied to the independent dataset resp. 
#' to its subgroups as defined by the application of possible split-variables 
#' (see above).
#' If \code{toXls} is TRUE, the results of the predictions will be exported to Excel.
#' @param indepDataset The dataset containing the independent data. An object 
#' of class 'aquap_data' as produced by \code{\link{gfd}}. 
#' @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}. 
#' It is an error to have no classification models in the cube.
#' @param ccv "Cube class variable", character vector or NULL. The names of 
#' one or more class variables in the cube on which classification models have 
#' been calculated. Leave at the default NULL to use \strong{all} of the class 
#' variables on which a classification model has been calculated, or provide a 
#' character vector with valid variable names for a sub-selection. For the selected 
#' variables, predictions from the data in the independent dataset will be made. 
#' If argument \code{icv} is left at its default NULL, class variables with 
#' exactly the same name are looked for in the independent dataset and, if 
#' present, are used for validating the predictions.
#' @param icv "Independent class variable", character vector or NULL. The names 
#' of class variables in the independent dataset. If left at the default 
#' NULL, class variables in the independent dataset with exactly the same 
#' name(s) as specified in argument \code{ccv} are looked for and, if present, 
#' are used for validating the predictions. If a character vector is provided, 
#' it has to have the same length as the one in \code{ccv}, and those variables 
#' will be used, in the given sequence, for validating the predictions.
#' @param apPlot The analysis procedure used for plotting.
#' @param pl Logical, defaults to TRUE. If predicted data should be plotted 
#' at all. If FALSE, only the calculation and the (possible) export to an excel 
#' file (see details) will be performed.
#' @param toxls 'def' or logical. If left at the default 'def' the value from 
#' \code{cl_indepPred_exportToExcel} in the settings file is used. Set to TRUE 
#' or FALSE to directly control if export of predicted data to excel should be 
#' performed or not.
#' @param info 'def' or logical. If left at the default 'def' the value from 
#' \code{cl_indepPred_showInfo} in the settings file is used. Set to TRUE 
#' or FALSE to directly control if information regarding the pairing of class 
#' variables in the model and those in the independent dataset used for validation 
#' should be displayed.
#' @param confirm 'def' or logical. If left at the default 'def' the value from 
#' \code{cl_indepPred_confirm} in the settings file is used. Set to TRUE 
#' or FALSE to directly control if manual confirmation is required after the 
#' (possible) display of pairing-information (see above). Ignored if \code{info} 
#' is \code{FALSE}.
#' @param predList NULL or list. If left at the default null, the independent 
#' dataset is used to make predictions on all available classification models,
#' resulting in a list containing the predictions. If this list is in turn 
#' provided to \code{predList}, no calculations are performed and the results 
#' are plotted straight away.
#' @param ... General plotting parameters, see XXX.
#' @return An (invisible) list containing the numerical results of the 
#' predictions, and if parameter \code{toXls} is TRUE, these data are exported 
#' to an excel file in the results folder as well.
#' @examples
#' \dontrun{
#' fd <- gfd() # loading or importing the rawdata
#' fd1 <- ssc(fd, C_Foo!="bar") # manually splitting up the dataset
#' fd2 <- ssc(fd, C_Foo=="bar") 
#' cube <- gdmm(fd1) # this is assuming that the standard analysis procedure is set
#' # up to perform a classifier method
#' # we are using `fd1` to produce the cube, and then `fd2` as independent dataset
#' # to perform independent predictions on all the models in the cube
#' predictions <- plot_classifX_indepPred(fd2, cube)
#' predictions <-  plot_classifX_indepPred(fd2, cube, icv="C_blabla", pl=FALSE)
#' # to redirect the original validation to class-variable "C_blabla"
#' }
#' @family Classification functions
#' @family Plot functions
#' @export
plot_classifX_indepPred <- function(indepDataset, cube, ccv=NULL, icv=NULL, pl=TRUE, toxls="def", info="def", confirm="def", predList=NULL, apPlot="def", ...) {
	autoUpS()
	if (!is.null(predList)) {
		anpPlot <- modifyThisAp(getAnproc(predList), ...)
	}
	if (is.null(predList)) {
		anpPlot <- doApsTrick(aps="cube", cube, ...)
		dsName <- deparse(substitute(indepDataset))
		cubeName <- deparse(substitute(cube))
		cubeID <- paste(cubeName, cube@timestamp, sep=" @ ")
		check_indXClassifPrediction_input(indepDataset, cube, ccv, icv, cubeName, dsName, toxls, info, confirm) ## !! is assigning ccv, icv, toxls, info, confirm
		apCu <- getap(.lafw_fromWhere="cube", cube=cube)
		if (!.ap2$stn$allSilent & .ap2$stn$cl_indepPred_printPairingMsg) {
			printMessagePairing_classif(ccv, icv, info, confirm)
		}	
		if (length(cube) == 1) {ad1 <- ""} else {ad1 <- "s"};  	if (length(ccv) == 1) {ad2 <- ""} else {ad2 <- "s"}
		if (!.ap2$stn$allSilent) {cat(paste0("Calc. classification predictions for ", length(cube), " cube element", ad1, " (", length(ccv), " model", ad2, " each)... "))}
		predList <- calculateIndepClassifXPrediction(indepDataset, cube, ccv, icv, apCu, cubeID) #### CORE #### calculation 
		if (!.ap2$stn$allSilent) {cat("ok\n")}
		###
		toxls=FALSE # XXX dev only
		if (toxls) {
			if (!.ap2$stn$allSilent) {cat(paste0("Exporting predicted data to excel file... "))}
			message("NOT YET IMPLEMENTED")		
		#	exportPredListToExcel(cube, predList, anpPlot) #### export to excel ####
			if (!.ap2$stn$allSilent) {cat("ok\n")}
		} # end predToXls
		###
	} else { # end is.null(predList) # so we provide a predList, that means we want only to plot
		# so here we HAVE provided input to `predList`
		if (class(predList) != "aquap_ipl") {
			stop(paste0("Please provide an object of class `aquap_ipl` to the argument `predList`."), call.=FALSE)
		}
		pl=TRUE
	}
	# now plot, please (use the ap2 now!)
	if (pl) {
		aa <- try(makeIndepClassifXValidationPlots(predList, anpPlot)) #### plot the results ####
		if (class(aa) == "try-error") {
			message("Sorry, there was an error during plotting.")
		}
	}
	###	
	return(invisible(predList))
} # EOF
