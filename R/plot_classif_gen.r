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


### CORE ###
plot_classif_typeClassOn <- function(type, classOn, cvSummaryList, testSummaryList, method, apCl, ap, expName, idString, colorLegValues, apClUser, nrCvTrain, nrCvPred, grpCvPred, nrTestCv, nrTestPred, grpTestPred) { # we are already in the PDF or not
#	cat(paste0("Type: ", type, "; classOn: ", classOn, " (", method, ")\n")) ; cat("CV Summary \n") ; print(cvSummaryList); cat("Test Summary \n"); print(testSummaryList)
	# ap is the ap coming from where the user wants it (the aps trick)
	# apClUser is the classification specific part of the ap taken from the user-defined ap (above); this can be used for real-time plotting changes
	# apCl is the classification specific part taken from the master list, i.e. that was added to the calculation results
	####
	# read in from the settings
	cvTestInOnePage <- .ap2$stn$cl_plot_CVandTestInOnePage
	txtSizeTables <- .ap2$stn$cl_plot_baseTextSizeTables
	charCV <- .ap2$stn$cl_plot_CharForCV
	charTest <- .ap2$stn$cl_plot_CharForTest
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
	cvDF <- makeClassifDataFrameForBarplot(cvConfAvg, cvConfSD, lte=makeAvgLegTxt(grpCvPred)) # transform the confusion tables into data frames that we can hand over to the barplot of ggplot
	testDF <- makeClassifDataFrameForBarplot(testConfAvg, testConfSD, lte=makeAvgLegTxt(grpTestPred))
	########################
	# prepare the confusion tables in grobs, add title etc.
	titAvgGr <- grid::textGrob(titTblAvg, gp = grid::gpar(fontsize=titFsize, fontface="italic"))
	titSDGr <- grid::textGrob(titTblSD, gp = grid::gpar(fontsize=titFsize, fontface="italic"))
	padding <- grid::unit(0.5,"line")
	tt <- gridExtra::ttheme_default(base_size=txtSizeTables, colhead=list(fg_params = list(parse=TRUE)), padding = grid::unit(c(tlbPad, tlbPad), "mm")) 
	#
	grobCvAvg <- gridExtra::tableGrob(cvConfAvg, theme=tt)
	grobCvAvg <- gtable::gtable_add_rows(grobCvAvg, heights = grid::grobHeight(titAvgGr) + padding, pos=0)
	grobCvAvg <- gtable::gtable_add_grob(grobCvAvg, list(titAvgGr), t=c(1), l=c(1), r=ncol(grobCvAvg))
	#
	grobCvSD <- gridExtra::tableGrob(cvConfSD, theme=tt)
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
	totN <- colorLegValues$sumPart
	grpCols <- colorLegValues$color_legend # ?$color_unique?;  $txtE
	#
	repeatsAdd <- "" ; capAvgAdd <- "avg. "; equalsCharSub <- equalsCharCap <- "="
	if (haveTest) { 
		equalsCharSub <- equalsCharCap <- tilde
	}
	real_nrCvPred <- round(sum(grpCvPred), rndNoo) # because in the case of bootstrap the incoming number (nrCvPred) is only an approximation, so we average the real numbers that we collected
	nrCvTrain_use <- nrCvTrain
	if (grepl("boot", method)) {
		nrCvTrain_use <- nrCvTrain - real_nrCvPred # in case of boot we give out the real number of observations that the models were built on
		equalsCharSub <- equalsCharCap <- tilde
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
	limits <- ggplot2::aes(ymax = avg + SD, ymin = avg - SD)
	geomBar <- ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge())
	geomErrBar <- ggplot2::geom_errorbar(limits, width=errBarWidth, position = ggplot2::position_dodge(width = 0.9), color=colorErrorbar)
	custColFil <- ggplot2::scale_fill_manual(values=as.character(grpCols)) # strange, but that is what is needed. yes.
	if (!useDataCols) {
		custColFil <- NULL
	}
	#
	cvPlot <- ggplot2::ggplot(cvDF, ggplot2::aes(fill=predicted, y=avg, x=true)) + geomBar + geomErrBar + custColFil + ggplot2::labs(title=cvTitle, subtitle=subCV, caption=comCap, x=xlab, y=ylab)
	testPlot <- testPlotAll <- NULL
	if (!is.null(testDF)) {
		testPlot <- ggplot2::ggplot(testDF, ggplot2::aes(fill=predicted, y=avg, x=true)) + geomBar + geomErrBar + custColFil + ggplot2::labs(title=testTitle, subtitle=subTest, caption=comCap, x=xlab, y=ylab)		
	}
	########################
	# arrange graphics and tables, check for NULL test, and finally plot
	if (addConfTables) {
		cvPlotAll <- gridExtra::arrangeGrob(cvPlot, cvTablesGrob, nrow=1, ncol=2, widths=c(3,1))
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
			plot_classif_typeClassOn(thisType, thisClassOn, classOnCvSummaryList, classOnTestList, thisMethod, masterList$apCl, ap, expName, idString, colorLegValues, apClUser, nrCvTrain, nrCvPred, grpCvPred, nrTestCv, nrTestPred, grpTestPred)
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
	suffix <- paste0("classification")
#	message <- paste0("classification")
	expName <- getExpName(cube)
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting classification... ")}
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	for (i in 1: length(cube)) {
		plot_classif_CubeElement(set=cube[[i]], slotChar, ap, expName, apClUser)
	}		
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF
