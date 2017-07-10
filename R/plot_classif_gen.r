xxxxDemo <- function() {
#	List of 5
#	 $ cvBranch   :List of 1
#	 $ testBranch :List of 1
#	 $ realClassOn: logi NA
#	 $ apCl       :List of 9
#	 $ id         : chr "DA__all @1300-to-1600"
####
# List of 5
#  $ cvBranch   :List of 1
#   ..$ :List of 3
#   .. ..$ cvSingle :List of 2
#   .. .. ..$ :List of 3
#   .. .. .. ..$ :List of 5
#   .. .. .. .. ..$ mods     :List of 70
#   .. .. .. .. ..$ dfPreds  :List of 70
#   .. .. .. .. ..$ dfTest   :List of 70
#   .. .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ corrClass:List of 2
#   .. .. .. ..$ :List of 5
#   .. .. .. .. ..$ mods     :List of 65
#   .. .. .. .. ..$ dfPreds  :List of 65
#   .. .. .. .. ..$ dfTest   :List of 65
#   .. .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ corrClass:List of 2
#   .. .. .. ..$ :List of 5
#   .. .. .. .. ..$ mods     :List of 65
#   .. .. .. .. ..$ dfPreds  :List of 65
#   .. .. .. .. ..$ dfTest   :List of 65
#   .. .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ corrClass:List of 2
#   .. .. ..$ :List of 3
#   .. .. .. ..$ :List of 5
#   .. .. .. .. ..$ mods     :List of 70
#   .. .. .. .. ..$ dfPreds  :List of 70
#   .. .. .. .. ..$ dfTest   :List of 70
#   .. .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ corrClass:List of 2
#   .. .. .. ..$ :List of 5
#   .. .. .. .. ..$ mods     :List of 65
#   .. .. .. .. ..$ dfPreds  :List of 65
#   .. .. .. .. ..$ dfTest   :List of 65
#   .. .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ corrClass:List of 2
#   .. .. .. ..$ :List of 5
#   .. .. .. .. ..$ mods     :List of 65
#   .. .. .. .. ..$ dfPreds  :List of 65
#   .. .. .. .. ..$ dfTest   :List of 65
#   .. .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ corrClass:List of 2
#   .. ..$ cvSummary:List of 2
#   .. .. ..$ :List of 2
#   .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ avg: table [1:4, 1:4] 32.2 34.1 33.6 0 30.8 32.1 37 0 31.6 34.4 ...
#   .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. .. .. ..$ SD : table [1:4, 1:4] 15.2 15.2 16.7 0 16.8 15.8 16 0 15.8 17 ...
#   .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. .. ..$ corrClass:List of 2
#   .. .. .. .. ..$ avg: num 49.4
#   .. .. .. .. ..$ SD : num 7.02
#   .. .. ..$ :List of 2
#   .. .. .. ..$ errors   :List of 2
#   .. .. .. .. ..$ avg: table [1:2, 1:2] 100 0 0 100
#   .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. .. .. ..$ SD : table [1:2, 1:2] 0 0 0 0
#   .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. .. ..$ corrClass:List of 2
#   .. .. .. .. ..$ avg: num 100
#   .. .. .. .. ..$ SD : num 0
#   .. ..$ method   :List of 2
#   .. .. ..$ : chr "boot"
#   .. .. ..$ : chr "boot"
#  $ testBranch :List of 1
#   ..$ :List of 2
#   .. ..$ :List of 2
#   .. .. ..$ testErrors   :List of 2
#   .. .. .. ..$ avg: table [1:4, 1:4] 35.9 32.4 31.8 0 28 18.6 53.5 0 29 24.9 ...
#   .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. .. ..$ SD : table [1:4, 1:4] 11.7 18.4 7.2 0 5.8 10 11.3 0 8.6 5.9 ...
#   .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ testCorrClass:List of 2
#   .. .. .. ..$ avg: num 50.1
#   .. .. .. ..$ SD : num 2.75
#   .. ..$ :List of 2
#   .. .. ..$ testErrors   :List of 2
#   .. .. .. ..$ avg: table [1:2, 1:2] 100 0 0 100
#   .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. .. ..$ SD : table [1:2, 1:2] 0 0 0 0
#   .. .. .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. ..$ testCorrClass:List of 2
#   .. .. .. ..$ avg: num 100
#   .. .. .. ..$ SD : num 0
#  $ realClassOn: logi NA
#  $ apCl       :List of 9
#   ..$ type      : chr "fda"
#   ..$ classOn   : chr [1:2] "C_Group" "C_waterNames"
#   ..$ testCV    : logi TRUE
#   ..$ percTest  : num 30
#   ..$ bootCutoff: num 15
#   ..$ bootFactor: num 1
#   ..$ valid     : num 8
#   ..$ pcaRed    : logi TRUE
#   ..$ pcaNComp  : int [1:12] 1 2 3 4 5 6 7 8 9 10 ...
#  $ id         : chr "DA__all @1300-to-1600"
} # EOF

getTruePercTest <- function(percTest) {
	nrOuterLoops <- round(100/percTest, 0)
	out <- round(100/nrOuterLoops, 1)
	return(out)
} # EOF

makeClassifDataFrameForBarplot <- function(avgTable, sdTable) {
	# both incoming tables (have to) have the same names and dimensions
	avgNum <- as.numeric(avgTable)
	sdNum <- as.numeric(sdTable)
	namesList <- attr(avgTable, "dimnames")
	namesTrue <- rep(namesList$true, each=nrow(avgTable))
	namesPred <- rep(namesList$predicted, nrow(avgTable))
	out <- data.frame(true=namesTrue, predicted=namesPred, avgCorrClass=avgNum, SD=sdNum)
	return(out)	
} # EOF

plot_classif_typeClassOn <- function(type, classOn, cvSummaryList, testSummaryList, method, apCl, ap, expName, onMain, onSub) { # we are already in the PDF or not
#	cat(paste0("Type: ", type, "; classOn: ", classOn, " (", method, ")\n"))
#	cat("CV Summary \n")
#	print(cvSummaryList)
#	cat("Test Summary \n")
#	print(testSummaryList)
	trueTestPerc <- getTruePercTest(apCl$percTest)
	pcaRedChar <- ""
	if (apCl$pcaRed) {
		pcaRedChar <- "pcaRed"
	}
	#
	cvConfAvg <- cvSummaryList$errors$avg
	cvConfSD <- cvSummaryList$errors$SD
	cvCorrClassAvg <- cvSummaryList$corrClass$avg
	cvCorrClassSD <- cvSummaryList$corrClass$SD
	#
	testConfAvg <- testSummaryList$testErrors$avg
	testConfSD <- testSummaryList$testErrors$SD
	testCorrClassAvg <- testSummaryList$testCorrClass$avg
	testCorrClassSD <- testSummaryList$testCorrClass$SD
	
	cvDF <- makeClassifDataFrameForBarplot(cvConfAvg, cvConfSD)
	testDF <- makeClassifDataFrameForBarplot(testConfAvg, testConfSD)
	
#	par(mfrow=c(2,1))
	cvPlot <- ggplot(cvDF, aes(fill=predicted, y=avgCorrClass, x=true)) + geom_bar(stat="identity", position="dodge") + ggtitle(paste0("Crossvalidation")) + xlab("true class") + ylab("average predicted class")
	testPlot <- ggplot(testDF, aes(fill=predicted, y=avgCorrClass, x=true)) + geom_bar(stat="identity", position="dodge") + ggtitle(paste0("Test")) + xlab("true class") + ylab("average predicted class") 
	plot(cvPlot)
	plot(testPlot)
#	par(mfrow=c(1,1))
#	print(cvDF); print(testDF)
	
	
} # EOF




plot_classif_typeClassOn_setup <- function(type, classOn, cvSummaryList, testSummaryList, method, apCl, ap, expName) {
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- paste0(type, "_classif")
	message <- paste0(type, "classification")
	filename <- paste(expName, suffix, sep="__")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	plot_classif_typeClassOn(type, classOn, cvSummaryList, testSummaryList, method, apCl, ap, expName, onMain, onSub)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

plot_classif_CubeElement <- function(masterList, ap, expName) { # here we are cycling through the type and the classOn value
	# we want, for each type, and there for each classOn variable, the CV summary and the test summary
	typeList <- vector("list", length(masterList$apCl$type))
	classOnList <- vector("list", length(masterList$apCl$classOn))
	for (i in 1: length(typeList)) { # going through the single types (mainly for DA)
		thisType <- masterList$apCl$type[i]
		siTypeCvSingleList <- masterList$cvBranch[[i]]$cvSingle
		siTypeCvSummaryList <- masterList$cvBranch[[i]]$cvSummary
		siTypeMethodList <- masterList$cvBranch[[i]]$method
		siTypeTestList <- masterList$testBranch[[i]]
		for (k in 1: length(classOnList)) { # going through the single classOn values
			thisClassOn <- masterList$apCl$classOn[k]
			classOnCvList <- siTypeCvSingleList[[k]]
			classOnCvSummaryList <- siTypeCvSummaryList[[k]]
			thisMethod <- siTypeMethodList[[k]]
			classOnTestList <- siTypeTestList[[k]]
			plot_classif_typeClassOn_setup(thisType, thisClassOn, classOnCvSummaryList, classOnTestList, thisMethod, masterList$apCl, ap, expName)
		} # end for k (classOn)
	} # end for i (type)
} # EOF

plot_classif_generalHandover <- function(cube, ap, slotChar) { # cycling through the cube elements; the slotChar comes in from the specific function in file "plot_classif_spec.r"
	expName <- getExpName(cube)
	for (i in 1: length(cube)) {
		plot_classif_CubeElement(masterList=slot(cube[[i]], slotChar), ap, expName)
	}		
} # EOF
