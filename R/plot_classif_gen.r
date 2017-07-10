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

plot_classif_typeClassOn <- function(type, classOn, cvSummaryList, testSummaryList, method, apCl) {
	cat(paste0("Type: ", type, "; classOn: ", classOn, " (", method, ")\n"))
	cat("CV Summary \n")
	print(cvSummaryList)
	cat("Test Summary \n")
	print(testSummaryList)
	trueTestPerc <- getTruePercTest(apCl$percTest)
	pcaRedChar <- ""
	if (apCl$pcaRed) {
		pcaRedChar <- "pcaRed"
	}
	
	
	
	
	
	
	
} # EOF


plot_classif_generalHandover <- function(masterList) { # in the specific function above (in file "plot_classif_spec.r") we are cycling through the cube elements!!; here we are cycling through the type and the classOn value
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
			plot_classif_typeClassOn(thisType, thisClassOn, classOnCvSummaryList, classOnTestList, thisMethod, masterList$apCl)
		} # end for k (classOn)
	} # end for i (type)
} # EOF

