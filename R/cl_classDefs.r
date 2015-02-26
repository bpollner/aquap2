##############################################################
##################### define classes #########################
##############################################################

.initializeAQ2Classes <- function() {
	setClass("aquap_md", contains="list")
	setClass("aquap_data", slots=c(header="data.frame", colRep="data.frame", NIR="matrix", ncpwl="numeric"))
	
} # EOF


.initializeAQM2ethods <- function() {
	setMethod("show", signature(object = "aquap_data"), definition = show_aquap_data )

	
	
} # EOF
str
