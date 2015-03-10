##############################################################
##################### define classes #########################
##############################################################

.initializeAQ2Classes <- function() {
#	setClassUnion(name="matNull", members = c("matrix", "NULL"))
#	setClassUnion(name="dfNull", members = c("data.frame", "NULL"))
	setClassUnion(name="listNull", members =c("list", "NULL"))
	##	
	setClass("aquap_md", contains="list")
	setClass("aquap_ap", contains="list")
#	setClass("aquap_data", slots=c(header="data.frame", colRep="data.frame", NIR="matrix", ncpwl="numeric"))
	setClass("aquap_data", slots=c(ncpwl="numeric"), contains="data.frame")
	setClass("aquap_cpt", slots=c(splitVars="list", wlSplit="list", smoothing="logical", noise="logical", len="numeric"))
	setClass("aquap_set", slots=c(dataset="aquap_data", idString="character", pca="listNull", plsr="listNull", plsrPlus="listNull", simca="listNull")) 
	setClass("aquap_cube", slots=c(metadata="aquap_md", anproc="aquap_ap", cp="data.frame", cpt="aquap_cpt"), contains="list")
} # EOF


.initializeAQ2Methods <- function() {
	setMethod("show", signature(object = "aquap_data"), definition = show_aquap_data )
	setMethod("show", signature(object = "aquap_cube"), definition = showCube )

	setGeneric("getNcpwl", function(object) standardGeneric("getNcpwl"))
	setMethod("getNcpwl", "aquap_data", function(object) object@ncpwl)

		
} # EOF
str
