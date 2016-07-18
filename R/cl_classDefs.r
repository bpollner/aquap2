#' @include aquap2.r
#' @include cl_classFunctions.r

# classes ---------------------------------------
setClassUnion(name="matNull", members = c("matrix", "NULL"))
setClassUnion(name="dfNull", members = c("data.frame", "NULL"))
setClassUnion(name="listNull", members =c("list", "NULL"))
setClassUnion(name="numChar", members =c("numeric", "character"))
##	
setClass("aquap_md", contains="list")
setClass("aquap_ap", contains="list")
setClass("aquap_data", slots=c(metadata="list", ncpwl="numeric", version="character"), contains="data.frame")
setClass("aquap_cpt", slots=c(splitVars="list", wlSplit="list", csAvg="logical", noise="logical", exOut="logical", len="numeric"))
setClass("aqg_calc", slots = c(ID="character", classVar="character", itemIndex="numeric", avg="matrix", colRep="numChar", possN="numeric", selInds="numeric", bootRes="matNull", rawSpec="dfNull", avgSpec="dfNull", subtrSpec="dfNull"))
#setClass("aqg_cr", slots = c(res="list", ran="listNull"))
#setClassUnion(name="aqgCrNull", members =c("aqg_cr", "NULL"))
setClass("aquap_set", slots=c(dataset="aquap_data", idString="character", pca="listNull", plsr="listNull", simca="listNull", aquagr="listNull")) 
setClass("aquap_cube", slots=c(metadata="aquap_md", anproc="aquap_ap", cp="data.frame", cpt="aquap_cpt", aqgRan="listNull"), contains="list")


# methods ----------------------------------------
setMethod("show", signature(object = "aquap_data"), definition = show_aquap_data )
setMethod("show", signature(object = "aquap_cube"), definition = showCube )


#' @rdname aquap_data-methods
#' @export
setMethod("[", signature(x = "aquap_data"), definition = function(x, i) {
			drop=FALSE
			header <- x$header[i, , drop=drop]
			colRep <- x$colRep[i, , drop=drop]
			NIR <- x$NIR[i, , drop=drop]
			rownames(NIR) <- rownames(x$NIR)[i]
			colnames(NIR) <- colnames(x$NIR)
			fd <- reFactor(data.frame(I(header), I(colRep), I(NIR)))
			return(new("aquap_data", fd, ncpwl=x@ncpwl, metadata=x@metadata))
		} ) # end set method

#' @rdname aquap_data-methods
#' @export
setMethod("-", signature(e1="aquap_data", e2="aquap_data"), definition = subtract_two_aquap_data_M)


setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
#' @rdname plot_spectra
#' @export
setMethod("plot", signature(x = "aquap_data"), definition = plot_spectra_Data_M)
#' @rdname plot_all_modells
#' @export
setMethod("plot", signature(x = "aquap_cube", y="raw"), definition = plot_cube_M) # strangely, adding the "raw" at only one (1) of the two plot methods solved the missing class-documentation problem; the corresponding definition have to have each the same arguments (except the "...") as in the signature.


setGeneric("plot_spectra", function(x, colorBy=NULL, ...) standardGeneric("plot_spectra"))
#' @rdname plot_spectra
#' @export
setMethod("plot_spectra", signature(x = "aquap_data"), definition = plot_spectra_Data_M)
#' @rdname plot_spectra
#' @export
setMethod("plot_spectra", signature(x = "aquap_cube"), definition = plot_spectra_Cube_M)


setGeneric("plot_pca", function(object, ...)  standardGeneric("plot_pca"))
#' @rdname plot_pca
#' @export
setMethod("plot_pca", signature(object = "aquap_cube"), definition = plot_pca_cube_M)
#' @rdname plot_pca
#' @export
setMethod("plot_pca", signature(object = "aquap_data"), definition = plot_pca_data_M)


setGeneric("getNcpwl", function(object) standardGeneric("getNcpwl"))
setMethod("getNcpwl", "aquap_data", function(object) object@ncpwl)

setGeneric("getExpName", function(object) standardGeneric("getExpName"))
setMethod("getExpName", "aquap_md", function(object) object$meta$expName)
setMethod("getExpName", "aquap_cube", function(object) object@metadata$meta$expName)

setGeneric("getPCAObject", function(object) standardGeneric("getPCAObject"))
setMethod("getPCAObject", "aquap_set", function(object) object@pca$model)

setGeneric("getPCAClassList", function(object) standardGeneric("getPCAClassList"))
setMethod("getPCAClassList", "aquap_ap", function(object) object$pca$colorBy)

setGeneric("getColRep", function(object) standardGeneric("getColRep"))
setMethod("getColRep", "aquap_data", definition=getColRep_data)
setMethod("getColRep", "aquap_set",  definition=getColRep_set)

#' @rdname Extract_Elements
setGeneric("getHeader", function(object) standardGeneric("getHeader"))
#' @rdname Extract_Elements
#' @export
setMethod("getHeader", "aquap_data", definition=getHeader_dataset)
#' @rdname Extract_Elements
#' @export
setMethod("getHeader", "aquap_set", definition=getHeader_set)

setGeneric("getIdString", function(object) standardGeneric("getIdString"))
setMethod("getIdString", "aquap_set", function(object) object@idString)
setMethod("getIdString", "aqg_calc", function(object) object@ID )

setGeneric("getDataset", function(object) standardGeneric("getDataset"))
setMethod("getDataset", "aquap_set", function(object) object@dataset)

setGeneric("getAqgResList", function(object) standardGeneric("getAqgResList"))
setMethod("getAqgResList", "aquap_set", function(object) object@aquagr)

setGeneric("getClassVar", function(object) standardGeneric("getClassVar"))
setMethod("getClassVar", "aqg_calc", function(object) object@classVar)

setGeneric("getItemIndex", function(object) standardGeneric("getItemIndex"))
setMethod("getItemIndex", "aqg_calc", function(object) object@itemIndex)

setGeneric("getWavelengths", function(object) standardGeneric("getWavelengths"))
setMethod("getWavelengths", "aquap_data", definition=getWavelengths_dataset)
setMethod("getWavelengths", "aquap_set", definition=getWavelengths_set)

setGeneric("getNIR", function(object) standardGeneric("getNIR"))
setMethod("getNIR", "aquap_data", definition=getNIR_df_dataset)
setMethod("getNIR", "aquap_set", definition=getNIR_df_set)

setGeneric("getMdDs", function(object) standardGeneric("getMdDs"))
setMethod("getMdDs", "aquap_data", function(object) object@metadata)


## SIMCA
setGeneric("getSIMCAClassList", function(object) standardGeneric("getSIMCAClassList"))
setMethod("getSIMCAClassList", "aquap_ap", function(object) object$simca$simcOn)


