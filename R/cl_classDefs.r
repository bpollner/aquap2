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
setClassUnion(name="apNull", members =c("aquap_ap", "NULL"))
setClass("aquap_data", slots=c(metadata="list", anproc="apNull", ncpwl="numeric", version="character"), contains="data.frame")
setClass("aquap_cpt", slots=c(splitVars="list", wlSplit="list", csAvg="logical", noise="logical", exOut="logical", len="numeric"))
setClass("aqg_calc", slots = c(ID="character", classVar="character", itemIndex="numeric", avg="matrix", colRep="numChar", possN="numeric", selInds="numeric", bootRes="matNull", rawSpec="dfNull", avgSpec="dfNull", subtrSpec="dfNull"))
#setClass("aqg_cr", slots = c(res="list", ran="listNull"))
#setClass("aquap_extMod", slots=c(type="character", mod="listNull"))
setClass("aquap_set", slots=c(dataset="aquap_data", idString="character", pca="listNull", plsr="listNull", simca="listNull", aquagr="listNull", extraModels="listNull")) 
setClass("aquap_cube", slots=c(metadata="aquap_md", anproc="aquap_ap", cp="data.frame", cpt="aquap_cpt", aqgRan="listNull"), contains="list")
# setClass("aquap_noiseDist", slots=c(timestamp="POSIXct", version="character"), contains="matrix")

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
			return(new("aquap_data", fd, ncpwl=x@ncpwl, metadata=x@metadata, anproc=x@anproc, version=x@version))
		} ) # end set method

#' @rdname aquap_data-methods
#' @export
setMethod("-", signature(e1="aquap_data", e2="aquap_data"), definition = subtract_two_aquap_data_M)
#' @rdname aquap_data-methods
#' @export
setMethod("/", signature(e1="aquap_data", e2="aquap_data"), definition = divide_two_aquap_data_M)


setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
#' @rdname plot_spectra
#' @export
setMethod("plot", signature(x = "aquap_data", y="missing"), definition = plot_spectra_Data_M)
#' @rdname plot_all_modells
#' @export
setMethod("plot", signature(x = "aquap_cube", y="missing"), definition = plot_cube_M) # strangely, adding any type except "ANY" for y at only one (1) of the two plot methods solved the missing class-documentation problem; the corresponding definition have to have each the same arguments (except the "...") as in the signature.


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


setGeneric("plot_pls", function(object, ...)  standardGeneric("plot_pls"))
#' @rdname plot_pls
#' @export
setMethod("plot_pls", signature(object = "aquap_cube"), definition = plot_pls_cube_M)


setGeneric("plot_simca", function(object, ...)  standardGeneric("plot_simca"))
#' @rdname plot_simca
#' @export
setMethod("plot_simca", signature(object = "aquap_cube"), definition = plot_simca_cube_M)


setGeneric("getNcpwl", function(object) standardGeneric("getNcpwl"))
setMethod("getNcpwl", "aquap_data", function(object) object@ncpwl)

setGeneric("getCP", function(object) standardGeneric("getCP"))
setMethod("getCP", "aquap_cube", function(object) object@cp)

setGeneric("getExpName", function(object) standardGeneric("getExpName"))
setMethod("getExpName", "aquap_md", function(object) object$meta$expName)
setMethod("getExpName", "aquap_data", function(object) object@metadata$meta$expName)
setMethod("getExpName", "aquap_cube", function(object) object@metadata$meta$expName)

setGeneric("getPCAObject", function(object) standardGeneric("getPCAObject"))
setMethod("getPCAObject", "aquap_set", function(object) object@pca$model)

setGeneric("getPCAClassList", function(object) standardGeneric("getPCAClassList"))
setMethod("getPCAClassList", "aquap_ap", function(object) object$pca$colorBy)

setGeneric("getPLSRObjects", function(object) standardGeneric("getPLSRObjects"))
setMethod("getPLSRObjects", "aquap_set", function(object) object@plsr)

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
#' @rdname getWavelengths
#' @export
setMethod("getWavelengths", "aquap_data", definition=getWavelengths_dataset)
#' @rdname getWavelengths
#' @export
setMethod("getWavelengths", "aquap_set", definition=getWavelengths_set)

setGeneric("getNIR", function(object) standardGeneric("getNIR"))
setMethod("getNIR", "aquap_data", definition=getNIR_df_dataset)
setMethod("getNIR", "aquap_set", definition=getNIR_df_set)

setGeneric("getMetadata", function(object) standardGeneric("getMetadata"))
setMethod("getMetadata", "aquap_data", function(object) object@metadata)
setMethod("getMetadata", "aquap_cube", function(object) object@metadata)

setGeneric("getAnproc", function(object) standardGeneric("getAnproc"))
setMethod("getAnproc", "aquap_cube", function(object) object@anproc)
setMethod("getAnproc", "aquap_data", function(object) object@anproc)

## SIMCA
setGeneric("getApSimcaClassList", function(object) standardGeneric("getApSimcaClassList"))
setMethod("getApSimcaClassList", "aquap_ap", function(object) object$simca$simcOn)
setMethod("getApSimcaClassList", "aquap_cube", function(object) object@anproc$simca$simcOn)

setGeneric("getSIMCAModels", function(object) standardGeneric("getSIMCAModels"))
setMethod("getSIMCAModels", "aquap_set", function(object) object@simca$mods)

setGeneric("getSIMCAPredictions", function(object) standardGeneric("getSIMCAPredictions"))
setMethod("getSIMCAPredictions", "aquap_set", function(object) object@simca$preds)

setGeneric("getSIMCAModels_cv", function(object) standardGeneric("getSIMCAModels_cv"))
setMethod("getSIMCAModels_cv", "aquap_set", function(object) object@simca$mods_cv)

setGeneric("getSIMCAPredictions_cv", function(object) standardGeneric("getSIMCAPredictions_cv"))
setMethod("getSIMCAPredictions_cv", "aquap_set", function(object) object@simca$preds_cv)

setGeneric("getSIMCAicDists", function(object) standardGeneric("getSIMCAicDists"))
setMethod("getSIMCAicDists", "aquap_set", function(object) object@simca$icDists)

setGeneric("getCorrectSimcaClasses", function(object) standardGeneric("getCorrectSimcaClasses"))
setMethod("getCorrectSimcaClasses", "aquap_set", function(object) object@simca$groupingVector)


## PLSR
setGeneric("getPlsrModels", function(object) standardGeneric("getPlsrModels"))
setMethod("getPlsrModels", "aquap_set", function(object) object@plsr$model)

setGeneric("getPlsrRegrOnList", function(object) standardGeneric("getPlsrRegrOnList"))
setMethod("getPlsrRegrOnList", "aquap_set", function(object) object@plsr$regrOn)



