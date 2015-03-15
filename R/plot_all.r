#' @rdname plot
#' @param what Character length one. What type of analysis to plot. Possible 
#' values are XXX.
#' @export
plot_cube <- function(x, what="all", ...) {
	autoUpS()
	pv <- pv_what_subPlots 			# c("all", "pca", "sim", "pls", "aqg")
	if (!all(what %in% pv)) {
		stop(paste("Please provide one or more of \n'", paste(pv, collapse="', '"), "' \nto the argument 'what'.", sep=""), call.=FALSE)
	}
	if (any(c(pv[1], pv[2]) %in% what)) { # PCA
		plot_pca(x, ...)
	}
	if (any(c(pv[1], pv[3]) %in% what)) { # SIMCA
	
	}
	if (any(c(pv[1], pv[4]) %in% what)) { # PLSR
	
	}
	if (any(c(pv[1], pv[5]) %in% what)) { # Aquagram
	
	}
	invisible(NULL)
} # EOIF


