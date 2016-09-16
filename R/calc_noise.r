noise_getHullValues <- function(dataset) {
	hullV <- apply(dataset$NIR, 2, range) # gives back a matrix with 2 rows and ncol(NIR) columns with the lower values in the first row
} # EOF
