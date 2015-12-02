#' @param spl.var NULL or character vector. If NULL, no splitting of the 
#'  dataset will be performed. Provide a character vector with the column names 
#'  of class variables to split the dataset along these variables.
#' @param spl.wl NULL or character vector. If NULL, all in the dataset 
#'  available wavelengths will be used. Provide a character vector in the format 
#'  "wlFrom-to-wlTo" (e.g. c("1000-to-2000", "1300-to-1600", ...)) 
#'  to use all previously defined splits in these wavelengths.
#' @param spl.do.exOut Logical. If exclusion of outliers should be performed.
#' @param spl.exOut.raw Logical. If, should exclusion of outliers be performed, 
#' the raw original data should be used as well. Recommended value = TRUE. 
#' @param spl.exOut.var Character length one. The variable that should be used 
#' for outlier detection.
#' @param spl.do.csAvg Logical. If all the consecutive scans of a single sample 
#' should be reduced, i.e. averaged into a single spectrum.
#' @param spl.csAvg.raw Logical. If, should the consecutive scans of a single 
#' sample be reduced, an other dataset containing every single consecutive scan 
#' should be kept as well as well.
#' @param spl.do.noise Logical. If artifical noise should be added to the dataset.
#' @param spl.noise.raw  If, should the noise-test be performed, the raw data 
#'  will be used as well in addition to the noise-data.
