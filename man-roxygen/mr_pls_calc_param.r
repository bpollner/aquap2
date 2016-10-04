#' @param do.pls Logical. If used in \code{getap}, if PLSR models should be 
#'  calculated with a given dataset.
#' @param pls.regOn NULL or character vector. Which variables should be used 
#'  to regress on. Set to NULL for using all numerical variables to regress on, or 
#'  provide a character vector with the column names of numerical variables to use 
#'  those for regression in the PLSR.
#' @param pls.ncomp NULL or integer length one. The number of components used in 
#'  PLSR. Set to NULL for automatic detection, or provide an integer to use this 
#'  number of components in the PLSR.
#' @param pls.valid Character. Which crossvalidation to use. Possible values are:
#'  \itemize{
#'    \item "def" Read in the default value from settings.r (parameter 
#'    \code{plsr_calc_typeOfCrossvalid})
#'    \item A numeric length one for this n-fold crossvalidation
#'    \item input in the format like e.g. \code{10@C_Group} for performing a 
#'    10-fold crossvalidation with groups XXX something here for using a certain 
#'    class-variable for grouping XXX
#'    \item "LOO" for a leave-one-out crossvalidation
#'  }
#'  Please note that via the parameter \code{plsr_calc_CV_consecsTogether} in 
#'  the settings file you can select if for crossvalidation the consecutive scans 
#'  (i.e. the scans with the same sample number) should always be excluded or 
#'  included together.
