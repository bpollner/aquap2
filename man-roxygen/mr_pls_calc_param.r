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
#'    \item A numeric length one for this n-fold crossvalidation. The default is 
#'    to always exclude resp. include consecutive scans together.
#'    \item A valid name of a class variable for performing a crossvalidation 
#'    based on the grouping defined by this variable. For a class variable 
#'    containing e.g. four different levels, a 4-fold crossvalidation with always 
#'    all members of one group being excluded is performed.
#'    This is overruling any grouping that would result from the consecutive 
#'    scans, please see below.
#'    \item "LOO" for a leave-one-out crossvalidation
#'  }
#'  Please note that via the parameter \code{plsr_calc_CV_consecsTogether} in 
#'  the settings file you can select if for crossvalidation the 
#'  \strong{consecutive scans} (i.e. the scans with the same sample number) should 
#'  always be excluded or included together. The default is to always exclude resp. 
#'  include the consecutive scans of a single sample together.
