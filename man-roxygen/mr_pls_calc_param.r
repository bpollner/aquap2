#'  @param do.pls Logical. If used in \code{getap}, if PLSR models should be 
#'  calculated with a given dataset.
#'  @param pls.regOn NULL or character vector. Which variables should be used 
#'  to regress on. Set to NULL for using all numerical variables to regress on, or 
#'  provide a character vector with the column names of numerical variables to use 
#'  those for regression in the PLSR.
#'  @param pls.ncomp NULL or integer length one. The number of components used in 
#'  PLSR. Set to NULL for automatic detection, or provide an integer to use this 
#'  number of components in the PLSR.
#'  @param pls.valid Character. Which crossvalidation to use. Possible values are
#'  \itemize{
#'    \item "def" Read in the default value from settings.r
#'    \item "CV" for a 10-fold crossvalidation
#'    \item "LOO" for a leave-one-out crossvalidation
#'    \item XXX something here for using a certain class-variable for grouping XXX
#'  }
