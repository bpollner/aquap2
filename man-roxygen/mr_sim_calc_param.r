#' @param do.sim Logical. If used in \code{getap}, if SIMCA models of the dataset 
#'  should be calculated.
#' @param sim.vars NULL or character vector. Which variables should be 
#'  used to group the data. Set to NULL for using all available class-variables, 
#'  or provide a character vector with the column names of class variables to group
#'  the data along those for calculating SIMCA models.
#' @param sim.K Numeric length one. The number of components used for calculating 
#'  the SIMCA models. In mode 'robust' leave at '0' for automatic detection of 
#'  optimal number of components. [It is a capital 'K' in the argument.]
