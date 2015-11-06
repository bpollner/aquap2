#' @title Class 'aquap_cube'
#' @description Holds all the statistical models / calculations that were 
#' performed on the split-variations of the dataset by the function 
#' \code{\link{gdmm}} in a list.
#' @details Each element of the list in aquap_cube is an object of class 
#' \code{\link{aquap_set}}.
#' @slot .Data A list with one object of class \code{\link{aquap_set}} in each 
#' element.
#' @slot metadata An object of class 'aquap_md' (what is list)
#' @slot anproc An object of class 'aquap_ap' (what is a list)
#' @slot cp A data frame with the 'comparison pattern', i.e. a description of the 
#' split-variations of the dataset in a well readable form. This slots gets printed 
#' to the screen when you just type the name of a cube-object. (method 'show')
#' @slot cpt An object of class \code{\link{aquap_cpt}}, what is basically just an 
#' other version of the data in 'cp' for internal use.
#' @seealso \code{\link{gdmm}}
#' @family Class documentations
#' @name aquap_cube
#' NULL



#' @title Class 'aquap_cpt'
#' @description Used to generate datasets.
#' @details The 'cpt' object is in the '@@cpt' slot of the \code{\link{aquap_cube}}
#' @slot len Numeric length one, the number of rows of the comparison pattern.
#' @slot splitVars A list with two elements:
#' \itemize{
#'  \item "classes" A data frame with the column names of the variables used for 
#'  splitting. 
#'  \item "values" A data frame with the values of the columns (variables) used 
#'  for splitting.
#' }
#' @slot wlSplit A list with to numerics (wavelength 'from' and 'to') in each 
#' element.
#' @slot smoothing A logical vector.
#' @slot noise A logical vector.
#' @seealso \code{\link{gdmm}}
#' @family Class documentations
#' @name aquap_cpt
#' NULL



#' @title Class 'aquap_set'
#' @description Holds a dataset and all the statistical models / calculations that 
#' were performed on this specific dataset.
#' @details The 'aquap_set' is in each list-element of the \code{\link{aquap_cube}} 
#' object.
#' @slot dataset The specific dataset used to calculate the models
#' @slot idString A character length one describing the split-parameter of the 
#' dataset.
#' @slot pca The PCA model.
#' @slot plsr The PLSR model(s).
#' @slot simca The SIMCA model(s).
#' @slot aquagr All data related to the calculation of Aquagrams.
#' @seealso \code{\link{gdmm}}
#' @family Class documentations
#' @name aquap_set
#' NULL
