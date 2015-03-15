#'  @title Analysis Procedure File
#'	@description The analysis procedure file is used to first split the dataset 
#'  according to the provided values in the 'split dataset' section, and 
#'  then, in the 'statistics' section (starting with \code{do.pca}), to tell 
#'  the system which statistics to apply resp. what models to calculate on those 
#'  datasets. It also contains specific and general plotting options that are used 
#'  by the \code{\link{plot}} function. 
#'  Arguments used to control the split-process, the behaviour of statistics / 
#'  calculations / specific plotting options and the general plotting options 
#'  start with a certain prefix:
#'  \itemize{
#'    \item "spl" for all arguments related to the split-process
#'    \item "pca" for all arguments related to PCA models (except do.pca)
#'    \item "sim" for all arguments related to SIMCA models (except do.sim)
#'    \item "pls" for all arguments related to PLSR models (except do.pls)
#'    \item "aqg" for all arguments related to the calculation of Aquagrams 
#'          (except do.aqg)
#'    \item "pg" for the general plotting options that are used in each of the 
#'    plotting functions.
#'  }
#'  By providing any of the arguments of the analysis procedure file to the 
#'  function \code{\link{getap}}, also when using it inside the function 
#'  \code{\link{gdmm}}, you can override the values in the file with the 
#'  provided values. See examples at \code{\link{gdmm}}.
#'  @details The default name for the analysis procedure file can be set in 
#'  settings.r. Any other .r file can be loaded by providing a valid .r filename 
#'  to the appropriate argument, e.g. in the function \code{\link{getap}}. 
#'  By providing any of the arguments of the analysis procedure file to the 
#'  function \code{\link{getap}} also when using it inside the function 
#'  \code{\link{gdmm}}, you can override the values in the file with the 
#'  provided values. See examples at \code{\link{gdmm}}.
#'  
#'  @param spl.var NULL or character vector. If NULL, no splitting of the 
#'  dataset will be performed. Provide a character vector with the column names 
#'  of class variables to split the dataset along these variables.
#'  @param spl.wl NULL or character vector. If NULL, all in the dataset 
#'  available wavelengths will be used. Provide a character vector in the format 
#'  "wlFrom-to-wlTo" (e.g. c("1000-to-2000", "1300-to-1600", ...)) 
#'  to use all previously defined splits in these wavelengths.
#'  @param spl.do.smo Logical. If smoothing of the NIR data should be performed. The 
#'  values for the smoothing operation itself are defined in the settings.
#'  @param spl.smo.raw Logical. If, should smoothing be performed, the raw 
#'  unsmoothed data will be used as well in addition to the smoothed data.
#'  @param spl.do.noise Logical. If artifical noise should be added to the dataset.
#'  @param spl.noise.raw  If, should the noise-test be performed, the raw data 
#'  will be used as well in addition to the noise-data.
#'  
#'  @template mr_pca_calc_param
#'  @template mr_pca_plot_param
#'  
#'  @param do.sim Logical.If SIMCA models of the dataset should be calculated.
#'  @param sim.vars NULL or character vector. Which variables should be 
#'  used to group the data. Set to NULL for using all available class-variables, 
#'  or provide a character vector with the column names of class variables to group
#'  the data along those for calculating SIMCA models.
#'  @param sim.K Numeric length one. The number of components used for calculating 
#'  the SIMCA models. In mode 'robust' leave at '0' for automatic detection of 
#'  optimal number of components. [It is a capital 'K' in the argument.]
#'  
#'  @param do.pls Logical. If PLSR models should be calculated with a given dataset.
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
#'  @param pls.colorBy NULL or character. What class-variable should be 
#'  used for coloring in the RMSEP and RMSECV plots. Set to NULL for no coloring, 
#'  or provide a character length one with a single column name of a class 
#'  variable that should be used for coloring.
#'  
#'  @template mr_aqg_calc_param
#'  
#'  @seealso \code{\link{getap}}, \code{\link{gdmm}}
#'  @family fileDocs
#'  @name anproc_file
NULL
