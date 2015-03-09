#'  @title Analysis Procedure File
#'	@description The analysis procedure file is used to first split the dataset 
#'  according to the provided values in the first section, and then to tell the 
#'  system which statistics to apply resp. what models to calculate on those 
#'  datasets.
#'  @details The default name for the analysis procedure file can be set in 
#'  settings.r. Any other .r file can be loaded by providing a valid .r filename 
#'  to the appropriate argument, e.g. in the function \code{\link{getap}}
#'  @param splitByVariable NULL or character vector. If NULL, no splitting of the 
#'  dataset will be performed. Provide a character vector with the column names 
#'  of class variables to split the dataset along these variables.
#'  @param splitByWavelength NULL or character vector. If NULL, all in the dataset 
#'  available wavelengths will be used. Provide a character vector in the format 
#'  "wlFrom-to-wlTo" (e.g. c("1000-to-2000", "1300-to-1600", ...)) 
#'  to use all previously defined splits in these wavelengths.
#'  @param do_smooth Logical. If smoothing of the NIR data should be performed. The 
#'  values for the smoothing operation itself are defined in the settings.
#'  @param smooth_useRaw Logical. If, should smoothing be performed, the raw 
#'  unsmoothed data will be used as well in addition to the smoothed data.
#'  @param do_noiseTest Logical. If artifical noise should be added to the dataset.
#'  @param noiseTest_useRaw  If, should the noise-test be performed, the raw data 
#'  will be used as well in addition to the noise-data.
#'  @param do_PCA Logical. If a PCA should be calculated with a given dataset
#'  @param pca_colorByVariable NULL or character vector. Which class-variables 
#'  should be used for coloring the PCA score plot. Set to NULL for using all 
#'  available class variables for coloring.
#'  @param do_SIMCA Logical.If SIMCA models of the dataset should be calculated.
#'  @param simca_variables NULL or character vector. Which variables should be 
#'  used to group the data. Set to NULL for using all available class-variables, 
#'  or provide a character vector with the column names of class variables to group
#'  the data along those for calculating SIMCA models.
#'  @param simca_K Numeric length one. The number of components used for calculating 
#'  the SIMCA models. In mode 'robust' leave at '0' for automatic detection of 
#'  optimal number of components.
#'  @param do_PLSR Logical. If PLSR models should be calculated with a given dataset.
#'  @param plsr_regressOn NULL or character vector. Which variables should be used 
#'  to regress on. Set to NULL for using all numerical variables to regress on, or 
#'  provide a character vector with the column names of numerical variables to use 
#'  those for regression in the PLSR.
#'  @param plsr_ncomp NULL or integer length one. The number of components used in 
#'  PLSR. Set to NULL for automatic detection, or provide an integer to use this 
#'  number of components in the PLSR.
#'  @param plsr_valid Character. Which crossvalidation to use. Possible values are
#'  \itemize{
#'    \item CV for a 10-fold crossvalidation
#'    \item LOO for a leave-one-out crossvalidation
#'    \item XXX something here for the using a certain class-variable to make the 
#'    groupe !!! XXX
#'  }
#'  @param plsr_classForColoring NULL or character. What class-variable should be 
#'  used for coloring in the RMSEP and RMSECV plots. Set to NULL for no coloring, 
#'  or provide a character length one with a single column name of a class 
#'  variable that should be used for coloring.
#'  @param aquagram_variables NULL or character vector. Which class variables 
#'  should be used for grouping the data for the Aquagram. Provide a character 
#'  vector with the column names of class variables for grouping data and generate 
#'  an Aquagram for every one of them, or set to NULL if you are providing the 
#'  Aquagram class-variable later when calculating and plotting the Aquagram.
#'  @seealso \code{\link{getap}}
#'  @family fileDocs
#'  @name anproc_file
NULL
