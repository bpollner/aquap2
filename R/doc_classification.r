# XDA ---------------

#' @title Plot Discriminant Analysis - Arguments
#' @description The following parameters can be used in the \code{...} argument 
#' e.g. in function \code{\link{plot}} and \code{\link{plot_da}} to override 
#' the values in the analysis procedure file and so to modify the graphics - 
#' see examples.
#' \describe{
#' \item{\code{plot(cube, ...)}}{ }
#' \item{ \code{plot_da(cube, ...)}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_XDA_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_da}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Plot arguments
#' @family DA documentation
#' @name plot_discrimAnalysis_args
NULL


#' @title Calculate Discriminant Analysis - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of discriminant analysis models - see examples.
#' \describe{
#' \item{\code{getap(...)}}{ }
#' \item{\code{gdmm(dataset, ap=getap(...))}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_XDA_calc_param
#' @seealso \code{\link{gdmm}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Calc. arguments
#' @family Classification functions
#' @family DA documentation
#' @name calc_discrimAnalysis_args
NULL


# RNF ---------------
#' @title Plot Random Forest - Arguments
#' @description The following parameters can be used in the \code{...} argument 
#' e.g. in function \code{\link{plot}} and \code{\link{plot_rnf}} to override 
#' the values in the analysis procedure file and so to modify the graphics - 
#' see examples.
#' \describe{
#' \item{\code{plot(cube, ...)}}{ }
#' \item{ \code{plot_rnf(cube, ...)}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_RNF_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_rnf}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Plot arguments
#' @family RNF documentation
#' @name plot_randomForest_args
NULL


#' @title Calculate Random Forest - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of random forest models - see examples.
#' \describe{
#' \item{\code{getap(...)}}{ }
#' \item{\code{gdmm(dataset, ap=getap(...))}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_RNF_calc_param
#' @seealso \code{\link{gdmm}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Calc. arguments
#' @family Classification functions
#' @family RNF documentation
#' @name calc_randomForest_args
NULL


# SVM ---------------
#' @title Plot SVM - Arguments
#' @description The following parameters can be used in the \code{...} argument 
#' e.g. in function \code{\link{plot}} and \code{\link{plot_svm}} to override 
#' the values in the analysis procedure file and so to modify the graphics - 
#' see examples.
#' \describe{
#' \item{\code{plot(cube, ...)}}{ }
#' \item{ \code{plot_svm(cube, ...)}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_SVM_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_svm}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Plot arguments
#' @family SVM documentation
#' @name plot_SVM_args
NULL


#' @title Calculate SVM - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of SVM models - see examples.
#' \describe{
#' \item{\code{getap(...)}}{ }
#' \item{\code{gdmm(dataset, ap=getap(...))}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_SVM_calc_param
#' @seealso \code{\link{gdmm}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Calc. arguments
#' @family Classification functions
#' @family SVM documentation
#' @name calc_SVM_args
NULL

# NNET ---------------
#' @title Plot NNET - Arguments
#' @description The following parameters can be used in the \code{...} argument 
#' e.g. in function \code{\link{plot}} and \code{\link{plot_nnet}} to override 
#' the values in the analysis procedure file and so to modify the graphics - 
#' see examples.
#' \describe{
#' \item{\code{plot(cube, ...)}}{ }
#' \item{ \code{plot_nnet(cube, ...)}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_NNET_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_nnet}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Plot arguments
#' @family NNET documentation
#' @name plot_NNET_args
NULL


#' @title Calculate NNET - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of articial neural network (NNET/ANN) models - see examples.
#' \describe{
#' \item{\code{getap(...)}}{ }
#' \item{\code{gdmm(dataset, ap=getap(...))}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_classif_NNET_calc_param
#' @seealso \code{\link{gdmm}}, \code{\link{siWlg}} for reducing the number of 
#' wavelengths in a dataset
#' @examples
#' \dontrun{
#' }
#' @family Calc. arguments
#' @family Classification functions
#' @family NNET documentation
#' @name calc_NNET_args
NULL



#' @title Classification - General Considerations
#' @description General remarks regarding the use of the classification functions 
#' in package aquap2.
#` @details 
#' \itemize{
#' \item Except the svm classification, most methods do not work well in fat data 
#' matrices, so if there are more variables (wavelengths) than observations.
#' For an easy reduction of the number of wavelengths we provide, especially in 
#' the Aquaphotomics context, a special function to reduce the number of 
#' wavelengths in a dataset (as produced by \code{\link{gfd}}): function 
#' \code{\link{siWlg}} can isolate any custom group of wavelengths from the dataset, 
#' or, if left at its default, is isolating the ranges of the 12 water matrix 
#' coordinates within the 1st overtone. For further data reduction, this groups 
#' can be averaged within as well by setting the second argument in 
#' \code{\link{siWlg}} to \code{TRUE}.
#' \item For reducing the number of variables not in the raw data, but on the 
#' classification side of the algorithm, it is possible to apply all classifiers 
#' not on the rawdata, but on the PCA scores of the rawdata. This option can be 
#' activated by setting the respective argument \code{.pcaRed} in the analysis 
#' procedure to \code{TRUE}. In this case the prediction data for crossvalidation 
#' as well as the independent test data are projected into the pca-models of the 
#' training data, and the resulting scores are then used for classification.
#' \item For both traditional crossvalidation and the bootstrap "crossvalidation", 
#' consecutive scans of the same sample are always excluded resp. included 
#' together.
#' \item A traditional crossvalidation (no bootstrap) is indicated via a "."
#' after the name of the classificaiton-variable, while the bootstrap
#' process is indicated via a "`".
#' }
#' @family Classification Helpers
#' @name classification_helpers
NULL

