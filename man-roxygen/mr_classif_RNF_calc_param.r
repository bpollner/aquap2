#' @param do.rnf Logical. If used in \code{getap}, if classification via 
#' \code{\link[randomForest]{randomForest}} should be performed in the given 
#' dataset.
#' @param rnf.classOn Character vector. One or more class variables to define the 
#' grouping used for classification.
#' @param rnf.testCV Logical, if the errors of the test-data should be crossvalidated. 
#' If set to true, CV and testing is repeated in alternating datasets. See below.
#' @param rnf.percTest Numeric length one. The percentage of the dataset that should 
#' be set aside for testing the models; these data are never seen during training 
#' and crossvalidation.
#' @param rnf.cvBootCutoff  If the number of observations within the smallest 
#' subgroup defined by the classification grouping variable is EQUAL or HIGHER then 
#' \code{.cvBootCutoff}, the crossvalidation is done via splitting the training 
#' data in the number of segments defined in \code{.valid} (see below), otherwise 
#' the crossvalidation is done via bootstrap resampling, with the number of 
#' bootstrap iterations resulting from the multiplication by the number of 
#' observations in this smallest subgroup with \code{.cvBootFactor}. Set the latter 
#' to 1 to NEVER perform the CV of the training data via bootstrap, or, for a 
#' global solution, set the parameter \code{cl_gen_neverBootstrapForCV} in the 
#' settings file to \code{TRUE}.
#' @param rnf.cvBootFactor The factor used to multiply the number of observations 
#' within the smallest subgroup defined by the classification grouping variable 
#' with, resulting in the number of iterations of a possible bootstrap 
#' crossvalidation of the trainign data -- see \code{.cvBootCutoff}.
#' @param rnf.valid The number of segments the training data should be divided 
#' into in case of a "traditional" crossvalidation of the training data; see above.
#' @param rnf.pcaRed Logical, if variable reduction via PCA should be applied; if 
#' TRUE, the subsequent classifications are performed on the PCA scores, see
#' \code{rnf.pcaNComp} below.
#' @param rnf.pcaNComp Character or integer vector. Provide the character "max" 
#' to use the maximum number of components (i.e. the number of observations minus 
#' 1), or an integer vector specifying the components resp. their scores to be 
#' used for random forest classification.
