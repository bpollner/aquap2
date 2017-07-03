#' @param do.nnet Logical. If used in \code{getap}, if classification via 
#' artificial neural networks (\code{\link[nnet]{nnet}}) should be performed 
#' in the  given dataset.
#' @param nnet.clOn Character vector. One or more class variables to define the 
#' grouping used for classification.
#' @param nnet.testCV Logical, if the errors of the test-data should be crossvalidated. 
#' If set to true, CV and testing is repeated in alternating datasets. See below.
#' @param nnet.percTest Numeric length one. The percentage of the dataset that should 
#' be set aside for testing the models; these data are never seen during training 
#' and crossvalidation.
#' @param nnet.cvBootCutoff  If the number of observations within the smallest 
#' subgroup defined by the classification grouping variable is EQUAL or HIGHER then 
#' \code{.cvBootCutoff}, the crossvalidation is done via splitting the training 
#' data in the number of segments defined in \code{.valid} (see below), otherwise 
#' the crossvalidation is done via bootstrap resampling, with the number of 
#' bootstrap iterations resulting from the multiplication by the number of 
#' observations in this smallest subgroup with \code{.cvBootFactor}. Set the latter 
#' to 1 to NEVER perform the CV of the training data via bootstrap, or, for a 
#' global solution, set the parameter \code{cl_gen_neverBootstrapForCV} in the 
#' settings file to \code{TRUE}.
#' @param nnet.cvBootFactor The factor used to multiply the number of observations 
#' within the smallest subgroup defined by the classification grouping variable 
#' with, resulting in the number of iterations of a possible bootstrap 
#' crossvalidation of the trainign data -- see \code{.cvBootCutoff}.
#' @param nnet.valid The number of segments the training data should be divided 
#' into in case of a "traditional" crossvalidation of the training data; see above.
