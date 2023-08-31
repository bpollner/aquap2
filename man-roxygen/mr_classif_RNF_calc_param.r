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
#' @param rnf.cvBootCutoff The minimum number of observations (W) that should be 
#' in the smallest subgroup (as defined by the classification grouping variable) 
#' *AFTER* the split into \code{rnf.valid} crossvalidation segments (below). If W 
#' is equal or higher than \code{rnf.cvBootCutoff}, the crossvalidation is done 
#' via splitting the training data in \code{rnf.valid} (see below) segments, 
#' otherwise the crossvalidation is done via bootstrap resampling, with the number 
#' of bootstrap iterations resulting from the multiplication of the number of 
#' observations in this smallest subgroup (as defined by the classification 
#' grouping variable) in *all* of the training data with \code{rnf.cvBootFactor}. 
#' To never perform the CV of the training data via bootstrap, set the parameter 
#' \code{cl_gen_neverBootstrapForCV} in the settings.r file to \code{TRUE}. 
#' An example: With \code{rnf.cvBootCutoff} 
#' set to \code{15} and a 8-fold crossvalidation \code{rnf.valid <- 8}, the 
#' required minimum number of observations in the smallest subgroup *after* the 
#' split in 8 segments would be 15, and in all the training data to perform the 
#' desired 8-fold CV would be (8x15=) 120, in what case then 8 times 15 
#' observations will form the test data to be projected into models made from 
#' (120-15=) 105 observations. If there would be less than 120 observations, lets 
#' say for example, only 100 observations in the smallest group as defined by the 
#' classification grouping variable, bootstrap resampling with 
#' \code{rnf.cvBootFactor * 100} iterations would be performed. In this example, 
#' if we would also be satisfied with a 5-fold crossvalidation, then we would have 
#' enough data: 100 / 5 = 20, and with the \code{rnf.cvBootCutoff} value being 15, 
#' the 5-fold crossvalidation would be performed.
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
