## pv_classificationFuncs_XDA <- c("lda", "qda", "fda", "mclustda")
#' @param do.da Logical. If used in \code{getap}, if classification via 
#' discriminant analysis (\code{\link[MASS]{lda}}, \code{\link[MASS]{qda}}, 
#' \code{\link[mda]{fda}}, \code{\link[mclust]{MclustDA}}) should be performed 
#' in the given dataset.
#' @param da.type Character vector. The type of discriminant analysis (DA) to 
#' perform; possible values (one or more) are: 
#' \code{<%=r_listize(pv_classificationFuncs_XDA)%>}:
#' \itemize{
#'    \item \code{<%=pv_classificationFuncs_XDA[1]%>} Linear DA using 
#'    \code{\link[MASS]{lda}}.
#'    \item \code{<%=pv_classificationFuncs_XDA[2]%>} Quadratic DA using 
#'    \code{\link[MASS]{qda}}.
#'    \item \code{<%=pv_classificationFuncs_XDA[3]%>} Flexible DA using 
#'    \code{\link[mda]{fda}}.
#'    \item \code{<%=pv_classificationFuncs_XDA[4]%>} DA based on Gaussian finite 
#'    mixture modeling using \code{\link[mclust]{MclustDA}}.
#'  }
#' @param da.classOn Character vector. One or more class variables to define the 
#' grouping used for classification.
#' @param da.testCV Logical, if the errors of the test-data should be crossvalidated. 
#' If set to true, CV and testing is repeated in alternating datasets. See below.
#' @param da.percTest Numeric length one. The percentage of the dataset that should 
#' be set aside for testing the models; these data are never seen during training 
#' and crossvalidation.
#' @param da.cvBootCutoff  If the number of observations within the smallest 
#' subgroup defined by the classification grouping variable is EQUAL or HIGHER then 
#' \code{.cvBootCutoff}, the crossvalidation is done via splitting the training 
#' data in the number of segments defined in \code{.valid} (see below), otherwise 
#' the crossvalidation is done via bootstrap resampling, with the number of 
#' bootstrap iterations resulting from the multiplication by the number of 
#' observations in this smallest subgroup with \code{.cvBootFactor}. Set the latter 
#' to 1 to NEVER perform the CV of the training data via bootstrap, or, for a 
#' global solution, set the parameter \code{cl_gen_neverBootstrapForCV} in the 
#' settings file to \code{TRUE}.
#' @param da.cvBootFactor The factor used to multiply the number of observations 
#' within the smallest subgroup defined by the classification grouping variable 
#' with, resulting in the number of iterations of a possible bootstrap 
#' crossvalidation of the trainign data -- see \code{.cvBootCutoff}.
#' @param da.valid The number of segments the training data should be divided 
#' into in case of a "traditional" crossvalidation of the training data; see above.
