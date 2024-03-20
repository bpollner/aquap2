#' @title Multivariate Data Analysis Tools for R including Aquaphotomics Methods
#' @description Methods and tools for multivariate data analysis in R, with an 
#' additional focus on specialized Aquaphotomics procedures and methods. 
#' The package can support you in preparing (randomizing) your 
#' experiment, and it will assist you in splitting up your data and analyzing 
#' it using standard multivariate analysis methods and, especially, the Aquagram.
#' @details For an introduction and more detailed information please see the package 
#' vignettes: \code{browseVignettes(package = "aquap2")}
#' @author Bernhard Pollner, Zoltan Kovacs
#' @section Maintainer: Bernhard Pollner <bernhard.pollner@@mac.com>
#' @section Important Functions: \code{\link{esl}}, \code{\link{gfd}} 
#' \code{\link{gdmm}}, \code{\link{plot_cube}}
#' @examples
#' \dontrun{
#'  dataset <- gfd() # will load or import data
#' cube <- gdmm(dataset) # split up the dataset and make models
#' }
#'
#' @import methods
#' @import foreach
#' @import doParallel
#' @importFrom pls MSEP
#' @importFrom pls mvrValstats
# the 'importFrom' seems to be necessary due to a bug (?) in the pls package, 
# so that these two functions can not be found 
#
# mda for: fda    ## otherwise the independent predictions (calc_classif_gen.r 865 crash. (I think they should not.))
# MASS for: lda, qda
# mclust for:  mclustda
# e1071 for: svm
#' @importFrom mda fda
#' @import MASS
#			 #' @importFrom MASS lda, qda
#' @importFrom mclust MclustDA
# 			#' @import mclust
#' @importFrom stringr str_count
#' @import e1071
#' @import nnet
#' @import randomForest
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @name aquap2
NULL
