#' @title Aquaphotomics NIRS Analysis
#' @description Methods and tools to analyse NIRS data from an Aquaphotomics 
#' perspective. The package can support you in preparing (randomizing) your 
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
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @docType package
#' @name aquap2
NULL
