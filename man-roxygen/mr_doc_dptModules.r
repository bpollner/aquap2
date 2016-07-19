## pv_dptModules <- c("sgol", "snv", "msc", "emsc", "osc", "deTr")

#' @title Data pre-treatment modules
#' @description Apply one or more data pre-treatment modules to the dataset.
#' The data pre-treatment (\code{dpt.pre}) can happen right \strong{after} a 
#' (possible) wavelength-split and \strong{before} a (possible) splitting of the 
#' dataset according to the provided split-variables below \code{dpt.pre}, being 
#' csAvg, noise and exOut, and / or \strong{after} the splitting of the dataset 
#' (\code{dpt.post}), in other words as a final treatment to a dataset in each 
#' 'row' of the 'cube', i.e. in each 'cube-element'.
#' @details Via the ... argument in the function \code{\link{getap}}, the values
#' of both \code{dpt.pre} and \code{dpt.post} can be overridden. Via the 
#' separator '@@', additional values can be appended to some of the single modules. 
#' Possible values for dpt-modules are <%=r_listize(pv_dptModules)%>. Single 
#' modules can be combined and repeated in any arbitrary order, i.e. there is no 
#' upper limit on the modules that can be applied in the 'dpt.pre' and 'dpt.post' 
#' process -- see examples.
#' @param <%=pv_dptModules[1]%> Transform the dataset using the Savitzy-Golay 
#' filter by calling \code{\link{do_sgolay}} (what in turn is relying on 
#' \code{\link[signal]{sgolay}}). Provide only the character 
#' "<%=pv_dptModules[1]%>" to use the standard values for p, n and m, what are 
#' 2, 21 and 0, respectively. Use a string in the format 
#' "<%=pv_dptModules[1]%>@@p-n-m", with p,n,m being integers to modify the 
#' behaviour of \code{\link{do_sgolay}} by supplying your own values. Please note 
#' that 'n' has to be odd. The single integers have to be separated by a 'minus' 
#' ('-').
#' \describe{
#' \item{p}{The filter order. Defaults to 2.}
#' \item{n}{The filter length, must be odd. Defaults to 21.}
#' \item{m}{Return the m-th derivative of the filter coefficients. Defaults to 0.}
#' }
#' @param <%=pv_dptModules[2]%> Transform the dataset using standard normal 
#' variation 'snv' by calling internally the function \code{\link{do_snv}}. No 
#' additional arguments can be provided here.
#' @param <%=pv_dptModules[3]%> Transform the datset using multiplicative scatter 
#' correction 'msc' \code{\link{do_msc}}. If no reference is provided, the average 
#' of all spectra of a dataset is used as a reference for the baseline correction. 
#' Provide a dataset with a single spectrum in the format 
#' "<%=pv_dptModules[3]%>@@yourObj" with "yourObj" naming an existing object in 
#' your workspace containing an oject of class \code{aquap_data} containing only 
#' one row, (as e.g. produced by \code{\link{do_avg}}) to use this as a reference 
#' for baseline correction.
#' \describe{
#' \item{@@yourObj}{Format: "<%=pv_dptModules[3]%>@@yourObj", with 'yourObj' naming an 
#' existing object in your workspace containing an oject of class 
#' \code{aquap_data} with only one row. See also \code{\link{getcd}} for 
#' extracting a singel dataset from the 'cube' object, and \code{\link{do_avg}} 
#' for averaging a single dataset into a single spectrum.}
#' }
#' @param <%=pv_dptModules[4]%>  Transform the datset using 'emsc'; internally the 
#' function \code{\link{do_emsc}} is called. You have to provide the name of an 
#' object containing a data frame or matrix with one or two loadings or with one 
#' regression vector in the format 
#' \describe{
#' \item{@@yourObj}{Format: "<%=pv_dptModules[4]%>@@yourObj", with 'yourObj' naming an 
#' existing object in your workspace containing  a data frame or matrix with one 
#' or two loadings or with one regression vector. See also \code{\link{getcm}} for 
#' extracting single models from the 'cube' object.}
#' }
#' @param <%=pv_dptModules[5]%> Not yet implemented.
#' @param <%=pv_dptModules[6]%> Not yet implemented.
#' @section Important:
#' Please see the description and \code{\link{split_dataset}} to understand 
#' \strong{when} in the data-processing procedure the respective data treatment 
#' modules are applied!
#' @examples 
#' \dontrun{
#' fd <- gfd() # load a dataset
#' cube <- gdmm(fd) # you should split in at least 2 or 3 groups
#' cube # look at the structure - each row of the cube is treated separately using 
#' # the modules specified in 'dpt.pre' and 'dpt.post' -- see description
#' ####
#' ## the argument 'dpt.pre' resp. 'dpt.post' in the analysis procedure could look 
#' ## like this:
#' dpt.pre <- c("sgol@2-51-0")
#' dpt.post <- c("sgol@2-51-0", "snv")
#' dpt.pre <- c("msc", "snv", "msc") # (of course not useful, but it shows that 
#' ## modules can be combined and repeated in any arbitrary order.)
#' dpt.post <- "msc@myDS" # with 'myDS' being the name of a standard dataset 
#' dpt.post <- c("sgol@2-51-0", "emsc@myDF") # with 'myDF' being the name of a 
#' ## data frame containing one or two loading vectors or one regression vector
#' }
#' @seealso \code{\link{split_dataset}}, \code{\link{anproc_file}}, 
#' \code{\link{getcd}} for extracting a dataset from a 'cube' object, 
#' \code{\link{do_avg}} for averaging datasets into a single spectrum, 
#' \code{\link{getcm}} for extracting a specific model from a 'cube' object.
