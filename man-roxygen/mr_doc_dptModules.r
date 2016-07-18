## pv_dptModules <- c("sgol", "snv", "msc", "emsc", "osc", "deTr")

#' @title Data pre-treatment modules
#' @description Apply one or more data pre-treatment modules to the dataset.
#' The data pre-treatment can either happen right after the wavelength-split and
#' before the possible flagging / exclusion of outliers (\code{dpt.pre}), or 
#' after the generation of the datasets via \code{\link{gdmm}} (\code{dpt.post}).
#' @details Via the ... argument in the function \code{\link{getap}}, the values
#' of both \code{dpt.pre} and \code{dpt.post} can be overridden. Via the 
#' separator '@@', additional values can be appended to some of the single modules. 
#' Possible values are <%=r_listize(pv_dptModules)%>.
#' @param <%=pv_dptModules[1]%> Transform the dataset using the Savitzy-Golay 
#' filter \code{\link[signal]{sgolay}}. Use a string in the format 
#' "<%=pv_dptModules[1]%>@@p-n-m", with p,n,m being integers to modify the 
#' behaviour of \code{\link{do_sgolay}}, what is called internally.
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
#' "<%=pv_dptModules[3]%>@@x" with "x" being a character length one naming an 
#' existing object in your workspace containing an oject of class 
#' \code{aquap_data} containing only one row, (as e.g. produced by 
#' \code{\link{do_avg}}) to use this as a reference for baseline correction.
#' \describe{
#' \item{x}{Format: "<%=pv_dptModules[3]%>@@x", with 'x' being a character length 
#' one naming an existing object in your workspace containing an oject of class 
#' \code{aquap_data} with only one row. See also \code{\link{do_avg}}.}
#' }
#' @param <%=pv_dptModules[4]%>  Transform the datset using 'emsc' 
#' \code{\link{do_emsc}}. XXX
#' @param <%=pv_dptModules[5]%> XXX
#' @param <%=pv_dptModules[6]%> XXX
#' @section Important:
#' Please see \code{\link{split_dataset}} to understand \strong{when} in the 
#' data-processing procedure the respective data treatment modules are applied!
#' @examples 
#' \dontrun{
#' ## the argument 'dpt.pre' resp. 'dpt.post' in the analysis procedure could look 
#' ## like this:
#' dpt.pre <- c("sgol@2-51-0")
#' dpt.post <- c("sgol@2-51-0", "snv")
#' dpt.post <- "msc"
#' dpt.post <- "msc@myDS" # with 'myDS' being the name of a standard dataset 
#' ## containing 1 row
#' }
#' @seealso \code{\link{split_dataset}}, \code{\link{anproc_file}}
