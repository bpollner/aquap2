#' @param spl.var NULL or character vector. If NULL, no splitting of the 
#'  dataset will be performed. Provide a character vector with the column names 
#'  of class variables to split the dataset along these variables.
#' @param spl.wl NULL or character vector. If NULL, all in the dataset 
#'  available wavelengths will be used. Provide a character vector in the format 
#'  "wlFrom-to-wlTo" (e.g. c("1000-to-2000", "1300-to-1600", ...)) 
#'  to use all previously defined splits in these wavelengths.
#' @param dpt.pre Character vector, which of the available modules of data 
#' pre-treatments to apply \strong{AFTER} a (possible) split by variable 
#' \code{spl.var} and wavelength \code{spl.w.}, and \strong{BEFORE} a (possible) 
#' splitting of the dataset according to the provided split-variables below 
#' (csAvg, noise, exOut). Leave at NULL for no data pre-treatment. Possible values 
#' are <%=r_listize(pv_dptModules)%>. Add additional parameters to \emph{some} of 
#' the single strings via the separator '@@'. For further information and examples 
#' see \code{\link{dpt_modules}}.
#' @param spl.do.csAvg Logical. If all the consecutive scans of a single sample 
#' should be reduced, i.e. averaged into a single spectrum.
#' @param spl.csAvg.raw Logical. If, should the consecutive scans of a single 
#' sample be reduced, an other dataset containing every single consecutive scan 
#' should be kept as well as well.
#' @param spl.do.noise Logical. If artifical noise should be added to the dataset.
#' @param spl.noise.raw  If, should the noise-test be performed, the raw data 
#'  will be used as well in addition to the noise-data.
#' @param spl.do.exOut Logical. If exclusion of outliers should be performed.
#' @param spl.exOut.raw Logical. If, should exclusion of outliers be performed, 
#' the raw original data should be used as well; if set to TRUE, outliers will 
#' be flagged in the dataset in any case.
#' @param spl.exOut.var Character vector. The variables that should be used 
#' for the grouping defining the scope for outlier detection.
#' @param dpt.post Character vector, which of the available modules of data 
#' pre-treatments to apply \strong{AFTER} (possibly) splitting the dataset. Leave 
#' at NULL for no additional data treatment. Possible values are 
#' <%=r_listize(pv_dptModules)%>. Add additional parameters to some of the single 
#' strings via the separator '@@'. For examples and further information see
#' \code{\link{dpt_modules}}.
