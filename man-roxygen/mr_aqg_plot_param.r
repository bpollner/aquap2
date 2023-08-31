#' @param do.aqg Logical. If used in a plotting function, if Aquagrams should 
#' be plotted.
#' @param aqg.fsa 'Fix scale for Aquagram'. Logical, numeric or Character. 
#' If left at the default logical FALSE, every single aquagram will be plotted in 
#' its own, independent scale. If a numeric vector length two is provided, all the 
#' aquagrams to be plotted (normal AND bootstrapped ones) will be in the provided 
#' range, no independently scaled aquagrams will be plotted.  If character, the 
#' following values are possible:
#' \itemize{
#'  \item  \code{"both"}: both independently scaled AND automatically calculated 
#'      fix-scaled aquagrams will be plotted
#'  \item \code{"only"}: only the automatically calculated fix-scaled aquagrams 
#'      will be plotted. (normal AND bootstrap)
#' }
#' @param aqg.fss 'Fix scale for subtraction spectra'. Logical, numeric or 
#' character. If left at the default logical FALSE', every single 
#' subtraction-spectra plot will be plotted in its own, independendent 
#' scale. If a numeric vector length two is provided, all the subtraction-spectra 
#' to be plotted (if 'plotSpectra' contains 'subtr', and 'minus' contains a 
#' valid value) will be in the provided range, no independently scaled 
#' subtraction-spectra will be plotted. If character, the following values are 
#' possible: 
#' \itemize{
#'  \item \code{"both"}: both independently scaled AND automatically calculated 
#'      fix-scaled spectra will be plotted 
#'  \item \code{"only"}: only the  automatically 
#' calulated fix-scaled subtraction spectra will be plotted
#' }
#' @param aqg.ccol Custom Color - NULL, Numeric or Character vector. 
#' Custom colors for  drawing the lines in the aquagram. Length must exactly match 
#' the number of groups to be plotted in the aquagram. If not, the default coloring 
#' from the dataset is used. This can be used when plotting aquagrams with 
#' different numbers of groups: only this group that matches the number of provided 
#' custom colors is colored differently. Especially useful when you have more than 
#' 8 lines to be plotted -- custom-color similar groups in similar colors.
#' @param aqg.clt Character or Integer vector. Custom line type for plotting the 
#' lines in the Aquagram. If left at the default 'def', the vector provided in the
#' settings.r file is taken (and recycled). If an integer vector is provided, this 
#' is used (and recycled) as line-types in the Aquagram.
#' @param aqg.pplot Logical or character 'def'. If, should spectra be plotted, an 
#' additional plot with picked peaks should be added. If left at the default value 
#' 'def', the default from the settings.r file is used.
#' @param aqg.plines Logical, numeric or character 'def'. If set to \code{FALSE},
#' no additional lines, if set to \code{TRUE} all the additional lines will be 
#' plotted. If an integer vector [2..5] is provided, one or more of 
#' the additional lines get plotted. See \code{\link{adLinesToVector}} for details.
#' If left at the default value 'def', the default from the settings.r file 
#' (parameter \code{aqg_AdLines}) is used. 
#' @param aqg.discr Logical or character 'def'. If set to TRUE, negative (resp. 
#' positive) peaks can be only found in peak-heights below (resp. above) zero.
