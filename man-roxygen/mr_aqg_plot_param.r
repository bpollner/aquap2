#' @param do.aqg Logical. If used in a plotting function, if Aquagrams should 
#' be plotted.
#' @param fsa Fix scale for the Aquagrams.
#' Logical, numeric or Character. If left at the default logical FALSE, 
#' every single aquagram will be plotted in its own, independent
#' scale. If a numeric vector length two is provided, all the aquagrams to be
#' plotted (normal AND bootstrapped ones) will be in the provided range, 
#' no independently scaled aquagrams will be plotted. 
#' If character, the following values are possible:
#' \code{"both"}: both independently scaled AND automatically calculated 
#' fix-scaled aquagrams will be plotted; \code{"only"}: only the automatically 
#' calculated fix-scaled aquagrams will be plotted. (normal AND bootstrap)
#' @param fss Fix scale for the subtraction spectra.
#' Logical, numeric or character. If left at the default logical FALSE', every 
#' single subtraction-spectra plot will be plotted in its own, independendent 
#' scale. If a numeric vector length two is provided, all the subtraction-spectra 
#' to be plotted (if 'plotSpectra' contains 'subtr', and 'minus' contains a 
#' valid value) will be in the provided range, no independently scaled 
#' subtraction-spectra will be plotted. If character, the following values are 
#' possible: \code{"both"}: both independently scaled AND automatically calculated 
#' fix-scaled spectra will be plotted; \code{"only"}: only the  automatically 
#' calulated fix-scaled subtraction spectra will be plotted.
