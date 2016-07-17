#' @title Data pre-treatment modules
#' @description Apply one or more data pre-treatment modules to the dataset.
#' The data pre-treatment can either happen right after the wavelength-split and
#' before the possible flagging / exclusion of outliers (\code{dpt.pre}), or 
#' after the generation of the datasets via \code{\link{gdmm}} (\code{dpt.post}).
#' @details Via the ... argument in the function \code{\link{getap}}, the values
#' of both \code{dpt.pre} and \code{dpt.post} can be overridden. Possible values
#' are <%=r_listize(pv_dptModules)%>.
