### pv_what_models <- c("pca", "sim", "pls") # what possible values for arguments when retrieving the models using getCubeModel
#' @details The default for \code{what} is "pca. Provide any of 
#' <%=r_listize(pv_what_models)%> to obtain its corresponding model. Possible 
#' values for \code{what} are:
#' \describe{
#' \item{<%=pv_what_models[1]%>}{Gives the back the pca-model of the selected 
#' dataset from the cube}
#'  \item{<%=pv_what_models[2]%>}{Gives back the simca-model of the selected 
#'  dataset from the cube}
#'  \item{<%=pv_what_models[3]%>}{Gives back the plsr-model of the selected 
#'  dataset from the cube}
#' }
