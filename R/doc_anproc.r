#'@title Analysis Procedure File
#'@description The analysis procedure file is used to first split the dataset 
#'  according to the provided values in the 'split dataset' section, and 
#'  then, in the 'statistics' section (starting with \code{do.pca}), to tell 
#'  the system which statistics to apply resp. what models to calculate on those 
#'  datasets. It also contains specific and general plotting options that are used 
#'  by the \code{\link{plot}} function. 
#'  Arguments used to control the split-process, the behaviour of statistics / 
#'  calculations / specific plotting options and the general plotting options 
#'  start with a certain prefix:
#'  \itemize{
#'    \item "spl" for all arguments related to the split-process 
#'         (For a separate listing please see  \code{\link{split_dataset}})
#'    \item "pca" for all arguments related to PCA models (except do.pca)
#'        (For a separate listing see \code{\link{calc_pca_args}} and 
#'        \code{\link{plot_pca_args}})
#'    \item "sim" for all arguments related to SIMCA models (except do.sim)
#'         (For a separate listing see \code{\link{calc_sim_args}} and 
#'        \code{\link{plot_sim_args}})
#'    \item "pls" for all arguments related to PLSR models (except do.pls)
#'        (For a separate listing see \code{\link{calc_pls_args}} and 
#'        \code{\link{plot_pls_args}})
#'    \item "aqg" for all arguments related to Aquagrams (except do.aqg)  
#'         (For a separate listing see \code{\link{calc_aqg_args}} and 
#'        \code{\link{plot_aqg_args}})
#'    \item "pg" for the general plotting options that are used in each of the 
#'         plotting functions. (For a separate listing see 
#'         \code{\link{plot_pg_args}})
#'  }
#'  By providing any of the arguments of the analysis procedure file to the 
#'  function \code{\link{getap}}, also when using it inside the function 
#'  \code{\link{gdmm}}, you can override the values in the file with the 
#'  provided values. See examples at \code{\link{gdmm}}.
#'  
#'@template mr_spl_split_param
#'  
#'@template mr_pca_calc_param
#'@template mr_pca_plot_param
#'
#'@template mr_sim_calc_param
#'@template mr_sim_plot_param
#'  
#'@template mr_pls_calc_param
#'@template mr_pls_plot_param
#'  
#'@template mr_aqg_calc_param
#'@template mr_aqg_plot_param
#'  
#'@template mr_pg_genParams
#'  
#'@details The default name for the analysis procedure file can be set in 
#'  settings.r. Any other .r file can be loaded by providing a valid .r filename 
#'  to the appropriate argument, e.g. in the function \code{\link{getap}}. 
#'  By providing any of the arguments of the analysis procedure file to the 
#'  function \code{\link{getap}} also when using it inside the function 
#'  \code{\link{gdmm}} or to any of the \code{\link{plot}} functions, you can 
#'  override the values in the file with the provided values. See examples at 
#'  \code{\link{gdmm}} and \code{\link{plot}}.
#'  
#'@seealso \code{\link{getap}}, \code{\link{gdmm}}
#'@family fileDocs
#'@name anproc_file
NULL
