#'  @param do.pca Logical. If used in a plotting function, if PCA score / loading 
#'  plots should be plotted.
#'  @param pca.colorBy NULL or character vector. Which class-variables 
#'  should be used for coloring the PCA score plot. Set to NULL for using all 
#'  available class variables for coloring.
#'  @param pca.what Character length one. What element of the PCA analysis to plot.
#'  Possible values are <%=r_listize(pv_pca_what)%>.
#'  @param pca.sc Numeric length 2. Two PCs to be plotted against each other 
#'  in the score plots.
#'  @param pca.sc.pairs Numeric vector of length >=2, indicating what PCs 
#'  to plot in the score pairs plot. Set to NULL for *not* plotting the pairs 
#'  plot.
#'  @param pca.lo  Numeric vector of length >=2, indicating what PCs to plot in 
#'  the loadingplot.
