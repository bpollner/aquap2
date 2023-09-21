rw_PCA <- function(X, warn = TRUE) {
  ndf <- nrow(X) - 1
  X.svd <- svd(X)
  varnames <- colnames(X)
  if (is.null(varnames)) varnames <- paste("Var", 1:ncol(X))

  object <- list(scores = X.svd$u %*% diag(X.svd$d),
                 loadings = X.svd$v,
                 var = X.svd$d^2 / ndf,
                 totalvar = sum(X.svd$d^2)/ndf)
  dimnames(object$scores) <- list(rownames(X),
                                  paste("PC", 1:ncol(object$scores)))
  dimnames(object$loadings) <- list(varnames,
                                    paste("PC", 1:ncol(object$loadings)))
  names(object$var) <- paste("PC", 1:length(X.svd$d))

  if (!isTRUE(all.equal(colMeans(X), rep(0, ncol(X)),
                        check.attributes = FALSE))) {
    if (warn)
      warning("Performing PCA on a non-meancentered data matrix!")
    object$centered.data <- FALSE
  } else {
    object$centered.data <- TRUE
  }
  class(object) <- "PCA"
  return(object)
} # EOF

rw_variances <- function(object, npc = maxpc) {
  maxpc <- max(ncol(object$loadings), ncol(object$scores))
  if (npc > maxpc) {
    warning(paste("Maximal number of PCs:", maxpc))
    npc <- maxpc
  }
  return(object$var[1:npc])
} # EOF

rw_screeplot <- function(object, type = c("scree", "percentage"), npc, ...) {
  if (missing(npc)) npc <- length(rw_variances(object))
  type <- match.arg(type)

  vars <- switch(type,
                 scree = log(rw_variances(object)[1:npc]),
                 percentage = cumsum(100*object$var[1:npc]/object$totalvar))
  ylab <- switch(type,
                 scree = "log(variance)",
                 percentage = "% variance")

  return(barplot(vars, names.arg = 1:npc, xlab = "# PCs", ylab = ylab, ...))
} # EOF
