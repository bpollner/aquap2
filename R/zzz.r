#######################################################################
### the function that gets called when loading / starting a package

.onLoad <- function(libname, pkgname) {
    .GlobalEnv$.ap2 <- new.env()
} # EOF
