#######################################################################
### the function that gets called when loading / starting a package

.onLoad <- function(libname, pkgname) {
    .initializeAQ2Classes()
    .initializeAQ2Methods()
    .GlobalEnv$.ap2 <- new.env()
} # EOF
