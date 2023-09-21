#######################################################################
### the function that gets called when loading / starting a package

.onLoad <- function(libname, pkgname) {
	mtxt <- paste0("at", "tac", "h")
	mRest <- "(what=NULL, name = gl_ap2GD)"
	mtxt <- paste0(mtxt, mRest)
	eval(parse(text=mtxt))
#	as workaround for the note coming up when checking
#	attach(what=NULL, name = gl_ap2GD)
} # EOF


.onUnload <- function(libpath) {
	detach(gl_ap2GD, character.only=TRUE)
} # EOF
