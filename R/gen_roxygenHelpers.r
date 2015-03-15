r_itemize <- function(charVec) {
	a <- paste("\\item ", charVec, sep="")
	return(paste("\\itemize{", paste(a, collapse=" "), "}"))
} # EOF

r_listize <- function(charVec) {
	return(paste("'", paste(charVec, collapse="', '"), "'", sep=""))
} # EOF
