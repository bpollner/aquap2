	## basic settinsg
	dbname <- "aquap2"
	user <- "bernhard"
	pwd <- "hybrid12"
	host <- "localhost"

library(RMySQL); library(DBI)

makeDBCon <- function() {
	dbcon <- DBI::dbConnect(RMySQL::MySQL(), user=user, password=pwd, dbname=dbname, client.flag = CLIENT_MULTI_STATEMENTS, host=host)
} # EOF

makeDBPlyr <- function() {
	dbplyr <- src_mysql(dbname=dbname, host=host, user=user, password=pwd)
} # EOF

makeRandomString <- function(n=28) {
	pool <- sample(c(letters, LETTERS, seq(1:9)))
	a <- paste("nird_", paste(sample(pool, n), collapse=""), sep="")
	return(a)
} # EOF

###########################

if (!exists("dbplyr")) {
	dbplyr <- makeDBPlyr()
}

if (!exists("dbcon")) {
	dbcon <- makeDBCon()
}
############################


wr  <- function(dataset, eid, con=dbcon) {
	path <- Sys.getenv("AQUAP2DATA")
	fn <- makeRandomString()
	pathName <- paste(path, fn, sep="/")
	NIR <- dataset$NIR
	wls <- getWavelengths(dataset)
	wlsR <- range(wls)
	diffwls <- diff(wls)
	if( (sd(diffwls)== 0) ) {
		dwl <- mean(diffwls)
	} else {
		dwl <- NA
	}
	datf <- data.frame(expId=eid, nir_as3="", nir_loc=fn, nrows=nrow(NIR), ncols=ncol(NIR), ncpwl=dataset@ncpwl, delta_wl=dwl, ranFrom=wlsR[1], ranTo=wlsR[2])
	DBI::dbWriteTable(con, "nir", datf, append=TRUE, row.names=FALSE)
	save(NIR, file=pathName)
	invisible(NULL)
}

rr <- function(eid) {
	path <- Sys.getenv("AQUAP2DATA")
	nt <- tbl(dbplyr, sql("select * from nir"))
	fullRow <- collect(filter(nt, expId==eid))
	localName <- fullRow$nir_loc
	pathName <- paste(path, localName, sep="/")
	nir <- eval(parse(text=(load(pathName)))) # :-)
	return(invisible(nir))
}










