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


if (!exists("dbplyr")) {
	dbplyr <- makeDBPlyr()
}

if (!exists("dbcon")) {
	dbcon <- makeDBCon()
}

nt <- tbl(dbplyr, sql("select * from nir"))

getNIRdb <- function(eid, nirtable=nt) {
	fullRow <- collect(filter(nirtable, expId==eid))
	nirmat <- matrix(as.numeric(eval(parse(text=paste("c('", gsub(" ", "', '", fullRow$nirdata), "')", sep="")))), nrow=fullRow$nrows)
#	colnames(nirmat) <- eval(parse(text=paste("c('", gsub(" ", "', '", fullRow$wls), "')", sep="")))
#	rownames(nirmat) <- eval(parse(text=paste("c('", gsub(" ", "', '", fullRow$rowNames), "')", sep="")))
	return(invisible(nirmat))
} # EOF

# x is the dataset
writeNIRdb <- function(x, eid=4, con=dbcon) {
	wlsR <- range(getWavelengths(x))
	nird <- paste(as.character(c(x$NIR)), collapse=" ")
	rns <- paste(rownames(x$NIR), collapse=" ")
	wlsn <- paste(colnames(x$NIR), collapse=" ")
	datframe <- data.frame(expId=eid, nirdata=nird, nrows=nrow(x$NIR), wls=wlsn, rowNames=rns, ncpwl=x@ncpwl, ranFrom=wlsR[1], ranTo=wlsR[2])
	DBI::dbWriteTable(con, "nir", datframe, append=TRUE, row.names=FALSE)
	invisible(NULL)	
} # EOF

	
makeNir <- function(dataset) {
	return(c(dataset$NIR))
} 


