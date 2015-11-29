#############################################
### from / after Ron Wehrens
pickPeaksPos_RW <- function(x, span) {
	x <- signal::sgolayfilt(x, p=2, n=21, m=0)
	span.width <- span *2+1
	loc.max <- span.width +1 - apply(embed(x, span.width), 1, which.max)
	loc.max[loc.max == 1 | loc.max == span.width] <- NA
	pks <- loc.max + 0:(length(loc.max)-1)
	unique(pks[!is.na(pks)])
} # EOF

pickPeaksNeg_RW <- function(x, span) {
	x <- signal::sgolayfilt(x, p=2, n=21, m=0)
	span.width <- span*2 +1
	locmin <- span.width +1 - apply(embed(x, span.width), 1, which.min)
	locmin[locmin == 1 | locmin == span.width] <- NA
	pks <- locmin + 0:(length(locmin)-1)
	minima <- unique(pks[!is.na(pks)])
	out <- sort(minima)
} #EOF

pickPeaks_RW <- function(x, span) {
	span.width <- span*2 +1
	loc.max <- span.width +1 - apply(embed(x, span.width), 1, which.max)
	loc.max[loc.max == 1 | loc.max == span.width] <- NA
	pks <- loc.max + 0:(length(loc.max)-1)
	maxima <- unique(pks[!is.na(pks)]) ############
	locmin <- span.width +1 - apply(embed(x, span.width), 1, which.min)
	locmin[locmin == 1 | locmin == span.width] <- NA
	pks <- locmin + 0:(length(locmin)-1)
	minima <- unique(pks[!is.na(pks)])
	out <- sort(c(maxima, minima))
} #EOF
#peaks <- pickPeaks_RW(kcl.sel.pca$loadings[,1], 40) 
#abline(v=wls[peaks], col="gray")
#############################################

##### dataset with wavelengths in the rows !!, the value-to-pick-peaks in  the column, so Factors in the column for PCA / PLSR
## ! provide only the loadings data as data frame! wavelengths have to be in the row!
pickPeaksInner <- function(vectorToBePickedFrom, bandwidth, discrim, wavelengths) { 
	vectorToBePickedFrom <- t(vectorToBePickedFrom)
	vecInput <- vectorToBePickedFrom
	seqHalfBandwidth<-seq(1, bandwidth/2)
	positivePeakPosition <- matrix(NA , ncol=length(wavelengths), nrow=nrow(vectorToBePickedFrom))
	positivePeakHeight <- negativePeakPosition <- negativePeakHeight <- positivePeakPosition
	rownames(positivePeakPosition) <- paste(rownames(vectorToBePickedFrom), "Pos_PosP", sep="-")
	rownames(positivePeakHeight) <- paste(rownames(vectorToBePickedFrom), "Hght_PosP", sep="-")
	rownames(negativePeakPosition) <- paste(rownames(vectorToBePickedFrom), "Pos_NegP", sep="-")
	rownames(negativePeakHeight) <- paste(rownames(vectorToBePickedFrom), "Hght_NegP", sep="-")
	if (.ap2$stn$pp_AutoSmooth) {
		vectorToBePickedFrom <- t(apply(vectorToBePickedFrom, 1, signal::sgolayfilt, p=2, n=5, m=0))
		# XXX what in the case of non-linear spaces between wavelengths ?? gap-segment derivative?
	} # end if autosmooth
	for(k in 1: nrow(vectorToBePickedFrom)) {
		spect <- vectorToBePickedFrom[k,] ## read in the spectrum row by row
		
		for (i in (max(seqHalfBandwidth)+1):(length(wavelengths)-max(seqHalfBandwidth))) {
	#		if (!( any(is.na(spect[i-seqHalfBandwidth])) | any(is.na(spect[i+seqHalfBandwidth])) ) ) {
				if  (all(spect[i-seqHalfBandwidth] < spect[i] & spect[i] > spect[i+seqHalfBandwidth]) ) {
					if (!discrim) {
						positivePeakPosition[k,i]<-wavelengths[i]
						positivePeakHeight[k,i] <- spect[i]
					} else {	# yes, we want to take only the really positive peaks
						if(spect[i] >= 0 ) {
							positivePeakPosition[k,i]<-wavelengths[i]
							positivePeakHeight[k,i] <- spect[i]
						} else {
							# do nothing
						}
					}
				} else {
					if  (all(spect[i-seqHalfBandwidth] > spect[i] & spect[i] < spect[i+seqHalfBandwidth]) ) {
						if(!discrim) {
							negativePeakPosition[k,i]<-wavelengths[i]
							negativePeakHeight[k,i]<-spect[i]
						} else { # yes please discriminate, take only the really negative peaks
							if (spect[i] < 0) {
								negativePeakPosition[k,i]<-wavelengths[i]
								negativePeakHeight[k,i]<-spect[i]
							} else {
								# do nothing
							}
						}			
					} # end peak to lower
				} # end side-else
		#	} # end if ! any is.na
		} # end for i
	}	# end for k
	pickResults <- rbind(positivePeakPosition, negativePeakPosition, positivePeakHeight, negativePeakHeight)
	colnames(pickResults) <- wavelengths
	picks <-  list(pickWindow = bandwidth, pickResult = pickResults)
	return(list(rawVector=vecInput, picks=picks)) ## pick results have the wavelength in the column !!!!
} # EOF

## Master
## the universal input, checking for the class of the input-object
pickPeaks <- function(ObjectToPickPeaks, bandwidth=25, comps=1:4, discrim=FALSE, wavelengths) {		### universal peak picker / extractor
	if (!exists("aquagramPSettings",  where=.ap2)) {
		assign("aquagramPSettings", readInAquagramPSettings(), pos=.ap2) # need this later for defining the special wavelengths
	}
	##
	if (class(ObjectToPickPeaks) == "mvr") {
		allColNames <- colnames(ObjectToPickPeaks$coefficients[,,1:ObjectToPickPeaks$ncomp])
		lastName <- allColNames[length(allColNames)]
		if (!is.character(lastName)) {		## problem if there is only one component -- we do not get a name back then .... somehow ..
			lastName <- "1 comps"
		}
		mat <- ObjectToPickPeaks$coefficients[,, ObjectToPickPeaks$ncomp]
		dfToPickPeaks <- data.frame(X=mat)
		colnames(dfToPickPeaks) <- lastName
	}
	if (class(ObjectToPickPeaks) == "PCA") {
		dfToPickPeaks <- as.data.frame(ObjectToPickPeaks$loadings[,comps])
	}
	if (class(ObjectToPickPeaks) == "data.frame") {
		dfToPickPeaks <- ObjectToPickPeaks
	}
	pickResultList <- pickPeaksInner(dfToPickPeaks, bandwidth, discrim, wavelengths) 
	return(pickResultList)
} # EOF

## needs the pick results object created by pickPeaks as input; contains the vector that was used for picking !
plotPickResults <- function (pickResults, onMain="", onSub="", pcaVariances=NULL, customColor=NULL, ylim=NULL, wavelengths) { 		#### here the matplot is called
	if (length(pickResults$rawVector) < 1) {
		stop("An Error at plotPickResults occured.", call.=FALSE)
	}
	res <- pickResults$picks$pickResult
	colPos <- .ap2$stn$pp_colPosPeaks
	colNeg <-  .ap2$stn$pp_colNegPeaks
	positionTable <-res[1: (nrow(res)/2) ,]
	heigthTable <-res[((nrow(res)/2)+1):nrow(res) , ]
	Yrange <- range(pickResults$rawVector)[2] - range(pickResults$rawVector)[1]
	if (onSub=="") {
		osFill <- ""
	} else {
		osFill <- ", "
	}
	onSub <- paste(onSub, osFill, "bw=", pickResults$picks$pickWindow, sep="")
	ind99 <- pcaVariances$ind99
	vars <- pcaVariances$vars
	if (!is.null(pcaVariances)) {
		if (ind99 == "") {
			legendText <- rownames(pickResults$rawVector)
			onSubText <- onSub
		} else { 	## so if we come in from a pca model
			legendText <- paste(rownames(pickResults$rawVector), " (", vars, "%)", sep="")
			onSubText <- paste(onSub, ", ", ind99, " PCs for 99% var.", sep="")
		}
	} else {
		legendText <- rownames(pickResults$rawVector)
		onSubText <- onSub
	}
	if (is.null(customColor)) {
		ColorYea <- 1:nrow(pickResults$rawVector)
	} else {
		ColorYea <- customColor
	}
	clt <- 1:nrow(pickResults$rawVector)
	matplot(wavelengths, t(pickResults$rawVector), type="l", ylab="Coefficient", main=onMain, sub=onSubText, col=ColorYea, ylim=ylim, lty=clt) #### here the matplot
	abline(0,0, col="gray")
	legend("topright", legend=legendText, lty=clt, col=ColorYea, lwd=2.5) 	#### XXX legend problem here
		
	for (i in 1: nrow(positionTable)) {
		if (i <= (nrow(positionTable)/2)) {
			if (!all(is.na(positionTable[i,]))) {
				peakPosition <- positionTable[i, -(which(is.na(positionTable[i,])))]
				peakHeight <- heigthTable[i, -(which(is.na(heigthTable[i,])))]
			#	cat("peak Position:", peakPosition, "\n");  cat("peak Height", peakHeight, "\n"); wait()
				text(peakPosition, peakHeight + Yrange/49.6, labels=round(peakPosition, 0), col=colPos, cex=0.8)
			} # end if all na check
		} else {
			if (!all(is.na(positionTable[i,]))) {	
				peakPosition <- positionTable[i, -(which(is.na(positionTable[i,])))]
				peakHeight <- heigthTable[i, -(which(is.na(heigthTable[i,])))]
			#	cat("peak Position:", peakPosition, "\n");  cat("peak Height", peakHeight, "\n"); wait()
			#	labelsCont <- round(peakPosition, 0); print(labelsCont); wait()
				text(peakPosition, peakHeight -Yrange/49.6, labels=round(peakPosition,0), col=colNeg, cex=0.8)
			} # end if all na check
		} # end else
	} # end for i
}# EOF

plotVerticalLinesFromPeaks <- function(pickResults, customColor=NULL) {
#	cc <- customColor
	if (is.null(customColor)) {
		cc <- 1:nrow(pickResults$rawVector)
	} else {
		cc <- customColor
	}
	res <- pickResults$picks$pickResult	
#	colPos <- .ap2$stn$pp_colPosPeaks
#	colNeg <- .ap2$stn$pp_colNegPeaks
	droppLwd <- .ap2$stn$pp_droppLwd
	positionTable <-res[1: (nrow(res)/2) ,]
	heigthTable <-res[((nrow(res)/2)+1):nrow(res) , ]
	for (i in 1: nrow(positionTable)) {
		if (i <= (nrow(positionTable)/2)) {
			if (!all(is.na(positionTable[i,]))) {
				peakPosition <- positionTable[i, -(which(is.na(positionTable[i,])))]
				peakHeight <- heigthTable[i, -(which(is.na(heigthTable[i,])))]
				segments(x0=peakPosition, y0=0, y1=peakHeight, col=cc[i], lwd=droppLwd, lty=1)
			} # end if all na check
		} else {
			if (!all(is.na(positionTable[i,]))) {
				peakPosition <- positionTable[i, -(which(is.na(positionTable[i,])))]
				peakHeight <- heigthTable[i, -(which(is.na(heigthTable[i,])))]
				segments(x0=peakPosition, y0=0, y1=peakHeight, col=cc[i-(nrow(positionTable)/2)], lwd=droppLwd, lty=1)
			} # end if all na check
		}
	} # end for i
} # EOF

plotSpecialWlsLines <- function(pickResults) { # we need the pick results for determining the y-range
	fac <- .ap2$stn$wamac_factor
	specACol1 <- .ap2$stn$wamac_col1
	specACol2 <- .ap2$stn$wamac_col2
	#
	Yspan <- a <- range(pickResults$rawVector)[2] - range(pickResults$rawVector)[1]
	height <- (Yspan*fac)/2
	py <- c(-height, -height, height, height) 		## for drawing the polygon
	#
	plotSpecAreas <- function(){
		specA <- readInSpecAreas() 	# is in the file "gen_general.r"
		alternColor <- rep(c(specACol1, specACol2), ceiling(ncol(specA)/2))
		for (i in 1:ncol(specA)) {
			px <- c(specA[,i], rev(specA[,i]))
			polygon(px, py, col=alternColor[i], border=NA)
		} # end for i
	abline(h=0, col="gray")
	} # EOIF
	plotSpecAreas()
} # EOF

plotHumidityWlsLines <- function(pickResults) {	
	humWls <- .ap2$stn$hum_wls
	humFact <- .ap2$stn$hum_fact
	humLwd <- .ap2$stn$hum_lwd
	humColor <- .ap2$stn$hum_color
	Yspan <- range(pickResults$rawVector)[2] - range(pickResults$rawVector)[1]
	height <- (Yspan*humFact)/2
	segments(x0=humWls,  y0= -3.4*(height), y1= -1.9*(height), lwd=humLwd, col=humColor)
} # EOF

plotDelGiudiceAreas <- function(pickResults) {	
	dga1 <- .ap2$stn$dga_dga1
	dga2 <- .ap2$stn$dga_dga2
	dga3 <- .ap2$stn$dga_dga3
	cohCol <- .ap2$stn$dga_Coh_col
	gasCol <- .ap2$stn$dga_Gas_col
	facDga <- .ap2$stn$dga_facDga
	Yspan <- range(pickResults$rawVector)[2] - range(pickResults$rawVector)[1]
	height <- a <- ((Yspan*facDga)/2)
	py <- c(-a, -a, a, a) - (Yspan/55)
	px1 <- c(dga1, rev(dga1))
	px2 <- c(dga2, rev(dga2))
	px3 <- c(dga3, rev(dga3))
	px <- c(px1, px2, px3)
	polygon(px1, py, col=cohCol, border=NA)
	polygon(px2, py, col=gasCol, border=NA)
	polygon(px3, py, col=cohCol, border=NA)
} # EOF

# Master
plotPeaks <- function(pickPeaksResult, onMain="", onSub="", adLines=TRUE, pcaVariances=NULL, customColor=NULL, ylim=NULL, wavelengths) {
	plotPickResults(pickPeaksResult, onMain, onSub, pcaVariances, customColor, ylim, wavelengths)
#	if (any(adLines==1)) { # so it is TRUE or 1, plot all additional lines
#		plotDelGiudiceAreas(pickPeaksResult)
#		plotSpecialWlsLines(pickPeaksResult)
#		plotVerticalLinesFromPeaks(pickPeaksResult, customColor)
#		plotHumidityWlsLines(pickPeaksResult)
#	}
	if (any(adLines==3) | any(adLines==TRUE)){
		plotDelGiudiceAreas(pickPeaksResult)
	}
	if (any(adLines==4) | any(adLines==TRUE)){
		plotSpecialWlsLines(pickPeaksResult)
	}
	if (any(adLines==2) | any(adLines==TRUE)){
		plotVerticalLinesFromPeaks(pickPeaksResult, customColor)
	}
	if (any(adLines==5) | any(adLines==TRUE)){
		plotHumidityWlsLines(pickPeaksResult)
	}
} # EOF



#' @title Ad Lines to a Vector Plot
#' @description Ad various lines, i.e. graphical elements, to the plot of a 
#' vector like loading or regression vector.
#' @details XXX
#' @name adLinesToVector
NULL

