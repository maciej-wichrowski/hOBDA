library(plotrix)

# This function requires the variable rDataDir to be set to the directory
# where RData can be found. Not a very elegant construct, but it works...
getHaskellRData <- function(fn, csv, alg) {
	curDir <- getwd()
	setwd(rDataDir)
	system(paste("./RData", fn, csv, alg, "> tmp.csv"))
	data <- read.csv('tmp.csv', head=TRUE, sep=';')
	system("rm tmp.csv")
	setwd(curDir)
	return(data)
}

getGroupSize <- function(alg) {
   data <- getHaskellRData("featureGroupSize", "", alg)
   return(data$Y)
}

bgColor      <- rgb(0.98, 0.98, 0.98)
gridColor    <- rgb(0.8, 0.8, 0.8, alpha=0.7)
pointColor   <- rgb(0.15, 0.50, 0.75)
barColor     <- rgb(0.15, 0.50, 0.75, alpha=0.8)
areaColor    <- rgb(0.15, 0.50, 0.75, alpha=0.4)

chart_mgp <- c(5, 2, 0)
chart_mar <- c(6, 7, 2,2) + 0.1

errorPlot <- function(xs, ys, fancy=FALSE) {
   par(cex=1, cex.lab=2.3, cex.axis=2.3, mgp=chart_mgp, mar=chart_mar)
   
	plot(xs, ys, type="n", xlab="Training examples", ylab="Error rate"
	                     , ylim=c(0, 1))
	
	color <- rgb(0, 0, 0)
	pointSize <- 0.4
	
	if (fancy) {
	   color <- pointColor
	   pointSize <- 0.6
	   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=bgColor)
	   grid(nx=0, ny=NULL, col=gridColor, lty="solid")
   	polygon(c(1, xs, xs[length(xs)]), c(0, ys, 0), col=areaColor, border=NA)
   }
   
	lines(xs, ys, type="o", cex=pointSize, pch=19, col=color)
}

pdfErrorPlot <- function(pdfname, csv, alg, fancy=FALSE) {
   data <- getHaskellRData("errorRates", csv, alg)
   pdf(pdfname)
	errorPlot(data$X, data$Y, fancy)
	dev.off()
}



sensitivityPlot <- function(xs, ys, regr) {
   par(cex=1, cex.lab=2.3, mgp=chart_mgp, mar=chart_mar)
	plot(xs, ys
	       , xlab="Errors injected", ylab="Error rate"
	       , cex.axis=2.3, pch=18
	       , ylim=c(0, 1))
	if (regr) {
		lo <- loess(ys~xs, span=0.6, 
		                   family = "symmetric",
		                   control=loess.control(surface = "direct",
                                               statistics = "exact",
                                               trace.hat = "exact",
                                               cell = 0.2, 
                                               iterations = 4))
		lines(predict(lo), col="blue", lwd=2)
	}
}

pdfSensitivityPlot <- function(pdfname, csv, alg, regr) {
   data <- getHaskellRData("sensitivity", csv, alg)
	pdf(pdfname)
	sensitivityPlot(data$X, data$Y, regr)
	dev.off()
}


weightsPlot <- function(xs, ys, groupSize, fancy=FALSE) {
   if (groupSize > 1) {
      for(i in seq(1, length(ys), 2)) {
         dY = min(c(ys[i], ys[i+1]))
         ys[i] = ys[i] - dY
         ys[i + 1] = ys[i + 1] - dY
      }
   }
   
   bColor <- "gray"
   if (fancy) {
      bColor <- barColor
   }
   
   ylim <- c(min(0, min(ys)), max(0, max(ys)))
   axisStagger <- groupSize > 1
   axisNames <- xs
   if (axisStagger) {
      axisNames <- rep("", length(xs))
   }
   
   
   par(cex=1, cex.lab=2.3, mgp=chart_mgp, mar=chart_mar)
   barp(ys, width=0.37, names=axisNames, ylim=ylim, col=bColor
	       , xlab="Features", ylab="Feature Weight"
	       , cex.axis=2.3
	       , do.first=expression(if (fancy) {
	                                 rect( par("usr")[1]
	                                     , par("usr")[3]
	                                     , par("usr")[2]
	                                     , par("usr")[4]
	                                     , col=bgColor)
	                                 grid(nx=0, ny=NULL, col=gridColor, lty="solid")
	                             })
	    )
	
#   axis(1,at=x,labels=rep("",ngroups),cex.axis=cex.axis)
   if (axisStagger) {
      staxlab(1,at=1:length(xs),labels=xs,cex=2.3, top.line=1.5)
   }

   for (i in 1:(length(xs) - 1)) {
 	   if ((i %% groupSize) == 0) {
    	   segments(i + 0.5, ylim[1], i + 0.5, ylim[2])
    	}
 	}
}

pdfWeightsPlot <- function(pdfname, csv, alg, fancy=FALSE) {
   data <- getHaskellRData("featureWeights", csv, alg)
   pdf(pdfname)
	groupSize <- getGroupSize(alg)
	weightsPlot(data$X, data$Y, groupSize, fancy)
	dev.off()
}