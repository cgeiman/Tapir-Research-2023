###=============================================================================
# Date: June, 2023
# Author: Cate Geiman -- This code adapted from Amy Eppert's
# Title: "Single species circadian plots with night shading and %Night"
# Description: This code takes solar data for each of the four tapir species
#   and creates kernel density plots for each to show the circadian activity 
#   patterns of each. Percent Night is a value that is determined by 
#   calculating the ratio of records between sunset and sunrise (nighttime) 
#   compared to records in the daytime to get a better understanding of 
#   nocturnal/diurnal activity patterns.
# Output: .tif
###=============================================================================


#===============================================================================
#----Individual Single Species Density Plots with Night Shading and %Night------
#===============================================================================


#Load required package
require(overlap)

#import datasets
library(readr)
bairdSunData <- read.csv("SunData/bairdSunDataIndependent.csv")
lowlandSunData <- read.csv("SunData/lowlandSunData.csv")
malayanSunData <- read.csv("SunData/malayanSunData.csv")
mountainSunData <- read.csv("SunData/mountainSunData.csv")

#Adjust these to change text size of labels on the axis and species name
xaxisSize = 0.7
yaxisSize = 0.7
textSize = 0.5

#Assign each species an animal number for simpler reference
animal1 <- bairdSunData$solar
animal2 <- lowlandSunData$solar
animal3 <- malayanSunData$solar
animal4 <- mountainSunData$solar

###Calculate % Night for each species (these values will be added to the plots later)
#Baird's------------------------------------------------------------------------
(length(bairdSunData$solar[bairdSunData$solar < (pi/2) | bairdSunData$solar > ((3*pi)/2)]))/
  (length(bairdSunData$solar))
#%Night: 0.82

#Lowland------------------------------------------------------------------------
(length(lowlandSunData$solar[lowlandSunData$solar < (pi/2) | lowlandSunData$solar > ((3*pi)/2)]))/
  (length(lowlandSunData$solar))
#%Night: 0.83

#Malayan------------------------------------------------------------------------
(length(malayanSunData$solar[malayanSunData$solar < (pi/2) | malayanSunData$solar > ((3*pi)/2)]))/
  (length(malayanSunData$solar))
#%Night: 0.88

#Mountain-----------------------------------------------------------------------
(length(mountainSunData$solar[mountainSunData$solar < (pi/2) | mountainSunData$solar > ((3*pi)/2)]))/
  (length(mountainSunData$solar))
#%Night: 0.63

#Edit density plot function so it includes nighttime shading 
densityPlotCustom <- function (A, xscale = 24, 
                               xcenter = c("noon", "midnight"), 
                               add = FALSE, 
                               rug = FALSE, 
                               extend = NULL, 
                               n.grid = 128, 
                               kmax = 3, 
                               adjust = 1, 
                               lwd=2, 
                               nightShade=TRUE, 
                               xaxsSelect="i", ...) {
  
  isMidnt <- match.arg(xcenter) == "midnight"
  bw <- getBandWidth(A, kmax = kmax)/adjust
  if (is.na(bw)) 
    stop("Bandwidth estimation failed.")
  if (is.null(extend)) {
    xx <- seq(0, 2 * pi, length = n.grid)
  }
  else {
    xx <- seq(-pi/4, 9 * pi/4, length = n.grid)
  }
  if (isMidnt) 
    xx <- xx - pi
  
  densA <- densityFit(A, xx, bw)
  xsc <- if (is.na(xscale)) 
    1
  else xscale/(2 * pi)
  toPlot <- cbind(x = xx * xsc, y = densA/xsc)
  dots <- list(...)
  if (length(dots) == 1 && class(dots[[1]]) == "list") 
    dots <- dots[[1]]
  defaultArgs <- list(main = deparse(substitute(A)), 
                      bty = "o", 
                      type = "l", 
                      xlab = "Time", 
                      ylab = "Density", 
                      lwd=2, 
                      font.axis=2, 
                      ylim =c(0, max(toPlot[, "y"])))
  
  useArgs <- modifyList(defaultArgs, dots)
  if (!add) {
    selPlot <- names(useArgs) %in% c(names(as.list(args(plot.default))), 
                                     names(par(no.readonly = TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$x <- toPlot
    plotArgs$y <- NULL
    plotArgs$type <- "n"
    plotArgs$xaxt <- "n"
    plotArgs$xaxs <- xaxsSelect #custom addition. "i" means no white space between data and xaxis
    do.call(plot, plotArgs, quote = TRUE)
    abline(h = 0, col = "grey")
    edge <- par("usr")
    if(nightShade){
      rect(edge[1],edge[3],6,edge[4], col=rgb(105/255, 167/255, 203/255,0.6),border=NA) #custom addition to color hours before dawn. 0.6 transparency (changed to 0.4 for overlap)
      rect(18,edge[3],edge[2],edge[4], col=rgb(105/255, 167/255, 203/255,0.6),border=NA) #custom addition to color hours after sunset. 0.6 transparency (changed to 0.4 for overlap)
    }
    
    if (!is.null(extend)) {
      if (isMidnt) {
        wrap <- c(-pi, pi) * xsc
      }
      else {
        wrap <- c(0, 2 * pi) * xsc
      }
      rect(c(edge[1], wrap[2]), rep(edge[3], 2), c(wrap[1], 
                                                   edge[2]), rep(edge[4], 2), border = NA, col = extend)
      box(bty = useArgs$bty)
    }
  }
  selPlot <- names(useArgs) %in% names(par(no.readonly = TRUE))
  plotArgs <- useArgs[selPlot]
  plotArgs$x <- toPlot
  plotArgs$y <- NULL
  do.call(lines, plotArgs, quote = TRUE)
  if (rug) {
    if (isMidnt) 
      A <- ifelse(A < pi, A, A - 2 * pi)
    rug(A * xsc, ...)
  }
  return(invisible(as.data.frame(toPlot)))
}

#===============================================================================
#Adjust and run density plot function for each species (and create tiff file)---

#Baird's------------------------------------------------------------------------

#will create a tiff file of plot
tiff(file = "BairdCircadian.tif", width=14500, height=8000, res=1200)
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#plot
plotDensity <- function(animal1) {
  par(ps=20, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  densityPlotCustom(
    animal4
    ,main="Baird's Tapir Circadian Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    #,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  #axis(2,at=axTicks(2)[c(1,length(axTicks)-1)],labels=axTicks(2)[c(1,length(axTicks)-1)],cex.axis=yaxisSize,font=2)#only first and last
  
  #add %Night to the plot
  mtext(expression(bold("%Night = 0.82")), side=3, line=0, at=-0.03, adj=-4.3, cex=1)
  
  #add RAI value (if applicable)
  mtext(expression(bold("RAI = 46.63")), side=3, line=0, at=0.3, adj=0.5, cex=1.1)
  
  #add n value (if applicable)
  mtext(paste(animal4[1]," n=",length(animal1)),3,cex=textSize, adj=0,font=2)
  
}

par(xpd=TRUE)
dev.off() ###tiff file will now appear in folder

#Lowland------------------------------------------------------------------------

#will create a tiff file of plot
tiff(file = "LowlandCircadian.tif", width=14500, height=8000, res=1200)
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#plot
plotDensity <- function(animal2) {
  par(ps=20, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  densityPlotCustom(
    animal4
    ,main="Lowland Tapir Circadian Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    #,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  #axis(2,at=axTicks(2)[c(1,length(axTicks)-1)],labels=axTicks(2)[c(1,length(axTicks)-1)],cex.axis=yaxisSize,font=2)#only first and last
  
  #add %Night to the plot
  mtext(expression(bold("%Night = 0.83")), side=3, line=0, at=-0.03, adj=-4.3, cex=1)
  
  #add RAI value (if applicable)
  mtext(expression(bold("RAI = 24.97")), side=3, line=0, at=0.3, adj=0.5, cex=1.1)
  
  #add n value (if applicable)
  mtext(paste(animal4[1]," n=",length(animal2)),3,cex=textSize, adj=0,font=2)
  
}

par(xpd=TRUE)
dev.off() ###tiff file will now appear in folder

#Malayan------------------------------------------------------------------------

#will create a tiff file of plot
tiff(file = "MalayanCircadian.tif", width=14500, height=8000, res=1200)
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#plot
plotDensity <- function(animal3) {
  par(ps=20, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  densityPlotCustom(
    animal4
    ,main="Malayan Tapir Circadian Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    #,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  #axis(2,at=axTicks(2)[c(1,length(axTicks)-1)],labels=axTicks(2)[c(1,length(axTicks)-1)],cex.axis=yaxisSize,font=2)#only first and last
  
  #add %Night to the plot
  mtext(expression(bold("%Night = 0.88")), side=3, line=0, at=-0.03, adj=-4.3, cex=1)
  
  #add RAI value (if applicable)
  mtext(expression(bold("RAI = 28.42")), side=3, line=0, at=0.3, adj=0.5, cex=1.1)
  
  #add n value (if applicable)
  mtext(paste(animal4[1]," n=",length(animal3)),3,cex=textSize, adj=0,font=2)
  
}

par(xpd=TRUE)
dev.off() ###tiff file will now appear in folder

#Mountain-----------------------------------------------------------------------

#will create a tiff file of plot
tiff(file = "MountainCircadian.tif", width=14500, height=8000, res=1200)
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#plot
plotDensity <- function(animal4) {
  par(ps=20, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  densityPlotCustom(
    animal4
    ,main="Mountain Tapir Circadian Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    #,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  #axis(2,at=axTicks(2)[c(1,length(axTicks)-1)],labels=axTicks(2)[c(1,length(axTicks)-1)],cex.axis=yaxisSize,font=2)#only first and last
 
  #add %Night to the plot
  mtext(expression(bold("%Night = 0.63")), side=3, line=0, at=-0.03, adj=-4.3, cex=1)
  
  #add RAI value (if applicable)
  mtext(expression(bold("RAI = 10.05")), side=3, line=0, at=0.3, adj=0.5, cex=1.1)
  
  #add n value (if applicable)
  mtext(paste(animal4[1]," n=",length(animal4)),3,cex=textSize, adj=0,font=2)
  
}

par(xpd=TRUE)
dev.off() ###tiff file will now appear in folder

