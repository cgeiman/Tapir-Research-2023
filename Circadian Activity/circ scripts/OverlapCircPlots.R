###=============================================================================
# Date: June, 2023
# Author: Cate Geiman -- This code adapted from Amy Eppert's
# Title: "Overlap circadian plots with coefficient of overlap"
# Description: This code takes solar data for each of the four tapir species
#   and creates kernel density plots to show the circadian activity 
#   patterns of each. The coefficient of overlap (∆) is determined to better
#   understand similarity between patterns.
# Output: .tif
###=============================================================================


#=============Overlap circadian plots with coefficient of overlap==============#
#required packages
require(overlap)
require(bayestestR)

#import datasets
library(readr)
bairdSunData <- read.csv("SunData/bairdSunDataIndependent.csv")
lowlandSunData <- read.csv("SunData/lowlandSunData.csv")
malayanSunData <- read.csv("SunData/malayanSunData.csv")
mountainSunData <- read.csv("SunData/mountainSunData.csv")

#Assign each species an animal number for simpler reference
animal1 <- bairdSunData$solar
animal2 <- lowlandSunData$solar
animal3 <- malayanSunData$solar
animal4 <- mountainSunData$solar

#Create custom overlap function to include night shading
overlapPlotCustom <- function (A, B, xscale = 24, 
                               xcenter = c("noon", "midnight"), 
                               linetype = c(1, 2), 
                               linecol = c("black", "blue"), 
                               linewidth = c(2,2), 
                               olapcol = "lightgrey", 
                               rug = FALSE, 
                               extend = NULL,  
                               n.grid = 128, 
                               kmax = 3, 
                               adjust = 1, 
                               nightShade = TRUE, 
                               xaxsSelect="i", ...) 
{
  isMidnt <- match.arg(xcenter) == "midnight"
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale)) 
    1
  else xscale/(2 * pi)
  if (is.null(extend)) {
    xxRad <- seq(0, 2 * pi, length = n.grid)
  }
  else {
    xxRad <- seq(-pi/4, 9 * pi/4, length = n.grid)
  }
  if (isMidnt) 
    xxRad <- xxRad - pi
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  dots <- list(...)
  if (length(dots) == 1 && class(dots[[1]]) == "list") 
    dots <- dots[[1]]
  defaultArgs <- list(main = paste(deparse(substitute(A)), 
                                   "and", deparse(substitute(B))), xlab = "Time", ylab = "Density", 
                      bty = "o", type = "l", xlim = range(xx), ylim = c(0, 
                                                                        max(densA, densB)), font.axis=2)
  useArgs <- modifyList(defaultArgs, dots)
  selPlot <- names(useArgs) %in% c(names(as.list(args(plot.default))), 
                                   names(par(no.readonly = TRUE)))
  plotArgs <- useArgs[selPlot]
  plotArgs$x <- 0
  plotArgs$y <- 0
  plotArgs$type <- "n"
  plotArgs$xaxt <- "n"
  plotArgs$xaxs <- xaxsSelect #custom addition. "i" means no white space between data
  
  do.call(plot, plotArgs, quote = TRUE)
  polygon(c(max(xx), min(xx), xx), c(0, 0, densOL), border = NA, 
          col = olapcol)
  edge <- par("usr")
  if (nightShade){
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
    edge <- par("usr")
    rect(c(edge[1], wrap[2]), rep(edge[3], 2), c(wrap[1], 
                                                 edge[2]), rep(edge[4], 2), border = NA, col = extend)
    box(bty = useArgs$bty)
  }
  segments(xx[1], 0, xx[n.grid], 0, lwd = 0.5)
  lines(xx, densA, lty = linetype[1], col = linecol[1], lwd = linewidth[1])
  lines(xx, densB, lty = linetype[2], col = linecol[2], lwd = linewidth[2])
  if (rug) {
    if (isMidnt) {
      A <- ifelse(A < pi, A, A - 2 * pi)
      B <- ifelse(B < pi, B, B - 2 * pi)
    }
    axis(1, at = A * xsc, labels = FALSE, tcl = 0.35, lwd = 0, 
         lwd.ticks = 0.5, col = linecol[1])
    axis(1, at = B * xsc, labels = FALSE, tcl = -0.35, lwd = 0, 
         lwd.ticks = 0.5, pos = 0, col = linecol[2])
  }
  return(invisible(data.frame(x = xx, densityA = densA, densityB = densB)))
}

#Calculate coefficient of overlap (∆) for each -- add these values to the plots later
overlap(animal1, animal2)
overlap(animal1, animal3)
overlap(animal1, animal4)
overlap(animal2, animal3)
overlap(animal2, animal4)
overlap(animal3, animal4)

#PLOTS--------------------------------------------------------------------------
#note: if you don't want a tif file, just ignore that piece of the code
#==============================================================================#
#============================ Baird's and Lowland =============================#
#Edit plotOverlap function
plotOverlap <- function(animal1, animal2) {
  par(ps=25, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  overlapPlotCustom(
    animal1, animal2
    ,main="Baird's and Lowland Circadian Overlap Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    ,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label

}

tiff(file = "CircadianOverlapBairdLowland.tif", width=13500, height=11000, res=1200)#will output a .tif file
#Set margin sizes -- might need to adjust these
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#run plot
plotOverlap(animal1, animal2)

#add legend
legend(x = "top", 
       legend = c("Baird's", "Lowland"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))

#Calculate coefficient of overlap (∆)
overlap(animal1, animal2)

#Add this value (∆) to the plot
mtext("∆=0.81", side=3, line=0, at=-0.03, adj=-7.5, cex=1)

par(xpd=TRUE)
dev.off()


#==============================================================================#
#============================ Baird's and Malayan =============================#
#Edit plotOverlap function
plotOverlap <- function(animal1, animal3) {
  par(ps=25, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  overlapPlotCustom(
    animal1, animal3
    ,main="Baird's and Malayan Circadian Overlap Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    ,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label

}

tiff(file = "CircadianOverlapBairdMalayan.tif", width=13500, height=11000, res=1200)#will output a .tif file
#Set margin sizes -- might need to adjust these
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#run plot
plotOverlap(animal1, animal3)

#add legend
legend(x = "top", 
       legend = c("Baird's", "Malayan"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))


#Add ∆ value to the plot
mtext("∆=0.89", side=3, line=0, at=-0.03, adj=-7.5, cex=1)

par(xpd=TRUE)
dev.off()


#==============================================================================#
#=========================== Baird's and Mountain =============================#
#Edit plotOverlap function
plotOverlap <- function(animal1, animal4) {
  par(ps=25, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  overlapPlotCustom(
    animal1, animal4
    ,main="Baird's and Mountain Circadian Overlap Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    ,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  
}

tiff(file = "CircadianOverlapBairdMountain.tif", width=13500, height=11000, res=1200)#will output a .tif file
#Set margin sizes -- might need to adjust these
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#run plot
plotOverlap(animal1, animal4)

#add legend
legend(x = "top", 
       legend = c("Baird's", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))

#Add ∆ value to the plot
mtext("∆=0.76", side=3, line=0, at=-0.03, adj=-7.5, cex=1)

par(xpd=TRUE)
dev.off()


#==============================================================================#
#=========================== Lowland and Malayan ==============================#
#Edit plotOverlap function -- change animal #'s and title
plotOverlap <- function(animal2, animal3) {
  par(ps=25, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  overlapPlotCustom(
    animal2, animal3
    ,main="Lowland and Malayan Circadian Overlap Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    ,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  
}

tiff(file = "CircadianOverlapLowlandMalayan.tif", width=13500, height=11000, res=1200)#will output a .tif file
#Set margin sizes -- might need to adjust these
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#run plot
plotOverlap(animal2, animal3)

#add legend
legend(x = "top", 
       legend = c("Lowland", "Malayan"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))

#Add ∆ value to the plot
mtext("∆=0.84", side=3, line=0, at=-0.03, adj=-7.5, cex=1)

par(xpd=TRUE)
dev.off()


#==============================================================================#
#========================== Lowland and Mountain ==============================#
#Edit plotOverlap function -- change animal #'s and title
plotOverlap <- function(animal2, animal4) {
  par(ps=25, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  overlapPlotCustom(
    animal2, animal4
    ,main="Lowland and Mountain Circadian Overlap Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    ,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  
}

tiff(file = "CircadianOverlapLowlandMountain.tif", width=13500, height=11000, res=1200)#will output a .tif file
#Set margin sizes -- might need to adjust these
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#run plot
plotOverlap(animal2, animal4)

#add legend
legend(x = "top", 
       legend = c("Lowland", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))

#Add ∆ value to the plot
mtext("∆=0.74", side=3, line=0, at=-0.03, adj=-7.5, cex=1)

par(xpd=TRUE)
dev.off()


#==============================================================================#
#========================== Malayan and Mountain ==============================#
#Edit plotOverlap function -- change animal #'s and title
plotOverlap <- function(animal3, animal4) {
  par(ps=25, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))
  overlapPlotCustom(
    animal3, animal4
    ,main="Malayan and Mountain Circadian Overlap Plot"
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    ,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  
}

tiff(file = "CircadianOverlapMalayanMountain.tif", width=13500, height=11000, res=1200)#will output a .tif file
#Set margin sizes -- might need to adjust these
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#run plot
plotOverlap(animal3, animal4)

#add legend
legend(x = "top", 
       legend = c("Malayan", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2))

#Add ∆ value to the plot
mtext("∆=0.72", side=3, line=0, at=-0.03, adj=-7.5, cex=1)

par(xpd=TRUE)
dev.off()

#done! :)