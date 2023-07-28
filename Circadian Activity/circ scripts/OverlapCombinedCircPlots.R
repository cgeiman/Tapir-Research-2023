###=============================================================================
# Date: June, 2023
# Author: Cate Geiman -- This code adapted from Amy Eppert's
# Title: "Combined overlap circadian plots with coefficient of overlap"
# Description: This code takes solar data for each of the four tapir species
#   and creates kernel density plots to show the circadian activity 
#   patterns of each. In this code, each of these four plots is combined as a 
#   matrix into one plot. The coefficient of overlap (∆) is determined to better
#   understand similarity between patterns.
# Output: .tif
###=============================================================================


#=========Combined overlap circadian plots with coefficient of overlap=========#

#Import data
bairdSunData <- read.csv("SunData/bairdSunDataIndependent.csv")
lowlandSunData <- read.csv("SunData/lowlandSunData.csv")
malayanSunData <- read.csv("SunData/malayanSunData.csv")
mountainSunData <- read.csv("SunData/mountainSunData.csv")

#load required package
require(overlap)
require(bayestestR)

#Identify sizes
xaxisSize <- 0.6
yaxisSize <- 0.7
legendSize <- 0.8
legendplacement <- 1.5
textSize=0.7

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

#New plot function for combined plots
plotActivityOverlapCombined <- function(animal1, animal2, yAxis, letter) {
  
  par(ps=30, lwd=1.5, cex.axis=0.85)
  overlapPlotCustom(
    animal1, animal2,
    main="",
    rug=TRUE
    ,font.lab=2
    ,extend=NULL
    ,xlab=""
    ,ylab=""
    ,yaxt="n"
  )
  if(yAxis){
    axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=0) 
    axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=2, hadj=0.5) 
    axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=1) 
  }
  else{
    axis(1, at=c(0), labels=c(""), cex.axis=xaxisSize, font=2, hadj=0) 
    axis(1, at=c(6, 12, 18), labels=c("", "", ""), cex.axis=xaxisSize, font=2, hadj=0.5) 
    axis(1, at=c(24), labels=c(""), cex.axis=xaxisSize, font=2, hadj=1) 
  }
  
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=2)
  axis(2,at=axTicks(2)[c(2,length(axTicks(2))-1)],labels=axTicks(2)[c(2,length(axTicks(2))-1)],cex.axis=yaxisSize,font=2)
  mtext(letter,side=3,font=2,cex=0.5,at=5.5,line=0.4,xpd=TRUE,col="blue")
}

#Calculate coefficient of overlap (∆) --- these values will be added to the plot using mtext()
overlap(animal1, animal2) #Baird's and Lowland
overlap(animal1, animal3) #Baird's and Malayan
overlap(animal1, animal4) #Baird's and Mountain
overlap(animal2, animal3) #Lowland and Malayan
overlap(animal2, animal4) #Lowland and Mountain
overlap(animal3, animal4) #Malayan and Mountain


#===============================================================================
#----------------------------Create plots---------------------------------------

tiff(file = "CircadianOverlapCombined3x2.tif", width=13500, height=11000, res=1200)#will output a .tif file
#Set margin sizes
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#Create a matrix layout and add each graph
layout(matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=FALSE))
plotActivityOverlapCombined(animal1, animal2, yAxis=FALSE,letter="Baird's & Lowland")
legend(x = "top", y.intersp = 1.3,
       legend = c("Baird's", "Lowland"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2),
       cex = 0.7)
mtext("∆=0.81", side=3, line=0, at=-0.03, adj=-3.4, cex=0.8)
plotActivityOverlapCombined(animal1, animal3, yAxis=FALSE,letter="Baird's & Malayan")
legend(x = "top", y.intersp = 1.3,
       legend = c("Baird's", "Malayan"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2),
       cex = 0.7)
mtext("∆=0.89", side=3, line=0, at=-0.03, adj=-3.4, cex=0.8)
plotActivityOverlapCombined(animal1, animal4, yAxis=TRUE,letter="Baird's & Mountain")
legend(x = "top", y.intersp = 1.3,
       legend = c("Baird's", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2),
       cex = 0.7)
mtext("∆=0.76", side=3, line=0, at=-0.03, adj=-3.4, cex=0.8)
plotActivityOverlapCombined(animal2, animal3, yAxis=FALSE,letter="Lowland & Malayan")
legend(x = "top", y.intersp = 1.3,
       legend = c("Lowland", "Malayan"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2),
       cex = 0.7)
mtext("∆=0.84", side=3, line=0, at=-0.03, adj=-3.4, cex=0.8)
plotActivityOverlapCombined(animal2, animal4, yAxis=FALSE,letter="Lowland & Mountain")
legend(x = "top", y.intersp = 1.3,
       legend = c("Lowland", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2),
       cex = 0.7)
mtext("∆=0.74", side=3, line=0, at=-0.03, adj=-3.4, cex=0.8)
plotActivityOverlapCombined(animal3, animal4, yAxis=TRUE,letter="Malayan & Mountain")
legend(x = "top", y.intersp = 1.3,
       legend = c("Malayan", "Mountain"),
       col = c("black", "blue"),
       text.col = c("black", "blue"),
       lty = c(1, 2),
       cex = 0.7)
mtext("∆=0.72", side=3, line=0, at=-0.03, adj=-3.4, cex=0.8)
par(xpd=TRUE)

#Add overall axis labels
mtext(text = "Temporal Density", side=2, outer=TRUE,line=2,font=2)
mtext(text = "Time of Day", side=1, outer=TRUE,line=2,font=2)

par(xpd=TRUE)
dev.off()

#.tif file should now appear! Yay!