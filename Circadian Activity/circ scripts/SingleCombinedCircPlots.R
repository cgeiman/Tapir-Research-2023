###=============================================================================
# Date: June, 2023
# Author: Cate Geiman -- This code adapted from Amy Eppert's
# Title: "Combined single species circadian plots with night shading and %Night"
# Description: This code takes solar data for each of the four tapir species
#   and creates kernel density plots to show the circadian activity 
#   patterns of each. In this code, each of these four plots is combined as a 
#   matrix into one plot. Percent Night is a value that is determined by 
#   calculating the ratio of records between sunset and sunrise (nighttime) 
#   compared to records in the daytime to get a better understanding of 
#   nocturnal/diurnal activity patterns.
# Output: .tif
###=============================================================================


#===============================================================================
#----Combined single species circadian plots with night shading and %Night------
#===============================================================================


#load required package
library(overlap)

#import datasets
library(readr)
bairdSunData <- read.csv("SunData/bairdSunDataIndependent.csv")
lowlandSunData <- read.csv("SunData/lowlandSunData.csv")
malayanSunData <- read.csv("SunData/malayanSunData.csv")
mountainSunData <- read.csv("SunData/mountainSunData.csv")

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

###Calculate % Night for each species (these values will be added to the plots later)
#Baird's
(length(bairdSunData$solar[bairdSunData$solar < (pi/2) | bairdSunData$solar > ((3*pi)/2)]))/
  (length(bairdSunData$solar))
#%Night: 0.82

#Lowland
(length(lowlandSunData$solar[lowlandSunData$solar < (pi/2) | lowlandSunData$solar > ((3*pi)/2)]))/
  (length(lowlandSunData$solar))
#%Night: 0.83

#Malayan
(length(malayanSunData$solar[malayanSunData$solar < (pi/2) | malayanSunData$solar > ((3*pi)/2)]))/
  (length(malayanSunData$solar))
#%Night: 0.88

#Mountain
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

#Create new plot function for combined plots
plotActivitySingleCombined <- function(animal1, yAxis, letter) {
  
  par(ps=30, lwd=1.5, cex.axis=0.85)
  densityPlotCustom(
    animal1,
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
  
  #axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=2)
  
  axis(2,at=axTicks(2)[c(2,length(axTicks(2))-1)],labels=axTicks(2)[c(2,length(axTicks(2))-1)],cex.axis=yaxisSize,font=2)
  
  mtext(letter,side=3,font=2,cex=0.5,at=2,line=0.4,xpd=TRUE,col="blue")
}

#Create a tiff file (this will be the output)
tiff(file = "CircadianCombinedw/%Night.tif", width=14500, height=8000, res=1200)
par(oma=c(4,5,1,0), mar=c(2,2,2,3))

#Create a matrix layout and add each graph
#Mountain was being weird so I just added it as a densityPlotCustom instead of plotActivitySingleCombined
# - (because of this the steps are all more manual for Mountain)
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE)) #create layout
plotActivitySingleCombined(animal1, yAxis=FALSE,letter="Baird's") #add first plot
mtext(expression(bold("%Night = 0.82")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5) #add %Night value
plotActivitySingleCombined(animal2, yAxis=FALSE,letter="Lowland") #add second plot
mtext(expression(bold("%Night = 0.83")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5) #add %Night value
plotActivitySingleCombined(animal3, yAxis=TRUE,letter="Malayan") #add third plot
mtext(expression(bold("%Night = 0.88")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5) #add %Night value
densityPlotCustom(
  animal4
  ,main=""
  ,xlab=""
  ,ylab=""
  ,rug=TRUE
  #,xaxt="n"#remove axis labels so rewrite with custom
  ,yaxt="n"#remove axis labels so rewrite with custom
)                                                       #add fourth plot (manually)
#Adding axis labels for Mountain
axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=0) 
axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=2, hadj=0.5) 
axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=2, hadj=1)

#For Mountain: add y-axis so all tickmarks and choose which labels
axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=2)#all tick marks
axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(4)),by=2)],cex.axis=yaxisSize,font=2)
par(xpd=TRUE)
#Add blue title and %Night for Mountain
mtext("Mountain",side=3,font=2,cex=0.5,at=2,line=0.4,xpd=TRUE,col="blue")
mtext(expression(bold("%Night = 0.63")), side=3, line=0, at=-0.03, adj=-2.5, cex=0.5) #add %Night value

#Add overall axis labels for whole combined plot
mtext(text = "Temporal Density", side=2, outer=TRUE,line=2,font=2)
mtext(text = "Time of Day", side=1, outer=TRUE,line=2,font=2)

par(xpd=TRUE)
dev.off() ###tiff file will now appear in folder
