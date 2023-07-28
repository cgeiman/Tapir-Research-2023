#Individual Single Plots ----
#adjust these to change text size of labels on the axis and species name
xaxisSize = 1
yaxisSize = 1
textSize = 0.5

animalname1 <- "Puma concolor"
animal1 <- ind.data[ind.data$species == animalname1,"solar"] #will need to change this

#this creates a jpeg file and runs the plot functions. Or you can write them individually
filename <- paste0("~/Mooring Research/",animalname1, ".jpg") #can personalize filename to match your file location. Currently saving into working directory
jpeg(file = filename, width=10000, height=6000, res=800)#9200,6200
plotDensity(animalname1, animal1)


densityPlotCustom <- function (A, xscale = 24, xcenter = c("noon", "midnight"), add = FALSE, rug = FALSE, extend = NULL, n.grid = 128, kmax = 3, adjust = 1, lwd=2, nightShade=TRUE, xaxsSelect="i", ...) {
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
  defaultArgs <- list(main = deparse(substitute(A)), bty = "o", 
                      type = "l", xlab = "Time", ylab = "Density", lwd=2, font.axis=2, ylim = c(0, 
                                                                                                max(toPlot[, "y"])))
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


plotDensity <- function(animalname1, animal1) {
  par(ps=40, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))#3rd 3
  densityPlotCustom(
    animal1
    ,main=""
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
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  #axis(2,at=axTicks(2)[c(1,length(axTicks)-1)],labels=axTicks(2)[c(1,length(axTicks)-1)],cex.axis=yaxisSize,font=2)#only first and last
  mtext(paste(ind.data[ind.data$Species == animalname1, "Common"][1]," n=",length(animal1)),3,cex=textSize, adj=0,font=2)
  
  dev.off()
}


#Single Plots Combined ----
plotActivitySingleCombined <- function(anima1, yAxis, letter) {
  
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
  #mtext(paste0(ind.data[ind.data$Species==animalname1, "Common"][1]," (",animalname1,")"), side=3,adj=0, line=0.4, cex=1, font=2) 
  mtext(ind.data[ind.data$Species==animalname1, "Common"][1], side=3,adj=0, cex=textSize, font=2) #line=0.4
  #mtext(paste0("Day:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Day."],2),"%"), side=3,adj=1, line=0.4, cex=0.7, font=2)
  # mtext(paste0("Night:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Night."],2),"%"), side=3,adj=1, line=0.4, cex=0.7, font=2)
  # mtext(paste0("D/N:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Day."],0),"/",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Night."],0),"%"), side=3,adj=1, line=0.4, cex=0.7, font=2)
  #mtext(paste0("%D,N:",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Day."],0),",",round(proportions[proportions$Common.Species == ind.data[ind.data$Species==animalname1, "Common"][1],"Night."],0)), side=3,adj=1, line=0.4, cex=0.7, font=2)
  mtext(paste0("%Night = ",sprintf("%.2f",round(length(ind.data[ind.data$Species == animalname1 & ind.data$DNsun == "Night","sunRad"])/length(animal1),2))), side=3,adj=1,  cex=textSize, font=2) #line=0.4,
  mtext(letter,side=3,font=2,cex=0.85,at=-1,line=0.4,xpd=TRUE,col="blue")
}

xaxisSize <- 0.6
yaxisSize <- 0.7
legendSize <- 0.8
legendplacement <- 1.5
textSize=0.7

jpeg(file = "~/Mooring Research/Predators Combined2.jpg", width=10000, height=6000, res=800)
par(oma=c(4,5,1,0), mar=c(2,2,2,3))
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plotActivitySingleCombined("Panthera onca", yAxis=FALSE,letter="A")
plotActivitySingleCombined("Puma concolor", yAxis=FALSE,letter="B")
plotActivitySingleCombined("Leopardus pardalis", yAxis=TRUE,letter="C")
plotActivitySingleCombined("Canis latrans", yAxis=TRUE,letter="D")
par(xpd=TRUE)
mtext(text = "Temporal Density", side=2, outer=TRUE,line=2,font=2)
mtext(text = "Time of Day", side=1, outer=TRUE,line=2,font=2)
dev.off()
