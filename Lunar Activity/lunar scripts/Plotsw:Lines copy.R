###=============================================================================
# Date June, 2023
# Author: Cate Geiman -- This code adapted from Amy Eppert's
# Title: "Lunar phase plots with lines and percents"
# Description: Lunar plots for nocturnal and diurnal activity for each species, 
#   incorporating vertical lines and percents to section the number of 
#   observations for each plot
# Output: .tif
###=============================================================================


#===============================================================================
#-----------------Lunar phase plots with lines and percents🌙-------------------
#===============================================================================

#NOTE: see Lunar.R script for determining bairdNightActive object, etc.

#-------------------------------------------------------------------------------
#-----------------------------------BAIRD'S-------------------------------------

#NOCTURNAL----------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions from there
length(bairdNightActive)  
#2066

length(bairdNightActive[bairdNightActive > 0 & bairdNightActive < 0.7853982])
#253
length(bairdNightActive[bairdNightActive > 5.497787 & bairdNightActive < (2*pi)])
#302
(253+302)/2066
#27%

length(bairdNightActive[bairdNightActive > 0.7853982 & bairdNightActive < 2.356194])
#528
528/2066
#26%

length(bairdNightActive[bairdNightActive > 2.356194 & bairdNightActive < 3.926991])
#459
459/2066
#22%

length(bairdNightActive[bairdNightActive > 3.926991 & bairdNightActive < 5.497787])
#524
524/2066
#25%

#Plot
tiff(file = "BairdNocturnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(bairdNightActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Baird's Tapir Nocturnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(bairdNightActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("27%")), side=3, line=-1, at=-0.03, adj=-1.4, cex=1)
mtext(expression(bold("26%")), side=3, line=-1, at=-0.03, adj=-6.2, cex=1)
mtext(expression(bold("22%")), side=3, line=-1, at=-0.03, adj=-12.75, cex=1)
mtext(expression(bold("25%")), side=3, line=-1, at=-0.03, adj=-19.5, cex=1)

par(xpd=TRUE)
dev.off()


#DIURNAL------------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions from there
length(bairdDayActive)  
#513

length(bairdDayActive[bairdDayActive > 0 & bairdDayActive < 0.7853982])
#74
length(bairdDayActive[bairdDayActive > 5.497787 & bairdDayActive < (2*pi)])
#44
(74+44)/513
#23%

length(bairdDayActive[bairdDayActive > 0.7853982 & bairdDayActive < 2.356194])
#138
138/513
#27%

length(bairdDayActive[bairdDayActive > 2.356194 & bairdDayActive < 3.926991])
#120
120/513
#23%

length(bairdDayActive[bairdDayActive > 3.926991 & bairdDayActive < 5.497787])
#137
137/513
#27%

#Plot
tiff(file = "BairdDiurnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(bairdDayActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Baird's Tapir Diurnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(bairdDayActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("23%")), side=3, line=-1, at=-0.03, adj=-1.4, cex=1)
mtext(expression(bold("27%")), side=3, line=-1, at=-0.03, adj=-6.2, cex=1)
mtext(expression(bold("23%")), side=3, line=-1, at=-0.03, adj=-12.8, cex=1)
mtext(expression(bold("27%")), side=3, line=-1, at=-0.03, adj=-19.5, cex=1)

par(xpd=TRUE)
dev.off()


#-------------------------------------------------------------------------------
#----------------------------------LOWLAND--------------------------------------

#NOCTURNAL----------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions
length(lowlandNightActive)  

((length(lowlandNightActive[lowlandNightActive > 0 & lowlandNightActive < 0.7853982]))+length(lowlandNightActive[lowlandNightActive > 5.497787 & lowlandNightActive < (2*pi)]))/(length(lowlandNightActive))
'40%'

(length(lowlandNightActive[lowlandNightActive > 0.7853982 & lowlandNightActive < 2.356194]))/(length(lowlandNightActive)) 
'26%'

(length(lowlandNightActive[lowlandNightActive > 2.356194 & lowlandNightActive < 3.926991]))/(length(lowlandNightActive)) 
'20%'


(length(lowlandNightActive[lowlandNightActive > 3.926991 & lowlandNightActive < 5.497787]))/(length(lowlandNightActive)) 
'14%'

#Plot
tiff(file = "LowlandNocturnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(lowlandNightActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Lowland Tapir Nocturnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(lowlandNightActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("40%")), side=3, line=-1, at=-0.03, adj=-1.4, cex=1)
mtext(expression(bold("26%")), side=3, line=-1, at=-0.03, adj=-6.2, cex=1)
mtext(expression(bold("20%")), side=3, line=-1, at=-0.03, adj=-12.75, cex=1)
mtext(expression(bold("14%")), side=3, line=-1, at=-0.03, adj=-19.3, cex=1)

par(xpd=TRUE)
dev.off()


#DIURNAL------------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions from there
length(lowlandDayActive)  

((length(lowlandDayActive[lowlandDayActive > 0 & lowlandDayActive < 0.7853982]))+length(lowlandDayActive[lowlandDayActive > 5.497787 & lowlandDayActive < (2*pi)]))/(length(lowlandDayActive))
'15%'

(length(lowlandDayActive[lowlandDayActive > 0.7853982 & lowlandDayActive < 2.356194]))/(length(lowlandDayActive)) 
'48%'

(length(lowlandDayActive[lowlandDayActive > 2.356194 & lowlandDayActive < 3.926991]))/(length(lowlandDayActive)) 
'26%'

(length(lowlandDayActive[lowlandDayActive > 3.926991 & lowlandDayActive < 5.497787]))/(length(lowlandDayActive)) 
'11%'

#Plot
tiff(file = "LowlandDiurnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(lowlandDayActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Lowland Tapir Diurnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(lowlandDayActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("15%")), side=3, line=-1, at=-0.03, adj=-1.2, cex=1)
mtext(expression(bold("48%")), side=3, line=-1, at=-0.03, adj=-6.2, cex=1)
mtext(expression(bold("26%")), side=3, line=-1, at=-0.03, adj=-12.8, cex=1)
mtext(expression(bold("11%")), side=3, line=-1, at=-0.03, adj=-19.3, cex=1)

par(xpd=TRUE)
dev.off()


#-------------------------------------------------------------------------------
#--------------------------------MALAYAN----------------------------------------

#NOCTURNAL----------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions from there
length(malayanNightActive)  

((length(malayanNightActive[malayanNightActive > 0 & malayanNightActive < 0.7853982]))+length(malayanNightActive[malayanNightActive > 5.497787 & malayanNightActive < (2*pi)]))/(length(malayanNightActive))
'23%'

(length(malayanNightActive[malayanNightActive > 0.7853982 & malayanNightActive < 2.356194]))/(length(malayanNightActive)) 
'24%'

(length(malayanNightActive[malayanNightActive > 2.356194 & malayanNightActive < 3.926991]))/(length(malayanNightActive)) 
'26%'

(length(malayanNightActive[malayanNightActive > 3.926991 & malayanNightActive < 5.497787]))/(length(malayanNightActive)) 
'27%'

#Plot
tiff(file = "MalayanNocturnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(malayanNightActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Malayan Tapir Nocturnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(malayanNightActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("23%")), side=3, line=-1, at=-0.03, adj=-1.2, cex=1)
mtext(expression(bold("24%")), side=3, line=-1, at=-0.03, adj=-6.1, cex=1)
mtext(expression(bold("26%")), side=3, line=-1, at=-0.03, adj=-12.75, cex=1)
mtext(expression(bold("27%")), side=3, line=-1, at=-0.03, adj=-19.4, cex=1)

par(xpd=TRUE)
dev.off()


#DIURNAL------------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions from there
length(malayanDayActive)  

((length(malayanDayActive[malayanDayActive > 0 & malayanDayActive < 0.7853982]))+length(malayanDayActive[malayanDayActive > 5.497787 & malayanDayActive < (2*pi)]))/(length(malayanDayActive))
'22%'

(length(malayanDayActive[malayanDayActive > 0.7853982 & malayanDayActive < 2.356194]))/(length(malayanDayActive)) 
'23%'

(length(malayanDayActive[malayanDayActive > 2.356194 & malayanDayActive < 3.926991]))/(length(malayanDayActive)) 
'30%'

(length(malayanDayActive[malayanDayActive > 3.926991 & malayanDayActive < 5.497787]))/(length(malayanDayActive)) 
'25%'

#Plot
tiff(file = "MalayanDiurnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(malayanDayActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Malayan Tapir Diurnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(malayanDayActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("22%")), side=3, line=-1, at=-0.03, adj=-1.2, cex=1)
mtext(expression(bold("23%")), side=3, line=-1, at=-0.03, adj=-6.1, cex=1)
mtext(expression(bold("30%")), side=3, line=-1, at=-0.03, adj=-12.7, cex=1)
mtext(expression(bold("25%")), side=3, line=-1, at=-0.03, adj=-19.3, cex=1)

par(xpd=TRUE)
dev.off()


#-------------------------------------------------------------------------------
#----------------------------------MOUNTAIN-------------------------------------

#NOCTURNAL----------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions from there
length(mountainNightActive)  

((length(mountainNightActive[mountainNightActive > 0 & mountainNightActive < 0.7853982]))+length(mountainNightActive[mountainNightActive > 5.497787 & mountainNightActive < (2*pi)]))/(length(mountainNightActive))
'26%'

(length(mountainNightActive[mountainNightActive > 0.7853982 & mountainNightActive < 2.356194]))/(length(mountainNightActive)) 
'34%'

(length(mountainNightActive[mountainNightActive > 2.356194 & mountainNightActive < 3.926991]))/(length(mountainNightActive)) 
'21%'

(length(mountainNightActive[mountainNightActive > 3.926991 & mountainNightActive < 5.497787]))/(length(mountainNightActive)) 
'19%'

#Plot
tiff(file = "MountainNocturnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(mountainNightActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Mountain Tapir Nocturnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(mountainNightActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("26%")), side=3, line=-1, at=-0.03, adj=-1.15, cex=1)
mtext(expression(bold("34%")), side=3, line=-1, at=-0.03, adj=-6.2, cex=1)
mtext(expression(bold("21%")), side=3, line=-1, at=-0.03, adj=-12.7, cex=1)
mtext(expression(bold("19%")), side=3, line=-1, at=-0.03, adj=-19.3, cex=1)

par(xpd=TRUE)
dev.off()


#DIURNAL------------------------------------------------------------------------
#to get percents, determine lengths and calculate proportions from there
length(mountainDayActive)  

((length(mountainDayActive[mountainDayActive > 0 & mountainDayActive < 0.7853982]))+length(mountainDayActive[mountainDayActive > 5.497787 & mountainDayActive < (2*pi)]))/(length(mountainDayActive))
'30%'

(length(mountainDayActive[mountainDayActive > 0.7853982 & mountainDayActive < 2.356194]))/(length(mountainDayActive)) 
'38%'

(length(mountainDayActive[mountainDayActive > 2.356194 & mountainDayActive < 3.926991]))/(length(mountainDayActive)) 
'16%'

(length(mountainDayActive[mountainDayActive > 3.926991 & mountainDayActive < 5.497787]))/(length(mountainDayActive)) 
'16%'

#Plot
tiff(file = "MountainDiurnalLinesLunar.tif", width=12000, height=8000, res=1200)
densityPlot(mountainDayActive, 
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Mountain Tapir Diurnal Lunar Activity", 
            extend = NULL, 
            xaxs = "i")
rug(mountainDayActive, side = 1)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))

#add dotted lines at each desired point
abline(v=0.7853982, col="blue",lty = 2)
abline(v=2.356194, col="blue",lty = 2)
abline(v=3.926991, col="blue",lty = 2)
abline(v=5.497787, col="blue",lty = 2)

#add percents to sections
mtext(expression(bold("30%")), side=3, line=-1, at=-0.03, adj=-1.15, cex=1)
mtext(expression(bold("38%")), side=3, line=-1, at=-0.03, adj=-6.1, cex=1)
mtext(expression(bold("16%")), side=3, line=-1, at=-0.03, adj=-12.7, cex=1)
mtext(expression(bold("16%")), side=3, line=-1, at=-0.03, adj=-19.3, cex=1)

par(xpd=TRUE)
dev.off()

