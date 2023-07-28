###=============================================================================
# Date June, 2023
# Author: Cate Geiman -- This code adapted from Amy Eppert's
# Title: "Density plots for nocturnal and diurnal activity based on lunar phase"
# Description: This code creates lunar data for the four tapir species using the 
#   suncalc package and from here we can create kernel density plots. The plots 
#   in this code include single-species nocturnal and diurnal single plots, 
#   single-species nocturnal/diurnal overlap plots, two-species overlap 
#   plots for nocturnal activity, and combined/latticed plots.
# Output: .tif/jpeg
###=============================================================================


#===============================================================================
#----Density plots for nocturnal and diurnal activity based on lunar phase🌙----
#===============================================================================


#packages
library(readr)
require(overlap)
require(suncalc)
require(astroFns)
require(maptools)
require(bayestestR)
require(ggplot2)
require(hms)

#Import datasets
bairdDat <- read.csv("lunar data/Master(5.31.22).csv") #Baird's
lowlandDat <- read.csv("lunar data/lowlandCombined.csv") #Lowland
malayanDat <- read.csv("lunar data/TapirDataMalaysia_2009-2011.csv") #Malayan
mountainDat <- read.csv("lunar data/MOUNTAIN_Data Formatting - Global Tapir Project_JLM_final.xlsx - Data (1).csv") #Mountain

#change time to hms for lowland and malayan 
lowlandDat["hour"] <- format(as.POSIXct(lowlandDat$hour, format="%H:%M"), "%H:%M:%S")
malayanDat["Time"] <- format(as.POSIXct(malayanDat$Time, format="%H:%M"), "%H:%M:%S")
##Subset datasets so they have correct species and independent records (if relevant/needed)
bairdDatind <- subset(bairdDat[bairdDat$Independent == "Yes",]) #subset for independent records
bairdDat <- subset(bairdDatind[bairdDatind$Common == "Baird's Tapir",]) #subset for Baird's

lowlandDat <- subset(lowlandDat[lowlandDat$species == "tapir",]) #subset for tapir

#Create LunarDate column
bairdDat["LunarDate"] <- as.Date(bairdDat$Date, "%m/%d/%Y")
lowlandDat["LunarDate"] <- as.Date(lowlandDat$date, "%d/%m/%Y")
malayanDat["LunarDate"] <- as.Date(malayanDat$Date, "%m/%d/%Y")
mountainDat["LunarDate"] <- as.Date(mountainDat$Date, "%m/%d/%Y")

#Create Lunar column using 'getMoonIllumination' function to get lunar phase based on date
bairdDat["Lunar"] <- (getMoonIllumination(bairdDat$LunarDate, "phase")[,2])*2*pi
lowlandDat["Lunar"] <- (getMoonIllumination(lowlandDat$LunarDate, "phase")[,2])*2*pi
malayanDat["Lunar"] <- (getMoonIllumination(malayanDat$LunarDate, "phase")[,2])*2*pi
mountainDat["Lunar"] <- (getMoonIllumination(mountainDat$LunarDate, "phase")[,2])*2*pi

#Create Time.In.Radians column by getting the time back to hms for each species (if needed), then convert to radians
#(Baird's already had time in radians column)
lowlandDat["Time.In.Radians"] <- hms2rad(lowlandDat$hour) #converting time to radians

#malay <- read.csv("lunar data/TapirDataMalaysia_2009-2011.csv")  ###don't need this
#mount <- read.csv("lunar data/MOUNTAIN_Data Formatting - Global Tapir Project_JLM_final.xlsx - Data (1).csv")  ###don't need this

#malayanDat["Time"] <- malay$Time #putting Time back as hms from original dataset  ###don't need this
malayanDat["Time.In.Radians"] <- hms2rad(malayanDat$Time) #converting time to radians

#mountainDat["Time"] <- mount$Time #putting Time back as hms from original dataset  ###don't need this
mountainDat["Time.In.Radians"] <- hms2rad(mountainDat$Time) #converting time to radians

#Set date as posix format for sunTime function
bairdDat$Date <- as.POSIXct(bairdDat$Date, format = "%m/%d/%Y", tz = "America/Costa_Rica") #sets to posix format for sunTime function with timezone set to Costa Rica
lowlandDat$date <- as.POSIXct(lowlandDat$date, format = "%d/%m/%Y", tz = "America/Porto_Velho") #sets to posix format for sunTime function with timezone set to Porto Velho, Brazil
malayanDat$Date <- as.POSIXct(malayanDat$Date, format = "%m/%d/%Y", tz = "Asia/Kuala_Lumpur") #sets to posix format for sunTime function with timezone set to Kuala Lumpur, Malaysia
mountainDat$Date <- as.POSIXct(mountainDat$Date, format = "%m/%d/%Y", tz = "America/Lima") #sets to posix format for sunTime function with timezone set to Lima, Peru

#Create sunRad columns using 'sunTime' function to obtain solar data for sunset/sunrise times based on lat/lon
bairdDat["sunRad"] <- sunTime(bairdDat$Time.In.Radians, bairdDat$Date, 
                              sp::SpatialPoints(matrix(c(bairdDat$Longitude, bairdDat$Latitude), ncol = 2),
                                                proj4string=sp::CRS("+proj=longlat +datum=WGS84"))) #calculates time in radians for time of day using individual coordinates to find exact sunrise and sunset

#~~lowland lat/long were characters so they we converted to numeric using 'as.numeric'
lowlandDat["sunRad"] <- sunTime(lowlandDat$Time.In.Radians, lowlandDat$date, 
                              sp::SpatialPoints(matrix(c(as.numeric(lowlandDat$long), as.numeric(lowlandDat$lat)), ncol = 2),
                                                proj4string=sp::CRS("+proj=longlat +datum=WGS84"))) #calculates time in radians for time of day using individual coordinates to find exact sunrise and sunset

malayanDat["sunRad"] <- sunTime(malayanDat$Time.In.Radians, malayanDat$Date, 
                              sp::SpatialPoints(matrix(c(malayanDat$Longitd, malayanDat$Latitud), ncol = 2),
                                                proj4string=sp::CRS("+proj=longlat +datum=WGS84"))) #calculates time in radians for time of day using individual coordinates to find exact sunrise and sunset

mountainDat["sunRad"] <- sunTime(mountainDat$Time.In.Radians, mountainDat$Date, 
                              sp::SpatialPoints(matrix(c(mountainDat$Longitd, mountainDat$Latitud), ncol = 2),
                                                proj4string=sp::CRS("+proj=longlat +datum=WGS84"))) #calculates time in radians for time of day using individual coordinates to find exact sunrise and sunset


#-------------------------------------------------------------------------------
#Single species density plots for NOCTURNAL activity based on lunar phase-------

#Create a subset with Lunar data and nighttime
bairdNightActive <- subset(
  bairdDat$Lunar,
  (bairdDat$sunRad <= pi/2 | bairdDat$sunRad >= 3*pi/2) #interval of nighttime
)

lowlandNightActive <- subset(
  lowlandDat$Lunar,
  (lowlandDat$sunRad <= pi/2 | lowlandDat$sunRad >= 3*pi/2) #interval of nighttime
)

malayanNightActive <- subset(
  malayanDat$Lunar,
  (malayanDat$sunRad <= pi/2 | malayanDat$sunRad >= 3*pi/2) #interval of nighttime
)

mountainNightActive <- subset(
  mountainDat$Lunar,
  (mountainDat$sunRad <= pi/2 | mountainDat$sunRad >= 3*pi/2) #interval of nighttime
)

#Find n (sample size)
length(bairdNightActive) #2082
length(lowlandNightActive) #97
length(malayanNightActive) #678
length(mountainNightActive) #57

#Create the density plots for each species (.tif output)
#Baird's
tiff(file = "BairdNocturnalLunar.tif", width=10000, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
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
mtext("n=2082", side=3, line=0, at=-0.03, adj=-7, cex=1)
par(xpd=TRUE)
dev.off()

#Lowland
tiff(file = "LowlandNocturnalLunar.tif", width=10000, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
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
mtext("n=97", side=3, line=0, at=-0.03, adj=-11, cex=1)
par(xpd=TRUE)
dev.off()

#Malayan
tiff(file = "MalayanNocturnalLunar.tif", width=10000, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
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
mtext("n=678", side=3, line=0, at=-0.03, adj=-8.5, cex=1)
par(xpd=TRUE)
dev.off()

#Mountain    
tiff(file = "MountainNocturnalLunar.tif", width=10000, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
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
mtext("n=57", side=3, line=0, at=-0.03, adj=-11, cex=1)
par(xpd=TRUE)
dev.off()  


#-------------------------------------------------------------------------------
##Single species density plots for DIURNAL activity based on lunar phase--------
#this is assuming lunar data column has been created and time has been converted to sunRad for each data set

#Create a subset with Lunar data and daytime
bairdDayActive <- subset(
  bairdDat$Lunar,
  (bairdDat$sunRad >= pi/2 & bairdDat$sunRad <= 3*pi/2) #interval of daytime
)

lowlandDayActive <- subset(
  lowlandDat$Lunar,
  (lowlandDat$sunRad >= pi/2 & lowlandDat$sunRad <= 3*pi/2) #interval of daytime
)

malayanDayActive <- subset(
  malayanDat$Lunar,
  (malayanDat$sunRad >= pi/2 & malayanDat$sunRad <= 3*pi/2) #interval of daytime
)

mountainDayActive <- subset(
  mountainDat$Lunar,
  (mountainDat$sunRad >= pi/2 & mountainDat$sunRad <= 3*pi/2) #interval of daytime
)

#Find n (sample size)
length(bairdDayActive) #497
length(lowlandDayActive) #33
length(malayanDayActive) #131
length(mountainDayActive) #40

#Create the density plots for each species
#Baird's
tiff(file = "BairdDiurnalLunar.tif", width=10000, height=8000, res=1200)
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
par(xpd=TRUE)
dev.off() 

#Lowland
tiff(file = "LowlandDiurnalLunar.tif", width=10000, height=8000, res=1200)
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
par(xpd=TRUE)
dev.off() 

#Malayan
tiff(file = "MalayanDiurnalLunar.tif", width=10000, height=8000, res=1200)
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
par(xpd=TRUE)
dev.off() 

#Mountain    
tiff(file = "MountainDiurnalLunar.tif", width=10000, height=8000, res=1200)
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
par(xpd=TRUE)
dev.off()



#===============================================================================
#----------------------------Overlap plots--------------------------------------
#===============================================================================

#-------------------------------------------------------------------------------
#Single species overlap plots for nocturnal and diurnal activity----------------

#Calculate coefficient of overlap (∆)
overlap(bairdNightActive, bairdDayActive) #0.93
overlap(lowlandNightActive, lowlandDayActive) #0.89
overlap(malayanNightActive, malayanDayActive) #0.92
overlap(mountainNightActive, mountainDayActive) #0.90

#Plots (.tif output)
#Baird's
tiff(file = "BairdOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(bairdNightActive, bairdDayActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Baird's Tapir Lunar Activity",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Nocturnal (n=2082)", "Diurnal (n=497)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.93", side=3, line=0, at=-0.03, adj=-6.5, cex=1)

par(xpd=TRUE)
dev.off()

#Lowland
tiff(file = "LowlandOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(lowlandNightActive, lowlandDayActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Lowland Tapir Lunar Activity", 
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Nocturnal (n=97)", "Diurnal (n=33)"), 
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), 
       lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.82", side=3, line=0, at=-0.03, adj=-6.5, cex=1)
par(xpd=TRUE)
dev.off()

#Malayan
tiff(file = "MalayanOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(malayanNightActive, malayanDayActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Malayan Tapir Lunar Activity", 
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Nocturnal (n=678)", "Diurnal (n=131)"), 
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), 
       lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.92", side=3, line=0, at=-0.03, adj=-6.5, cex=1)
par(xpd=TRUE)
dev.off()

#Mountain
tiff(file = "MountainOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(mountainNightActive, mountainDayActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Mountain Tapir Lunar Activity", 
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Nocturnal (n=57)", "Diurnal (n=40)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), 
       lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.89", side=3, line=0, at=-0.03, adj=-6.5, cex=1)
par(xpd=TRUE)
dev.off()


#Example of another option for the legend if you want:
#     legend(x = "bottom", inset = 0.08,
#         legend = c("Nocturnal", "Diurnal"),
#         col = c("black", "blue"),
#         text.col = c("black", "blue"),
#         lty = c(1, 2))

#-------------------------------------------------------------------------------
#Two-species nocturnal overlap plots--------------------------------------------

#Calculate coefficient of overlap (∆)
overlap(bairdNightActive, lowlandNightActive) #0.84
overlap(bairdNightActive, malayanNightActive) #0.92
overlap(bairdNightActive, mountainNightActive) #0.85
overlap(lowlandNightActive, malayanNightActive) #0.81
overlap(lowlandNightActive, mountainNightActive) #0.82
overlap(malayanNightActive, mountainNightActive) #0.88

#Plots (.tif output)
#Baird's and Lowland
tiff(file = "BairdLowlandOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(bairdNightActive, lowlandNightActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Baird's and Lowland Tapir Lunar Activity",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Baird's (n=2082)", "Lowland (n=97)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.84", side=3, line=0, at=-0.03, adj=-7, cex=1)
par(xpd=TRUE)
dev.off()

#Baird's and Malayan
tiff(file = "BairdMalayanOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(bairdNightActive, malayanNightActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Baird's and Malayan Tapir Lunar Activity",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Baird's (n=2082)", "Malayan (n=678)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.92", side=3, line=0, at=-0.03, adj=-7, cex=1)
par(xpd=TRUE)
dev.off()

#Baird's and Mountain
tiff(file = "BairdMountainOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(bairdNightActive, mountainNightActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Baird's and Mountain Tapir Lunar Activity",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Baird's (n=2082)", "Mountain (n=57)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.85", side=3, line=0, at=-0.03, adj=-7, cex=1)
par(xpd=TRUE)
dev.off()

#Lowland and Malayan
tiff(file = "LowlandMalayanOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(lowlandNightActive, malayanNightActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Lowland and Malayan Tapir Lunar Activity",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Lowland (n=97)", "Malayan (n=678)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.81", side=3, line=0, at=-0.03, adj=-7, cex=1)
par(xpd=TRUE)
dev.off()

#Lowland and Mountain
tiff(file = "LowlandMountainOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(lowlandNightActive, mountainNightActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Lowland and Mountain Tapir Lunar Activity",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Lowland (n=97)", "Mountain (n=57)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.82", side=3, line=0, at=-0.03, adj=-7, cex=1)
par(xpd=TRUE)
dev.off()

#Malayan and Mountain
tiff(file = "MalayanMountainOverlapLunar.tif", width=10300, height=8000, res=1200)
par(ps=20, lwd=1.5, mar=c(6.5,8,5,5), cex.axis=0.7, mgp=c(5,1.5,0))
overlapPlot(malayanNightActive, mountainNightActive,
            xscale=NA, 
            xlab="Lunar Phase", 
            xaxt="n", 
            main = "Malayan and Mountain Tapir Lunar Activity",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
legend("topleft", legend = c("Malayan (n=678)", "Mountain (n=57)"),
       bty="n", pt.lwd=0.7, seg.len=3, col=c("black", "blue"), lty = c(1,2), cex=0.7,
       y.intersp=3, x.intersp = 0.4, inset=c(0,-0.1), ncol=2, lwd=3)
mtext("∆=0.88", side=3, line=0, at=-0.03, adj=-7, cex=1)
par(xpd=TRUE)
dev.off()


#===============================================================================
#----------------------------Combined Plots-------------------------------------
#===============================================================================

#-------------------------------------------------------------------------------
#combined plot for single species nocturnal + diurnal overlap plots-------------

#jpeg output
jpeg(filename = "LunarSingleOverlapCombined3.jpeg", width=7200, height=5000, res=600)#will output a .tif/jpeg file
#Set margin sizes
par(oma=c(3,4,1,0), mar=c(4,2,2,3))
#par(oma=c(4,5,1,0), mar=c(4,2,2,3))

#Create a matrix layout and add each graph
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=FALSE))

#Baird's
overlapPlot(bairdNightActive, bairdDayActive,
            xscale=NA, 
            xlab=" ", 
            xaxt="n", 
            yaxt = "n",
            main = " ",
            ylab = " ",
            cex.lab = 1.5,
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.5) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20), 
     labels = c(" ", "0.05", " ", "0.15", " "), font = 2, cex.axis = 1.5) #make y-axis bold/larger
legend("topleft", legend = c("Nocturnal (n=2082)", "Diurnal (n=497)"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.2,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.18), ncol=2, lwd=2, xpd = TRUE, text.font = 2)
mtext("∆=0.93", side=3, line=0, at=-0.03, adj=-6.7, cex=1.2, font=2)
mtext("Baird's", side=3, line=1.2, at=-0.03, adj=-2.3, cex=1.5, font=2)

#Malayan
overlapPlot(malayanNightActive, malayanDayActive,
            xscale=NA, 
            xlab=" ", 
            ylab = " ",
            xaxt="n", 
            yaxt ="n",
            main = " ", 
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.5) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20), 
     labels = c(" ", "0.05", " ", "0.15", " "), font = 2, cex.axis = 1.5) #make y-axis bold/larger
legend("topleft", legend = c("Nocturnal (n=678)", "Diurnal (n=131)"), 
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.2,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.18), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.92", side=3, line=0, at=-0.03, adj=-6.7, cex=1.2, font=2)
mtext("Malayan", side=3, line=1.2, at=-0.03, adj=-1.9, cex=1.5, font=2)

#Lowland
overlapPlot(lowlandNightActive, lowlandDayActive,
            xscale=NA, 
            xlab=" ", 
            ylab = " ",
            xaxt="n", 
            yaxt ="n",
            main = " ", 
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.5) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.5) #make y-axis bold/larger
legend("topleft", legend = c("Nocturnal (n=97)", "Diurnal (n=33)"), 
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.2,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.18), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.82", side=3, line=0, at=-0.03, adj=-6.7, cex=1.2, font=2)
mtext("Lowland", side=3, line=1.2, at=-0.03, adj=-1.9, cex=1.5, font=2)

#Mountain
overlapPlot(mountainNightActive, mountainDayActive,
            xscale=NA, 
            xlab=" ", 
            ylab = " ",
            xaxt="n", 
            yaxt ="n",
            main = " ", 
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.5) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.5) #make y-axis bold/larger
legend("topleft", legend = c("Nocturnal (n=57)", "Diurnal (n=40)"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.2,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.18), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.89", side=3, line=0, at=-0.03, adj=-6.7, cex=1.2, font=2)
mtext("Mountain", side=3, line=1.2, at=-0.03, adj=-1.7, cex=1.5, font=2)


#Add overall axis titles
mtext(expression(bold("Density")), side=2, outer=TRUE,line=1,font=2, cex=2)
mtext(expression(bold("Lunar Phase")), side=1, outer=TRUE,line=1,font=2, cex=2)

par(xpd=TRUE)
dev.off()



#-------------------------------------------------------------------------------
# combined plot for two-species nocturnal overlap plots-------------------------

#jpeg output
jpeg(filename = "LunarSpeciesOverlapCombined2.jpeg", width=6700, height=6000, res=600)#will output a .tif/jpeg file
#Set margin sizes
par(oma=c(3,4,1,0), mar=c(4,2,2,3))
#Create a matrix layout and add each graph
layout(matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=FALSE))
#Baird's and Lowland
overlapPlot(bairdNightActive, lowlandNightActive,
            xscale=NA, 
            xlab=" ", 
            xaxt="n", 
            yaxt="n",
            main = " ",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.8) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.8) #make y-axis bold/larger
legend("topleft", legend = c("Baird's (n=2082)", "Lowland (n=97)"), text.col = c("black", "blue"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.5,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.21), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.84", side=3, line=0, at=-0.03, adj=-7, cex=1.1, font=2)

#Baird's and Malayan
overlapPlot(bairdNightActive, malayanNightActive,
            xscale=NA, 
            xlab=" ", 
            xaxt="n", 
            yaxt="n",
            main = " ",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.8) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.8) #make y-axis bold/larger
legend("topleft", legend = c("Baird's (n=2082)", "Malayan (n=678)"),text.col = c("black", "blue"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.5,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.21), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.92", side=3, line=0, at=-0.03, adj=-7, cex=1.1, font=2)

#Baird's and Mountain
overlapPlot(bairdNightActive, mountainNightActive,
            xscale=NA, 
            xlab=" ", 
            xaxt="n", 
            yaxt="n",
            main = " ",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.8) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.8) #make y-axis bold/larger
legend("topleft", legend = c("Baird's (n=2082)", "Mountain (n=57)"),text.col = c("black", "blue"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.5,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.21), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.85", side=3, line=0, at=-0.03, adj=-7, cex=1.1, font=2)

#Lowland and Malayan
overlapPlot(lowlandNightActive, malayanNightActive,
            xscale=NA, 
            xlab=" ", 
            xaxt="n", 
            yaxt="n",
            main = " ",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.8) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.8) #make y-axis bold/larger
legend("topleft", legend = c("Lowland (n=97)", "Malayan (n=678)"),text.col = c("black", "blue"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.5,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.21), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.81", side=3, line=0, at=-0.03, adj=-7, cex=1.1, font=2)

#Lowland and Mountain
overlapPlot(lowlandNightActive, mountainNightActive,
            xscale=NA, 
            xlab=" ", 
            xaxt="n", 
            yaxt="n",
            main = " ",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.8) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.8) #make y-axis bold/larger
legend("topleft", legend = c("Lowland (n=97)", "Mountain (n=57)"),text.col = c("black", "blue"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.5,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.21), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.82", side=3, line=0, at=-0.03, adj=-7, cex=1.1, font=2)

#Malayan and Mountain
overlapPlot(malayanNightActive, mountainNightActive,
            xscale=NA, 
            xlab=" ", 
            xaxt="n", 
            yaxt="n",
            main = " ",
            extend = NULL, 
            xaxs = "i",
            rug=TRUE)
axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), 
     labels = c("New", "First Q", "Full", "Last Q", "New"), font = 2, cex.axis=1.8) #label x-axis with lunar phases
axis(side = 2, at = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25), 
     labels = c(" ", "0.05", " ", " ", "0.20", " "), font = 2, cex.axis = 1.8) #make y-axis bold/larger
legend("topleft", legend = c("Malayan (n=678)", "Mountain (n=57)"),text.col = c("black", "blue"),
       bty="n", pt.lwd=0.7, seg.len=2, col=c("black", "blue"), 
       lty = c(1,2), cex=1.5,
       y.intersp=3, x.intersp = 0.4, 
       inset=c(0,-0.21), ncol=2, lwd=2, xpd = TRUE, text.font=2)
mtext("∆=0.88", side=3, line=0, at=-0.03, adj=-7, cex=1.1, font=2)

#add overall axis titles
mtext(expression(bold("Density")), side=2, outer=TRUE,line=1,font=2, cex=2)
mtext(expression(bold("Lunar Phase")), side=1, outer=TRUE,line=1,font=2, cex=2)

par(xpd=TRUE)
dev.off()

