###=================================================================================###
### Graphics of Predicted Occupancy for all Four Tapir Species                      ###        							                                	###
### 21 July 2023									                                                  ###
### Sarah Turcic - this code adapted by Cate Geiman        							            ###
###=================================================================================###


rm(list=ls())

#load required plots
require("ggpubr")
require("ggplot2")
require("unmarked")
require("grid")

#Define two themes to account for exterior and interior graphs/ axis labels
#Road will be the graphic with y-axis labels
theme_tapir_Road <- function() {
  
  font <- "sans"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      # add border 1)
      # panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
      # color background 2)
      panel.background = element_rect(fill = NA),
      
      # modify text, axis and colour 4) and 5)
      axis.text.x = element_text(colour = "black", face = "italic"),
      axis.text.y = element_text(colour = "black", face = "italic"),
      axis.title.y = element_text(colour = "black", angle = 90),
      axis.ticks = element_line(colour = "black"),
      
      # legend at the bottom 6)
      legend.position = "bottom",
      
      #Title
      plot.title = element_text(colour = "black", face = "bold" , size = 10, hjust = 0.5, vjust = 3),
      
      #line
      line = element_line(colour = "palegreen4")
      
    )
}

#Define Interior Theme - all other graphics (use this if you don't want y-axis labels on plot)
theme_tapir2 <- function() {
  
  font <- "sans"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      # add border 1)
      # panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
      # color background 2)
      panel.background = element_rect(fill = NA),
      
      # modify text, axis and colour 4) and 5)
      axis.text.x = element_text(colour = "black", face = "italic"),
      axis.text.y = element_blank(), #no y-axis labels for interior graphs
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_line(colour = "black"),
      
      
      # legend at the bottom 6)
      legend.position = "bottom",
      
      #Title
      plot.title = element_text(colour = "black", face = "bold" , size = 10, hjust = 0.5, vjust = 3),
      
      #line
      line = element_line(colour = "palegreen4")
      
    )
}


###################################################################################################
#-----------------------------Lowland Tapir--------------------------------------------------------
###################################################################################################
###Read in Variables
setwd("~/Documents/Mooring23/Lowland")

#read in tapir occurrance records
AM_tapir<- readRDS("Lowland/tapir_AM.rds")
#read in effort table
AM_eff<- readRDS("Lowland/eff_AM.rds")
#read in covariate table
AM_cov<- read.csv("Lowland/cv_t_AM_v2.csv")

#scale covariates
AM_cov2<- cbind(AM_cov[,c(1:4)], round(scale(AM_cov[,5:ncol(AM_cov)]),3))


#ensure rownames match
rownames(AM_tapir) == rownames(AM_eff)
rownames(AM_eff) == AM_cov2$Station


#Establish Unmarked Data Frame
AM_umf<- unmarkedFrameOccu(y=AM_tapir, siteCovs=AM_cov2, obsCovs=list(Eff=AM_eff))
#summary(AM_umf) #67 sites with detection

#-----------------------------------------------------------------------
#Running Models with Eff as survey covariate
AM_m.psi1.pEff      <- occu(~Eff~ 1, AM_umf) 
AM_m.psiElev.pEff   <- occu(~Eff~ Elev , AM_umf)
AM_m.psiRoad.pEff   <- occu(~Eff~ d.Road , AM_umf)  
AM_m.psiNDVI.pEff   <- occu(~Eff~ NDVI , AM_umf)
AM_m.psiTempmax.pEff<- occu(~Eff~ Avg.Max.Temp , AM_umf)
AM_m.psiPrecip.pEff <- occu(~Eff~ MAP , AM_umf)
AM_m.psiNPP.pEff    <- occu(~Eff~ NPP , AM_umf)
AM_m.psiHFI.pEff    <- occu(~Eff~ HFI , AM_umf)
summary(AM_m.psiHFI.pEff) #summary for our top model, this gives p-value
AM_m.psiTempmin.pEff<- occu(~Eff~ Avg.Min.Temp, AM_umf) 


####plot####

#top model is HFI, so that's the plot we're making

#=================================### HFI ###===================================
fivenum(AM_cov2$HFI) #min and max inform our x-axis labels
fivenum(AM_cov$HFI) #min and max inform our breaks

# Plotting top Unicovariate model (HFI)
Am_pred.psi.hfi<- predict(AM_m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(AM_cov$HFI))), "state")

#Plot the results
Am_HFI <- ggplot(Am_pred.psi.hfi, aes(x = sort(scale(AM_cov$HFI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "gray23") + #Confidence intervals
  geom_path(linewidth = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.068", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+ #add p-value to plot
  labs(x = "Human Footprint Index", y = "Occupancy probability (ψ)") + # axis labels
  ggtitle("Lowland Tapir")+
  theme_tapir_Road() +
  scale_x_continuous(name = "Human Footprint Index", 
                     breaks = c(-0.931, 0.9044, 2.7400, 4.5757), #based on max and min values, these help with scaling the plot
                     labels = c(0, 3, 6, 9))+ #based on max and min values, these help with formatting the plot
  coord_cartesian(ylim = c(0,1))

#adjust the plot margins and label sizes
Am_HFI <- Am_HFI + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                         axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                         axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                         axis.text = element_text(size = 15),
                         plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

Am_HFI

#create a jpeg of the plot
jpeg(filename = "LowlandHFIPlot.jpeg", width=5000, height=3000, res=600)
Am_HFI
par(xpd=TRUE)
dev.off()

###################################################################################################
#-----------------------------Baird's Tapir--------------------------------------------------------
###################################################################################################

setwd("~/Downloads/CR")

#Read in data
#occurance recs
CR_tapir<- readRDS("Baird's/tapir_CR.rds") 
#effort table
CR_eff<- readRDS("Baird's/eff_CR.rds")
#covariates
CR_cov<- read.csv("Baird's/cv_t3.csv")

#ensure rownames match
rownames(CR_tapir) == rownames(CR_eff)
rownames(CR_eff) == CR_cov2$Station

#Establish Unmarked Data Frame---------------------------------------------------
CR_umf<- unmarkedFrameOccu(y=CR_tapir, siteCovs= as.data.frame(scale(CR_cov[,-c(1:5)])), obsCovs=list(Eff=CR_eff))
#summary(CR_umf)

#-----------------------------------------------------------------------
#Running Models with Eff as survey covariate
CR_m.psi1.pEff		    <- occu(~Eff~ 1, CR_umf) 
CR_m.psiElev.pEff	    <- occu(~Eff~ Elev , CR_umf)
CR_m.psiRoad.pEff	    <- occu(~Eff~ d.Road , CR_umf)
summary(CR_m.psiRoad.pEff) #summary for our top model, this gives p-value
CR_m.psiTempmax.pEff  <- occu(~Eff~ Avg.Max.Temp, CR_umf)
CR_m.psiTempmin.pEff  <- occu(~Eff~ Avg.Min.Temp, CR_umf) 
CR_m.psiNDVI.pEff	    <- occu(~Eff~ NDVI, CR_umf)
CR_m.psiPrecp.Eff     <- occu(~Eff~ Precip, CR_umf)
CR_m.psiNPP.pEff	    <- occu(~Eff~ NPP ,CR_umf)
CR_m.psiHFI.pEff	    <- occu(~Eff~ HFI ,CR_umf) 
#-----------------------------------------------------------------------

###plot###

#distance to road is the top model for Baird's so that's what we'll be plotting

#================================### d.Road ###=================================
#distance to road
fivenum(CR_cov$d.Road)
fivenum(CR_cov2$d.Road)

# Plotting top Unicovariate model (d.Road)
CR_pred.psi.road<- predict(CR_m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(CR_cov$d.Road))), "state")

#Plot the results
CR_Road <- ggplot(CR_pred.psi.road, aes(x = sort(scale(CR_cov$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "honeydew4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+ #add p-value to plot
  #labs(x = "Distance to Road", y = "Occupancy probability") +
  ylab("Occupancy probability (ψ)") + # axis labels
  ggtitle("Baird's Tapir")+
  theme_tapir_Road() +
  scale_x_continuous(name = "Distance to Road (m)", n.breaks = 10, 
                     breaks = c(-1.0909, -0.3066, 0.4937, 1.29397, 2.0942, 2.8946), #based on max and min values, these help with scaling
                     labels = c(100, 5000, 10000, 15000, 20000, 25000))+ #based on max and min values, these help with formatting the plot
  coord_cartesian(ylim = c(0,1))

#adjust the plot margins and label sizes
CR_Road <- CR_Road + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
              axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
              axis.title.y = element_text(size=20, face = "bold", vjust = 3),
              axis.text = element_text(size = 15),
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
CR_Road

#create a jpeg of the plot
jpeg(filename = "Baird'sOccupancyPlot_blank.jpeg", width=5000, height=3000, res=600)
CR_Road
par(xpd=TRUE)
dev.off()


###################################################################################################
#-----------------------------Mountain Tapir-------------------------------------------------------
###################################################################################################

###Read in Variables
setwd("~/Downloads/Mountain")

######Read in Tapir table and Effort###########
Mt_tapir<- readRDS("Mountain/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
Mt_eff<- readRDS("Mountain/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
Mt_cov<- read.csv("Mountain/Mt_T_Covs4.csv")
#MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/MT_covs.csv", comment.char = "#")

Mt_cov <- Mt_cov[,-1] 


#Establish Unmarked Data Frame#-------------------------------------------------
MT_umf<- unmarkedFrameOccu(y=MT_tapir[,-1], siteCovs= as.data.frame(scale(MT_cov[,-c(1:5)])), obsCovs=list(Eff=MT_eff[,-1]))

#-----------------------------------------------------------------------
#Running Models with Eff as survey covariate
MT_m.psi1.pEff       <- occu(~Eff~ 1, MT_umf)  # Eff Model
MT_m.psiElev.pEff    <- occu(~Eff~ Elev, Mt_umf)
summary(MT_m.psiElev.pEff) #summary for our top model, this gives p-value
MT_m.psiPrec.pEff    <- occu(~Eff~ Precip, MT_umf)
MT_m.psiRoad.pEff    <- occu(~Eff~ d.Road, MT_umf)
MT_m.psiTempmax.pEff <- occu(~Eff~ AvgMaxTemp, MT_umf) 
summary(MT_m.psiTempmax.pEff) #summary for our top model, this gives p-value
MT_m.psiNDVI.pEff    <- occu(~Eff~ NDVI, MT_umf) 
MT_m.psiTempmin.pEff <- occu(~Eff~ AvgMinTemp, MT_umf)
summary(MT_m.psiTempmin.pEff) #summary for our top model, this gives p-value
MT_m.psiNPP.pEff     <- occu(~Eff~ NPP, MT_umf)
#-----------------------------------------------------------------------

###plots###
#Elevation, AvgMaxTemp, and AvgMinTemp are the top models, so those will be the plots

#==============================### Elevation ###================================
#Elev
fivenum(Mt_cov$Elev)
fivenum(scale(Mt_cov$Elev))
# Plotting top Unicovariate model (Elev)
Mt_pred.psi.elev<- predict(MT_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Mt_cov$Elev))), "state")

#Plot the results
Mt_Elev <- ggplot(Mt_pred.psi.elev, aes(x = sort(scale(Mt_cov$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.23", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability (ψ)") + # axis labels
  ggtitle("Mountain Tapir")+
  labs(x = "Elevation (m)", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "Elevation (m) ", 
                     breaks = c(-1.6228, -0.7499, 0.123, 0.9959, 1.8688), #based on max and min values, these help with scaling
                     labels = c(1600, 2100, 2600, 3100, 3600))+ #based on max and min values, these help with formatting
  coord_cartesian(ylim = c(0,1))

#adjust the plot margins and label sizes
Mt_Elev <- Mt_Elev + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                axis.text = element_text(size = 15),
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

Mt_Elev

#create a jpeg of the plot
jpeg(filename = "MountainElevPlot.jpeg", width=5000, height=3000, res=600)
Mt_Elev
par(xpd=TRUE)
dev.off()

#============================### Avg Max Temp ###===============================
#Max Temp
fivenum(Mt_cov$AvgMaxTemp)
fivenum(scale(Mt_cov$AvgMaxTemp))

# Plotting top Unicovariate model (MaxTemp)
Mt_pred.psi.maxTemp<- predict(MT_m.psiTempmax.pEff, newdata= data.frame(AvgMaxTemp= sort(scale(Mt_cov$AvgMaxTemp))), "state")

#Plot the results
Mt_Tempmax <- ggplot(Mt_pred.psi.maxTemp, aes(x = sort(scale(Mt_cov$AvgMaxTemp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "indianred4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.12", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  labs(x = "Avg Max Temp", y = "Occupancy probability (ψ)") + # axis labels
  ggtitle("Mountain Tapir")+
  labs(x = "Avg Max Temp (°C)", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "Avg Max Temp (°C)", 
                     breaks = c(-1.9119, -1.0616, -0.2112, 0.63917, 1.4895), #based on max and min values, these help with scaling
                     labels = c(16, 18.25, 20.50, 22.75, 25))+ #based on max and min values, these help with formatting
  coord_cartesian(ylim = c(0,1))

#adjust the plot margins and label sizes
Mt_Tempmax <- Mt_Tempmax + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                   axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                   axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                   axis.text = element_text(size = 15),
                   plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

Mt_Tempmax

#create a jpeg of the plot
jpeg(filename = "MountainMaxTempPlot.jpeg", width=5000, height=3000, res=600)
Mt_Tempmax
par(xpd=TRUE)
dev.off()

#=============================### Avg Min Temp ###==============================
#Min Temp
fivenum(Mt_cov$AvgMinTemp)
fivenum(scale(Mt_cov$AvgMinTemp))

# Plotting top Unicovariate model (Min Temp)
Mt_pred.psi.minTemp<- predict(MT_m.psiTempmin.pEff, newdata= data.frame(AvgMinTemp= sort(scale(Mt_cov$AvgMinTemp))), "state")

#Plot the results
Mt_Tempmin <- ggplot(Mt_pred.psi.minTemp, aes(x = sort(scale(Mt_cov$AvgMinTemp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "steelblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.13", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  labs(x = "Avg Min Temp", y = "Occupancy probability (ψ)") + # axis labels
  ggtitle("Mountain Tapir")+
  labs(x = "Avg Min Temp ", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "Avg Min Temp (°C)", 
                     breaks = c(-2.0023, -1.33325, -0.4969, 0.3395, 1.1758), #based on max and min values, these help with scaling
                     labels = c(5, 7, 9, 11, 13))+ #based on max and min values, these help with formatting
  coord_cartesian(ylim = c(0,1))

#adjust the plot margins and label sizes
Mt_Tempmin <- Mt_Tempmin + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                   axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                   axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                   axis.text = element_text(size = 15),
                   plot.title = element_text(size = 20, face = "bold", hjust = 0.5))


Mt_Tempmin

#create a jpeg of the plot
jpeg(filename = "MountainMinTempPlot.jpeg", width=5000, height=3000, res=600)
Mt_Tempmin
par(xpd=TRUE)
dev.off()

#=============================### combined! ###=================================
#create a combined plot (with all 3 top model graphs on it)
commonplotMT <- ggarrange(Mt_Tempmin, Mt_Tempmax, Mt_Elev,
                        labels = c("A", "B", "C"),
                        ncol = 1, nrow = 3)
#add title to combined plot
commonplotMT <- annotate_figure(commonplotMT, top = text_grob("Mountain Tapir", face = "bold", size = 25))

commonplotMT

#create jpeg of combined plot
jpeg(filename = "MountainCombinedOccupancyPlot2.jpeg", width=5000, height=9000, res=600)
commonplotMT
par(xpd=TRUE)
dev.off()


###################################################################################################
#-----------------------------Malayan Tapir-------------------------------------------------------
###################################################################################################

#Read in Variables
setwd("~/Downloads/Malayan")
#read in tapir occurance records
MA_tapir<- readRDS("Malayan/Collapsed_Capture_Malayan_Tapir.rds") 

#read in effort table
MA_eff<- readRDS("Malayan/Effort_Malayan_Tapir.rds")

#read in covariate table
MA_cov<- read.csv("Malayan/Ma_T_Final_Covs.csv")


#Establish Unmarked Data Frame#-------------------------------------------------
MA_umf<- unmarkedFrameOccu(y=MA_tapir[,-1], siteCovs= as.data.frame(scale(MA_cov[,-c(1:5)])), obsCovs=list(Eff=MA_eff[,-1]))


#-----------------------------------------------------------------------
#Running Models with Eff as survey covariate
MA_m.psi1.pEff      <- occu(~Eff ~1, MA_umf)  # Eff Model
MA_m.psiElev.pEff   <- occu(~Eff ~Elev, MA_umf)
summary(MA_m.psiElev.pEff) #summary for our top model, this gives p-value
MA_m.psiPrec.pEff   <- occu(~Eff ~Precip, MA_umf)
summary(MA_m.psiPrec.pEff) #summary for our top model, this gives p-value
MA_m.psiRoad.pEff <- occu(~Eff ~d.Road, MA_umf)
MA_m.psiTempmax.pEff<- occu(~Eff ~AvgMaxTemp, MA_umf) 
summary(MA_m.psiTempmax.pEff) #summary for our top model, this gives p-value
MA_m.psiNDVI.pEff   <- occu(~Eff ~NDVI, MA_umf) 
summary(MA_m.psiNDVI.pEff) #summary for our top model, this gives p-value
MA_m.psiTempmin.pEff<- occu(~Eff ~AvgMinTemp, MA_umf)
summary(MA_m.psiTempmin.pEff) #summary for our top model, this gives p-value
MA_m.psiHFI.pEff    <- occu(~Eff ~HFI, MA_umf)
MA_m.psiNPP.pEff    <- occu(~Eff ~NPP, MA_umf)
#-----------------------------------------------------------------------

###plots###
#Elevation, Precipitation, NDVI, AvgMaxTemp, and AvgMinTemp are the top models, so those will be the plots

#=============================### Elevation ###=================================
#Elev
fivenum(MA_sc_t$Elev)
fivenum(scale(MA_sc_t2$Elev))

# Plotting top Unicovariate model (Elev)
MA_pred.psi.elev<- predict(MA_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(MA_sc_t$Elev))), "state")

#Plot the results
MA_Elev <- ggplot(MA_pred.psi.elev, aes(x = sort(scale(MA_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability") + # axis labels
  ggtitle("Malayan Tapir")+
  labs(x = "Elevation (m)", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "Elevation (m)", 
                     breaks = c(-1.408,-0.3392, 0.7299, 1.7990, 3.02089), #based on max and min values, these help with scaling
                     labels = c(300, 650, 1000, 1350, 1750))+ #based on max and min values, these help with formatting
  coord_cartesian(ylim = c(0,1))
#289 1763m
#-1.441897  3.060600
MA_Elev <- MA_Elev + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                           axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                           axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                           axis.text = element_text(size = 15),
                           plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

MA_Elev

#create a jpeg of the plot
jpeg(filename = "MalayanElevPlot_blank.jpeg", width=5000, height=3000, res=600)
MA_Elev
par(xpd=TRUE)
dev.off()

#===========================### Precipitation ###===============================
#Precip
fivenum(MA_sc_t$Precip)
fivenum(MA_sc_t2$Precip)

# Plotting top Unicovariate model (Precip)
MA_pred.psi.precip<- predict(MA_m.psiPrec.pEff, newdata= data.frame(Precip= sort(scale(MA_sc_t$Precip))), "state")

#Plot the results
MA_Precip <- ggplot(MA_pred.psi.precip, aes(x = sort(scale(MA_sc_t$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "steelblue3") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  ggtitle("Malayan Tapir")+
  labs(x = "Precipitation (mm)", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "Precipitation (mm)", 
                     breaks = c(-1.2355, -0.46797, 0.2996, 1.0672, 1.83476), #based on max and min values, these help with scaling
                     labels = c(190,  205,  220,  235,  250))+ #based on max and min values, these help with formatting
  coord_cartesian(ylim = c(0,1))
#-1.423178  2.005328
#186.3333 253.3333
MA_Precip <- MA_Precip + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                     axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                     axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                     axis.text = element_text(size = 15),
                     plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
MA_Precip

#create a jpeg of the plot
jpeg(filename = "MalayanPrecipPlot.jpeg", width=5000, height=3000, res=600)
MA_Precip
par(xpd=TRUE)
dev.off()

#================================### NDVI ###===================================
# NDVI
fivenum(MA_sc_t$NDVI)
fivenum(MA_sc_t2$NDVI)

# Plotting top Unicovariate model (NDVI)
MA_pred.psi.NDVI<- predict(MA_m.psiNDVI.pEff, newdata= data.frame(NDVI= sort(scale(MA_sc_t$NDVI))), "state")

#Plot the results
MA_NDVI <- ggplot(MA_pred.psi.NDVI, aes(x = sort(scale(MA_sc_t$NDVI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "seagreen") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.01", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  #labs(x = "HFI", y = "Occupancy probability") + # axis labels
  ggtitle("Malayan Tapir")+
  labs(x = "NDVI", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "NDVI", 
                     breaks = c(-3.131, -1.8765, -0.622, 0.6325, 1.887), #based on max and min values, these help with scaling
                     labels = c(0.21, 0.28, 0.34, 0.41, 0.48))+ #based on max and min values, these help with formatting
  coord_cartesian(ylim = c(0,1))
#-0.2090092  6.5705417
#0-7
MA_NDVI <- MA_NDVI + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                           axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                           axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                           axis.text = element_text(size = 15),
                           plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

MA_NDVI

#create a jpeg of the plot
jpeg(filename = "MalayanNDVIPlot.jpeg", width=5000, height=3000, res=600)
MA_NDVI
par(xpd=TRUE)
dev.off()

#============================### Avg Max Temp ###===============================
#MaxTemp
fivenum(MA_sc_t$AvgMaxTemp)
fivenum(MA_sc_t2$AvgMaxTemp)

# Plotting top Unicovariate model (AvgMaxTemp)
Ma_pred.psi.maxTemp<- predict(MA_m.psiTempmax.pEff, newdata= data.frame(AvgMaxTemp= sort(scale(MA_sc_t2$AvgMaxTemp))), "state")

#Plot the results
MA_MaxTemp <- ggplot(Ma_pred.psi.maxTemp, aes(x = sort(scale(MA_sc_t2$AvgMaxTemp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "indianred4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.0001", x = 0.55, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  ggtitle("Malayan Tapir")+
  labs(x = "Avg Max Temp (°C)", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "Avg Max Temp (°C)", n.breaks = 10, 
                     breaks = c(-2.928724, -1.4561, 0.016567, 1.489213), #based on max and min values, these help with scaling
                     labels = c(24, 26, 28, 30))+ #based on max and min values, these help with formatting
  coord_cartesian(ylim = c(0,1))

MA_MaxTemp <- MA_MaxTemp + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                   axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                   axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                   axis.text = element_text(size = 15),
                   plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
MA_MaxTemp

#create a jpeg of the plot
jpeg(filename = "MalayanMaxTempPlot_blank.jpeg", width=5000, height=3000, res=600)
MA_MaxTemp
par(xpd=TRUE)
dev.off()

#============================### Avg Min Temp ###===============================
#MinTemp
fivenum(MA_sc_t$AvgMinTemp)
fivenum(MA_sc_t2$AvgMinTemp)

# Plotting top Unicovariate model (AvgMinTemp)
Ma_pred.psi.minTemp<- predict(MA_m.psiTempmin.pEff, newdata= data.frame(AvgMinTemp= sort(scale(MA_sc_t2$AvgMinTemp))), "state")

#Plot the results
MA_MinTemp <- ggplot(Ma_pred.psi.minTemp, aes(x = sort(scale(MA_sc_t2$AvgMinTemp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "steelblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.55, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 20, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  ggtitle("Malayan Tapir")+
  labs(x = "Avg Min Temp (°C)", y = "Occupancy probability (ψ)") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = "Avg Min Temp (°C)", n.breaks = 10, 
                     breaks = c(-3.0066, -1.62998, -0.25336, 1.123), #based on max and min values, these help with scaling
                     labels = c(16, 18, 20, 22))+ #based on max and min values, these help with formatting
  theme(axis.text.y=element_text(size=15), axis.text.x=element_text(size=15)) +
  coord_cartesian(ylim = c(0,1))

MA_MinTemp <- MA_MinTemp + theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
                              axis.title.x = element_text(size = 20, face = "bold", vjust = -1),
                              axis.title.y = element_text(size=20, face = "bold", vjust = 3),
                              axis.text = element_text(size = 15),
                              plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

MA_MinTemp

#create a jpeg of the plot
jpeg(filename = "MalayanMinTempPlot_blank.jpeg", width=5000, height=3000, res=600)
MA_MinTemp
par(xpd=TRUE)
dev.off()

#=============================### combined! ###=================================
#create a combined plot (with all 5 top model graphs on it)
commonplotMA <- ggarrange(MA_NDVI, MA_MaxTemp, MA_MinTemp, MA_Elev, MA_Precip, 
                        labels = c("A", "B", "C", "D", "E"),
                        ncol = 1, nrow = 5)

#add title to combined plot
commonplotMA <- annotate_figure(commonplotMA, top = text_grob("Malayan Tapir", face = "bold", size = 25))

commonplotMA

#create a jpeg of the plot
jpeg(filename = "MalayanCombinedOccupancyPlot.jpeg", width=5000, height=15000, res=600)
commonplotMA
par(xpd=TRUE)
dev.off()


###################################################################################################
#-----------------------COMPLETE COMBINED PLOT FOR ALL SPECIES-------------------------------------
###################################################################################################

#create a combined plot for all 4 species
commonplot_tapir <- ggarrange(Am_HFI, CR_Road, Mt_Tempmin, Mt_Tempmax, Mt_Elev, MA_NDVI, MA_MaxTemp, MA_MinTemp, MA_Elev, MA_Precip, 
                          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
                          font.label = list(size = 25, color = "blue"),
                          ncol = 2, nrow = 5)

#add title to combined plot
commonplot_tapir <- annotate_figure(commonplot_tapir, top = text_grob(" ", face = "bold", size = 25))

commonplot_tapir

#create a jpeg of the plot
jpeg(filename = "TapirCombinedOccupancyPlot.jpeg", width=25000, height=6000, res=600)
commonplot_tapir
par(xpd=TRUE)
dev.off()

