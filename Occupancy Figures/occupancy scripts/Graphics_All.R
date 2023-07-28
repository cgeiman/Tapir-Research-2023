###=================================================================================###
### Complete Graphic Table of Predicted Occupancy for all Four Tapir Species        ###
### Honor's Project - 2023	        							                                	###
### 9 March 2023 											                                              ###
### Sarah Turcic        										                                        ###
###=================================================================================###


rm(list=ls())

#load libraries
library("ggpubr")
library("ggplot2")
library("unmarked")
library("grid")

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

#Define Interior Theme - all other graphics
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

############################################################################################################
##########################Lowland Tapir#####################################################################

###Read in Variables
setwd("~/Documents/Mooring23/Lowland")

#read in tapir occurance records
Am_tapir<- readRDS("tapir_AM.rds")

#read in effort table
Am_eff<- readRDS("eff_AM.rds")

#read in covariate table
Am_sc_t<- read.csv("cv_t_AM_v2.csv")

#scale covariates
Am_sc_t2<- cbind(Am_sc_t[,1:4], round(scale(Am_sc_t[,5:18]),3))


#ensure rownames match
rownames(Am_tapir) == rownames(Am_eff)
rownames(Am_eff) == Am_sc_t2$Station

###Establish Unmarked Data Frame

Am_umf<- unmarkedFrameOccu(y=Am_tapir, siteCovs=Am_sc_t2, obsCovs=list(Eff=Am_eff))
summary(Am_umf) #67 sites with detection


###Running Models

# Running Null model
Am_mod0 <- occu(~1~1, Am_umf)  # Null Model

# Running model with Eff as survey covariate
Am_m.psi1.pEff<- occu(~Eff~1, Am_umf) 

# Running unicovariate models
Am_m.psiElev.pEff<- occu(~Eff~Elev , Am_umf)
summary(Am_m.psiElev.pEff)
Am_m.psiRoad.pEff<- occu(~Eff~d.Road , Am_umf)   
summary(Am_m.psiRoad.pEff)
Am_m.psiPrecip.pEff<- occu(~Eff~Precip , Am_umf)
summary(Am_m.psiPrecip.pEff)
Am_m.psiNDVI.pEff<- occu(~Eff~NDVI , Am_umf)
summary(Am_m.psiNDVI.pEff)
Am_m.psiTemp.pEff<- occu(~Eff~Avg.Max.Temp , Am_umf)
summary(Am_m.psiTemp.pEff)

####plots####

### Distance to Road ###

Am_pred.psi.road<- predict(Am_m.psiRoad.pEff, newdata= data.frame(d.Road = sort(scale(Am_sc_t2$d.Road))), "state")

#Plot the results
Am_Road <- ggplot(Am_pred.psi.road, aes(x = sort(scale(Am_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") +
  #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.055", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Distance to Road (m)")+
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-1.021, -0.3995, 0.222, 0.8435, 1.465), labels = c(300, 25000, 50000, 75000, 100000))+
  coord_cartesian(ylim = c(0,1))
#-1.021 to 1.465
#324 - 106,043

#Elev
# Plotting top Unicovariate model (Elev)
Am_pred.psi.elev<- predict(Am_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Am_sc_t$Elev))), "state")

#Plot the results
Am_Elev <- ggplot(Am_pred.psi.elev, aes(x = sort(scale(Am_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.22", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Elevation (m)")+
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.45, -0.3033, 0.8433, 1.99), labels = c(50, 100, 150, 200))+
  coord_cartesian(ylim = c(0,1))
#-1.45 - 1.99
#48.14 - 204.00

#Precip
#Plotting top Unicovariate model (Precip)
Am_pred.psi.precip<- predict(Am_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Am_sc_t2$Precip))), "state")

#Plot the results
Am_Precip <- ggplot(Am_pred.psi.precip, aes(x = sort(scale(Am_sc_t2$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.20", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Precipitation (mm)")+
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.07, -0.379,  0.311,  1.00, 1.347), labels = c(120, 165, 210, 255, 300))+
  coord_cartesian(ylim = c(0,1))

#NDVI
#Plotting top Unicovariate model (Precip)
Am_pred.psi.NDVI<- predict(Am_m.psiNDVI.pEff, newdata= data.frame(NDVI= sort(scale(Am_sc_t2$NDVI))), "state")

#Plot the results
Am_NDVI <- ggplot(Am_pred.psi.NDVI, aes(x = sort(scale(Am_sc_t2$NDVI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "plum4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.81", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Precipitation (mm)")+
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-2.854, -1.8055, -0.757, 0.2915, 1.34), labels = c(0.037, 0.144, 0.251, 0.358, 0.465))+
  coord_cartesian(ylim = c(0,1))

#Temp
#Plotting top Unicovariate model (Precip)
Am_pred.psi.temp<- predict(Am_m.psiTemp.pEff, newdata= data.frame(Avg.Max.Temp= sort(scale(Am_sc_t2$Avg.Max.Temp))), "state")

#Plot the results
Am_Temp <- ggplot(Am_pred.psi.temp, aes(x = sort(scale(Am_sc_t2$Avg.Max.Temp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "steelblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.11", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  labs(x = " ", y = " ") + # axis labels
  #ggtitle("Precipitation (mm)")+
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-0.95, 0.176, 1.302, 2.428, 3.554), labels = c(31.8, 32.0, 32.2, 32.4, 32.6))+
  coord_cartesian(ylim = c(0,1))






############################################################################################################
##########################Baird's Tapir#####################################################################

setwd("~/Downloads/CR")

#load in data
CR_tapir_t<- readRDS("tapir_CR.rds")
CR_eff_t<- readRDS("eff_CR.rds")
CR_sc_t<- read.csv("cv_t3.csv")

CR_sc_t <- CR_sc_t[,-1]

#scale
CR_sc_t2<- cbind(CR_sc_t[,1:4], round(scale(CR_sc_t[,5:17]),3))

rownames(CR_tapir_t) == rownames(CR_eff_t)
rownames(CR_eff_t) == CR_sc_t$Station

CR_umf<- unmarkedFrameOccu(y=CR_tapir_t, siteCovs=CR_sc_t2, obsCovs=list(Eff=CR_eff_t))
summary(CR_umf)

# Running Null model
CR_mod0 <- occu(~1~1, CR_umf)  # Null Model
summary(CR_mod0)
plogis( -0.129)  	# Probability of occupancy
plogis(  -0.832)	# Probability of detection
22/141	#Naive occupancy

# Running model with Eff as survey covariate
CR_m.psi1.pEff<- occu(~Eff~1, CR_umf) 
summary(CR_m.psi1.pEff)

# Running unicovariate models
CR_m.psiElev.pEff<- occu(~Eff~Elev , CR_umf)	# SIGNIFICANT!!
summary(CR_m.psiElev.pEff)
CR_m.psiHFI.pEff<- occu(~Eff~HFI , CR_umf) 	# SIGNIFICANT!!
summary(CR_m.psiHFI.pEff)
CR_m.psiRoad.pEff<- occu(~Eff~d.Road , CR_umf)	# SIGNIFICANT!!
summary(CR_m.psiRoad.pEff)
CR_m.psiPrecip.pEff<- occu(~Eff~Precip , CR_umf)
summary(CR_m.psiPrecip.pEff)
CR_m.psiNDVI.pEff<- occu(~Eff~NDVI , CR_umf)
summary(CR_m.psiNDVI.pEff)
CR_m.psiTemp.pEff<- occu(~Eff~Avg.Max.Temp , CR_umf)
summary(CR_m.psiTemp.pEff)

###plots###

#distance to road
# Plotting top Unicovariate model (d.Road)
CR_pred.psi.road<- predict(CR_m.psiRoad.pEff, 
                           newdata= data.frame(d.Road= sort(scale(CR_sc_t2$d.Road))), "state")

#Plot the results
CR_Road <- ggplot(CR_pred.psi.road, aes(x = sort(scale(CR_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Distance to Road")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-1.09, -0.1175,  0.855,  1.8275, 2.7), labels = c(100, 5000, 10000, 15000, 20000))+
  coord_cartesian(ylim = c(0,1))
#-1.09 - 2.80
#94.14-24410.9

#Elev
# Plotting top Unicovariate model (Elev)
CR_pred.psi.elev<- predict(CR_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(CR_sc_t$Elev))), "state")

#Plot the results
CR_Elev <- ggplot(CR_pred.psi.elev, aes(x = sort(scale(CR_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.004", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.85, -1.0175, -0.185, 0.6475, 1.4), labels = c(20, 900, 1700, 2500, 3400))+
  coord_cartesian(ylim = c(0,1))
#-1.85-1.487
#23-2443

#Precip
# Plotting top Unicovariate model (Precip)
CR_pred.psi.precip<- predict(CR_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(CR_sc_t$Precip))), "state")

#Plot the results
CR_Precip <- ggplot(CR_pred.psi.precip, aes(x = sort(scale(CR_sc_t$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.61", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.599, -0.352, 0.895, 2.142, 3.389), labels = c(150, 200, 250, 300, 350))+
  coord_cartesian(ylim = c(0,1)) 
#-1.599433  3.389610
#152.25 353.75

#NDVI
# Plotting top Unicovariate model (Precip)
CR_pred.psi.NDVI<- predict(CR_m.psiNDVI.pEff, newdata= data.frame(NDVI= sort(scale(CR_sc_t$NDVI))), "state")

#Plot the results
CR_NDVI <- ggplot(CR_pred.psi.NDVI, aes(x = sort(scale(CR_sc_t$NDVI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "plum4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.11", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-3.989, -2.5685, -1.148, 0.2725, 1.693), labels = c(0.039, 0.147, 0.255, 0.363, 0.472))+
  coord_cartesian(ylim = c(0,1)) 
#-1.599433  3.389610
#152.25 353.75

# Temp
# Plotting top Unicovariate model (Precip)
CR_pred.psi.Temp<- predict(CR_m.psiTemp.pEff, newdata= data.frame(Avg.Max.Temp= sort(scale(CR_sc_t$Avg.Max.Temp))), "state")

#Plot the results
CR_Temp <- ggplot(CR_pred.psi.Temp, aes(x = sort(scale(CR_sc_t$Avg.Max.Temp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "steelblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.01", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.172, -0.418, 0.336, 1.09, 1.844), labels = c(13.80, 18.45, 23.05, 27.68, 32.30))+
  coord_cartesian(ylim = c(0,1)) 
#-1.599433  3.389610
#152.25 353.75


############################################################################################################
##########################Mountain Tapir####################################################################


###Read in Variables
setwd("~/Downloads/Mountain")

#read in tapir occurance records
Mt_tapir_t<- readRDS("Collapsed_Capture_Mountain_Tapir_revised_DR.rds") 

#read in effort table
Mt_eff_t<- readRDS("Effort_Mountain_Tapir_revised_DR.rds")


#read in covariate table
Mt_sc_t<- read.csv("Mt_T_Covs4.csv") 

#edit covariate table
#Mt_sc_t <- Mt_sc_t[,c("Camr_Nm","Latitud","Longitd","S06W080.hgt","HFI_MountainTapir_EPSG4326")]
#colnames(Mt_sc_t)[colnames(Mt_sc_t) == "S06W080.hgt"] ="Elev"
#colnames(Mt_sc_t)[colnames(Mt_sc_t) == "HFI_MountainTapir_EPSG4326"] ="HFI"
Mt_sc_t <- Mt_sc_t[,-1] #get rid of random X column

#scale covariates and rename to match previous code
Mt_sc_t2<- cbind(Mt_sc_t[,1:3], round(scale(Mt_sc_t[,c(4,6,7,8,10)]),3)) #don't scale HFI because all the same
Mt_sc_t2 <- cbind(Mt_sc_t2, Mt_sc_t[5]) #add HFI back
#colnames(Mt_sc_t2)[4]="Elev"
colnames(Mt_sc_t2)[1]="Station"

#ensure rownames match
rownames(Mt_tapir_t) == rownames(Mt_eff_t)
sort(rownames(Mt_eff_t)) == sort(Mt_sc_t2$Station) #they are the same but it still says false?

#effort
sum(rowSums(Mt_eff_t, na.rm = TRUE)) #9,456 effort
mean(rowSums(Mt_eff_t, na.rm = TRUE))	#111.25 mean camera active days
sd(rowSums(Mt_eff_t, na.rm = TRUE)) 	#sd mean camera active days 30.92
sum(rowSums(Mt_tapir_t, na.rm = TRUE)) #85 independent records


#Establish Unmarked Data Frame##############################################################

Mt_umf<- unmarkedFrameOccu(y=Mt_tapir_t, siteCovs=Mt_sc_t2, obsCovs=list(Eff=Mt_eff_t))
summary(Mt_umf) #28 sites with at least one detection!


#Running Models#######################################################################

# Running Null model
Mt_mod0 <- occu(~1~1, Mt_umf)  # Null Model
summary(Mt_mod0)
plogis( -0.595)  	# Probability of occupancy 0.35
plogis( -1.56)	# Probability of detection 0.17
28/85 #Naive occupancy - 0.3294

# Running model with Eff as survey covariate
Mt_m.psi1.pEff<- occu(~Eff~1, Mt_umf) 
summary(Mt_m.psi1.pEff)

# Running unicovariate models
#Elev
Mt_m.psiElev.pEff<- occu(~Eff~Elev , Mt_umf)
summary(Mt_m.psiElev.pEff)
#HFI
Mt_m.psiHFI.pEff<- occu(~Eff~HFI , Mt_umf) #all have same HFI, so no use for this species
summary(Mt_m.psiHFI.pEff)
#d.Road
Mt_m.psiRoad.pEff<- occu(~Eff~d.Road , Mt_umf)
summary(Mt_m.psiRoad.pEff)
#Precip
Mt_m.psiPrecip.pEff <- occu(~Eff~Precip, Mt_umf)
summary(Mt_m.psiPrecip.pEff)
#NDVI
Mt_m.psiNDVI.pEff <- occu(~Eff~NDVI, Mt_umf)
summary(Mt_m.psiNDVI.pEff)
#Temp
Mt_m.psiTemp.pEff <- occu(~Eff~AvgMaxTemp, Mt_umf)
summary(Mt_m.psiTemp.pEff)


###Plots

#distance to road
# Plotting top Unicovariate model (d.Road)
Mt_pred.psi.road<- predict(Mt_m.psiRoad.pEff, 
                           newdata= data.frame(d.Road= sort(scale(Mt_sc_t2$d.Road))), "state")

#Plot the results
Mt_Road <- ggplot(Mt_pred.psi.road, aes(x = sort(scale(Mt_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.80", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Distance to Road")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-2.3, -1.2, -0.117, 0.9745, 2), labels = c(750, 2500, 5000, 7500, 10000))+
  coord_cartesian(ylim = c(0,1))
#750 to 10345m from road
#-2.366 - 2.021

#Elev
# Plotting top Unicovariate model (Elev)
Mt_pred.psi.elev<- predict(Mt_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Mt_sc_t$Elev))), "state")

#Plot the results
Mt_Elev <- ggplot(Mt_pred.psi.elev, aes(x = sort(scale(Mt_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.23", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.6, -0.7, 0.2, 1.1, 1.8), labels = c(1500, 2000, 2500, 3000, 3500))+
  coord_cartesian(ylim = c(0,1))
#1615 m to 3678m
#-1.59 - 2.00

#Precip
# Plotting top Unicovariate model (Precip)
Mt_pred.psi.precip<- predict(Mt_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Mt_sc_t$Precip))), "state")

#Plot the results
Mt_Precip <- ggplot(Mt_pred.psi.precip, aes(x = sort(scale(Mt_sc_t$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.91", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.78, -0.8575, 0.065, 0.98, 1.8), labels = c(80, 90, 100, 110, 120))+
  coord_cartesian(ylim = c(0,1))
#-1.78 - 1.91
#82 - 118

#NDVI
# Plotting top Unicovariate model (Precip)
Mt_pred.psi.NDVI<- predict(Mt_m.psiNDVI.pEff, newdata= data.frame(NDVI= sort(scale(Mt_sc_t$NDVI))), "state")

#Plot the results
Mt_NDVI <- ggplot(Mt_pred.psi.NDVI, aes(x = sort(scale(Mt_sc_t$NDVI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "plum4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.30", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-2.587, -1.53525, -0.4835, 0.56825, 1.620), labels = c(0.13, 0.21, 0.29, 0.37, 0.45))+
  coord_cartesian(ylim = c(0,1))
#-1.78 - 1.91
#82 - 118

#Temp
# Plotting top Unicovariate model (Precip)
Mt_pred.psi.Temp<- predict(Mt_m.psiTemp.pEff, newdata= data.frame(AvgMaxTemp= sort(scale(Mt_sc_t$AvgMaxTemp))), "state")

#Plot the results
Mt_Temp <- ggplot(Mt_pred.psi.Temp, aes(x = sort(scale(Mt_sc_t$AvgMaxTemp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "steelblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.12", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.912, -1.0615, -0.211, 0.6395, 1.490), labels = c(16, 18.25, 20.50, 22.75, 25))+
  coord_cartesian(ylim = c(0,1))
#-1.78 - 1.91
#82 - 118


# Unable to use HFI, all the same so only one dot on a graph


#######################################################################################################################
########Malayan Tapir#################################################################################################


#Read in Variables###################################################################
setwd("~/Downloads/Malayan")
#read in tapir occurance records
Ma_tapir_t<- readRDS("Collapsed_Capture_Malayan_Tapir.rds") 

#read in effort table
Ma_eff_t<- readRDS("Effort_Malayan_Tapir.rds")

#read in covariate table
Ma_sc_t<- read.csv("Ma_T_Final_Covs.csv")

#scale covariates and rename to match previous code
Ma_sc_t2<- cbind(Ma_sc_t[,2:4], round(scale(Ma_sc_t[,c(5:11)]),3))


#Establish Unmarked Data Frame##############################################################

Ma_umf<- unmarkedFrameOccu(y=Ma_tapir_t, siteCovs=Ma_sc_t2, obsCovs=list(Eff=Ma_eff_t))
summary(Ma_umf) #150 sites with at least one detection!


#Running Models#######################################################################

# Running Null model
Ma_mod0 <- occu(~1~1, Ma_umf)  # Null Model
summary(Ma_mod0)
plogis( 0.188)  	# Probability of occupancy 0.5468
plogis( -1.82)	# Probability of detection 0.1394
150/329 #Naive occupancy - 0.4559

# Running model with Eff as survey covariate
Ma_m.psi1.pEff<- occu(~Eff~1, Ma_umf) 
summary(Ma_m.psi1.pEff)

# Running unicovariate models
#Elev
Ma_m.psiElev.pEff<- occu(~Eff~Elev , Ma_umf)
summary(Ma_m.psiElev.pEff)
plogis( 1.019) # Probability of occupancy 0.734
plogis(0.342)	# Probability of detection 0.5846

#d.Road
Ma_m.psiRoad.pEff<- occu(~Eff~d.Road , Ma_umf)
summary(Ma_m.psiRoad.pEff)
plogis(0.421) # Probability of occupancy 0.604
plogis(0.339)	# Probability of detection 0.584

#Precip
Ma_m.psiPrecip.pEff <- occu(~Eff~Precip, Ma_umf)
summary(Ma_m.psiPrecip.pEff)
plogis(0.790) # Probability of occupancy 0.6878
plogis(0.341)	# Probability of detection 0.584433

#NDVI
Ma_m.psiNDVI.pEff <- occu(~Eff~NDVI, Ma_umf)
summary(Ma_m.psiNDVI.pEff)

#Temp
Ma_m.psiTemp.pEff <- occu(~Eff~AvgMaxTemp, Ma_umf)
summary(Ma_m.psiTemp.pEff)

###Plots

#Elev
# Plotting top Unicovariate model (Elev)
Ma_pred.psi.elev<- predict(Ma_m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(Ma_sc_t$Elev))), "state")

#Plot the results
Ma_Elev <- ggplot(Ma_pred.psi.elev, aes(x = sort(scale(Ma_sc_t$Elev)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "darkslategray4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Elevation (m)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.3,-0.315, 0.81, 1.935, 3.0), labels = c(300, 650, 1000, 1350, 1750))+
  coord_cartesian(ylim = c(0,1))
#289 1763m
#-1.441897  3.060600

#Precip
# Plotting top Unicovariate model (Precip)
Ma_pred.psi.precip<- predict(Ma_m.psiPrecip.pEff, newdata= data.frame(Precip= sort(scale(Ma_sc_t$Precip))), "state")

#Plot the results
Ma_Precip <- ggplot(Ma_pred.psi.precip, aes(x = sort(scale(Ma_sc_t$Precip)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "thistle4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.001", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Precipitation (?)", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Elev")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-1.423, -0.567, 0.288, 1.14, 2.00), labels = c(180, 200, 220, 240, 260))+
  coord_cartesian(ylim = c(0,1))
#-1.423178  2.005328
#186.3333 253.3333

# NDVI
Ma_pred.psi.NDVI<- predict(Ma_m.psiNDVI.pEff, newdata= data.frame(NDVI= sort(scale(Ma_sc_t$NDVI))), "state")

#Plot the results
Ma_NDVI <- ggplot(Ma_pred.psi.NDVI, aes(x = sort(scale(Ma_sc_t$NDVI)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "plum4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.01", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "HFI", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - HFI")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", breaks = c(-3.131, -1.8765, -0.622, 0.6325, 1.887), labels = c(0.21, 0.28, 0.34, 0.41, 0.48))+
  coord_cartesian(ylim = c(0,1))
#-0.2090092  6.5705417
#0-7

#distance to road
# Plotting top Unicovariate model (d.Road)
Ma_pred.psi.road<- predict(Ma_m.psiRoad.pEff, 
                           newdata= data.frame(d.Road= sort(scale(Ma_sc_t2$d.Road))), "state")

#Plot the results
Ma_Road <- ggplot(Ma_pred.psi.road, aes(x = sort(scale(Ma_sc_t2$d.Road)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "skyblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P = 0.003", x = 0.05, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Distance to Road")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir_Road() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-1.48, -0.49, 0.435, 1.36, 2.27), labels = c(10, 5000, 10000, 15000, 18000))+
  coord_cartesian(ylim = c(0,1))
#7.9041525 18060.96 from road
#-1.424047  2.294052

#Temp
Ma_pred.psi.Temp<- predict(Ma_m.psiTemp.pEff, 
                           newdata= data.frame(AvgMaxTemp= sort(scale(Ma_sc_t2$AvgMaxTemp))), "state")

#Plot the results
Ma_Temp <- ggplot(Ma_pred.psi.Temp, aes(x = sort(scale(Ma_sc_t2$AvgMaxTemp)), y = Predicted)) + # mean line
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed", fill = "steelblue4") + #Confidence intervals
  geom_path(size = 1, colour = "black") +
  annotation_custom(grobTree(textGrob("P < 0.0001", x = 0.55, y = 0.95, hjust = 0, vjust = 1, gp = gpar(fontsize = 14, fontface = "italic"))))+
  #labs(x = "Distance to Road", y = "Occupancy probability") + # axis labels
  #ggtitle("Baird's Tapir - Distance to Road")+
  labs(x = " ", y = " ") + # axis labels
  theme_tapir2() +
  scale_x_continuous(name = " ", n.breaks = 10, breaks = c(-3.297, -2.1005, -0.904, 0.2925, 1.489), labels = c(23.5, 25.1, 26.8, 28.4, 30.0))+
  coord_cartesian(ylim = c(0,1))


###########################################################################################################
############Adding all plots together######################################################################


commonplot <- ggarrange(Am_Road, Am_Elev, Am_Precip, Am_NDVI, Am_Temp, CR_Road, CR_Elev, CR_Precip, CR_NDVI, CR_Temp, Ma_Road, Ma_Elev, Ma_Precip, Ma_NDVI, Ma_Temp, Mt_Road, Mt_Elev, Mt_Precip, Mt_NDVI, Mt_Temp,
                        labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"),
                        ncol = 5, nrow = 4)

#commonplot
#ploat titles are manually spaced, so you'll have to get the correct pixel dimensions for everything to line up correctly
#I didn't add the labels as y or x labels because it messed with the spacing of the text

annotate_figure(commonplot,  
                left = text_grob("            Mountain Tapir ψ        Malayan Tapir ψ           Baird's Tapir ψ         Lowland Tapir ψ", rot = 90, size = 13, face = "bold"),
                bottom = text_grob(" Distance to Road (m)                          Elevation (m)                            Precipitation (mm)                                 NDVI                                     Temperature (°C)", color = "black", face = "bold", size = 14))

