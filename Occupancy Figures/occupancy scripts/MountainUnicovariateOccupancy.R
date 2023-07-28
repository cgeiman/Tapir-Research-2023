###------------------------------------------------------------------------------###
### Running Unicovariate models on Mountain tapir
### CA 17Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> Collapsed_Capture_Mountain_Tapir_revised_DR.rds >> tapir occurance records
#> Effort_Mountain_Tapir_revised_DR.rds >> effort table
#> Mt_T_Covs4.csv now MT_covs.csv (17 jul 2023) >> covariate table


#clear system
rm(list=ls())
library(unmarked)
library(corrplot)

#set wd
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")#Directory of R-project "Models" on github
dir()#

######Read in Tapir table and Effort###########
MT_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
MT_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
#MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/Model Selection/Mt_T_Covs4.csv")
MT_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/MT_covs.csv", comment.char = "#")



#####Model-Prep######################
#Double check that all modeling columns are scaled correctly 
MT_umf<- unmarkedFrameOccu(y=MT_tapir[,-1], siteCovs= as.data.frame(scale(MT_cov[,-c(1:5)])), obsCovs=list(Eff=MT_eff[,-1]))

# correl<- cor(MT_cov[, -c(1,2,3,4,5)])
# 
# 
# 
# ggcorrplot(correl)
# corrgram(MT_cov[, -c(1,2,3,4,5)], lower.panel = panel.pts, method = "pearson")
# cor.test(MT_cov$NPP, MT_cov$NDVI, method = "spearman")

######Running Models!####################################
#Running Null model
#MT_mod0              <- occu(~1~1, MT_umf)  # Null Model
MT_m.psi1.pEff       <- occu(~Eff~ 1, MT_umf)  # Eff Model
MT_m.psiElev.pEff    <- occu(~Eff~ Elev, MT_umf)
MT_m.psiPrec.pEff    <- occu(~Eff~ Precip, MT_umf)
MT_m.psiRoad.pEff    <- occu(~Eff~ d.Road, MT_umf)
MT_m.psiTempmax.pEff <- occu(~Eff~ AvgMaxTemp, MT_umf) 
MT_m.psiNDVI.pEff    <- occu(~Eff~ NDVI, MT_umf) 
MT_m.psiTempmin.pEff <- occu(~Eff~ AvgMinTemp, MT_umf)
MT_m.psiNPP.pEff     <- occu(~Eff~ NPP, MT_umf)
##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other

MT_detlist<-fitList(
                    MT_m.psi1.pEff       ,
                    MT_m.psiElev.pEff    ,
                    MT_m.psiPrec.pEff    ,
                    MT_m.psiRoad.pEff    ,
                    MT_m.psiTempmax.pEff ,
                    MT_m.psiNDVI.pEff    ,
                    MT_m.psiTempmin.pEff ,
                    MT_m.psiNPP.pEff  
)

# modSel compares AND ranks the models against each other!
modSel(MT_detlist) 

#continuous variables, elev, d.road, precip, NDVI, minT, maxT, NPP
columns<- c(6, 8:13)

# Create a correlation matrix plot
corrplot(cor(MT_cov[,columns]), method = "shade", type = "upper", diag = TRUE, tl.cex = 0.8)

sink("MT_mods.txt", append = FALSE)
  print("**Mountain Tapir Models**")
  modSel(MT_detlist)
  cat("\n**P and 𝜓**\n")
  getStats()
  cat("\n***Correlation Matrix***\n")
  cor(MT_cov[, columns])
sink()
# 
# #function for psi value
# pf <- function(x) {
#   occu <- 0
#   if(length(x@estimates@estimates$state@estimates) > 2) {
#     for(i in 2:length(x@estimates@estimates$state@estimates)) {
#       occu <- (occu + plogis(x@estimates@estimates$state@estimates[i])) 
#     }
#     occu <- occu/(length(x@estimates@estimates$state@estimates)-1)
#   } else {
#     occu <- plogis(x@estimates@estimates$state@estimates[2])
#   }
#   print(paste("𝜓= ", signif(occu, digits = 4)))
# }
# 
# # Function to give detection probabilities (p) for models 
# pd <- function(x) {
#   detp <- 0
#   if(length(x@estimates@estimates$det@estimates) > 2) {
#     for(i in 2:length(x@estimates@estimates$det@estimates)) {
#       detp <- (detp + plogis(x@estimates@estimates$det@estimates[i])) 
#     }
#     detp <- detp/(length(x@estimates@estimates$det@estimates)-1)
#   } else {
#     detp <- plogis(x@estimates@estimates$det@estimates[2])
#   }
#   print(paste("p= ", signif(detp, digits=4)))
# }
# 
# #function of both funcs
# pfpd<- function(x){
#   print(x@formula)
#   pf(x)
#   pd(x)
# }
