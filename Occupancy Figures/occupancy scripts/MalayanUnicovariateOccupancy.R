###------------------------------------------------------------------------------###
### Running Unicovariate models on Malayan tapir
### CA 12Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> Collapsed_Capture_Malayan_Tapir.rds >> tapir occurance records
#> Effort_Malayan_Tapir.rds >> effort table
#> Ma_T_Final_Covs.csv now MA_covs.csv 17 jul 2023>> covariate table
#clear system
rm(list=ls())
library(unmarked)

#set wd
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Malaysia (Malayan Tapir)")##Directory of R-project "Models" on github
#dir()


######Read in Tapir table and Effort###########
MA_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Collapsed_Capture_Malayan_Tapir.rds")

MA_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Data Processing/Effort_Malayan_Tapir.rds")


#MA_cv<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/Ma_T_Final_Covs.csv")
MA_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/MA_covs.csv")


#####Model-Prep######################

MA_umf<- unmarkedFrameOccu(y=MA_tapir[,-1], siteCovs= as.data.frame(scale(MA_cov[,-c(1:5)])), obsCovs=list(Eff=MA_eff[,-1]))
#summary(MA_umf)
#head(MA_cv)

######Running Models!####################################

# Running model with Eff as survey covariate
MA_m.psi1.pEff      <- occu(~Eff ~1, MA_umf)  # Eff Model
MA_m.psiElev.pEff   <- occu(~Eff ~Elev, MA_umf)
MA_m.psiPrec.pEff   <- occu(~Eff ~Precip, MA_umf)
MA_m.psiRoad.pEff <- occu(~Eff ~d.Road, MA_umf)
MA_m.psiTempmax.pEff<- occu(~Eff ~AvgMaxTemp, MA_umf) 
MA_m.psiNDVI.pEff   <- occu(~Eff ~NDVI, MA_umf) 
MA_m.psiTempmin.pEff<- occu(~Eff ~AvgMinTemp, MA_umf)
MA_m.psiHFI.pEff    <- occu(~Eff ~HFI, MA_umf)
MA_m.psiNPP.pEff    <- occu(~Eff ~NPP, MA_umf)




##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other
#detList.tapir<-fitList(mod0, m.psi1.pEff, m.p1.psiHFI, m.p1.psiElev, m.p1.psiPrec, m.pEff.psiPrec, m.pEff.psiElev, m.pEff.psiHFI)
MA_detlist <-fitList(MA_m.psi1.pEff      ,
                    MA_m.psiElev.pEff   ,
                    MA_m.psiPrec.pEff   ,
                    MA_m.psiRoad.pEff ,
                    MA_m.psiTempmax.pEff,
                    MA_m.psiNDVI.pEff   ,
                    MA_m.psiHFI.pEff    ,
                    MA_m.psiTempmin.pEff,
                    MA_m.psiNPP.pEff
                        
)

# modSel compares AND ranks the models against eachother!
modSel(MA_detlist)

columns<- c(6:13)

sink("MA_mods.txt", append = FALSE)
  print("**Malayan Tapir Models**")
  modSel(MA_detlist)
  cat("\n**P and 𝜓**\n")
  getStats()
  cat("\n***Correlation Matrix***\n")
  cor(MA_cov[, columns])
sink()

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
