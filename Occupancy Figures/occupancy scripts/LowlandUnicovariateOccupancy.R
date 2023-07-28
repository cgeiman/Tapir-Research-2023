###------------------------------------------------------------------------------###
### Running Unicovariate models on lowland tapir
### CA 18Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> tapir_AM.rds >> tapir occurance records
#> eff_AM.rds >> effort table
#> cv_t_AM_v2.csv >> covariate table

rm(list=ls())
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Amazon (Lowland Tapir)")#Directory of R-project "Models" on github
#dir()

library(unmarked)


#read in tapir occurance records
AM_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/tapir_AM.rds")
#read in effort table
AM_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/eff_AM.rds")
#read in covariate table
AM_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Amazon (Lowland Tapir)/cv_t_AM_v2.csv")

#scale covariates
AM_cov<- cbind(AM_cov[,c(1:4)], round(scale(AM_cov[,5:ncol(AM_cov)]),3))

#ensure rownames match
# rownames(AM_tapir) == rownames(AM_eff)
# rownames(AM_eff) == AM_cv$Station

# Checking for sitecov correlations
#as.dist(cor(AM_cv[,-c(1:4)]))
# Some sitecovs are correlated: Road&Elev, ED&PD&DC (do not include correlated covs in the same model)

#Establish Unmarked Data Frame##############################################################

AM_umf<- unmarkedFrameOccu(y=AM_tapir, siteCovs=AM_cov, obsCovs=list(Eff=AM_eff))
#summary(AM_umf) #67 sites with detection


#Running Models#######################################################################

# Running model with Eff as survey covariate
AM_m.psi1.pEff      <- occu(~Eff~ 1, AM_umf) 
AM_m.psiElev.pEff   <- occu(~Eff~ Elev , AM_umf)
AM_m.psiRoad.pEff   <- occu(~Eff~ d.Road , AM_umf)  
AM_m.psiNDVI.pEff   <- occu(~Eff~ NDVI , AM_umf)
AM_m.psiTempmax.pEff<- occu(~Eff~ Avg.Max.Temp , AM_umf)
AM_m.psiPrecip.pEff <- occu(~Eff~ MAP , AM_umf)
AM_m.psiNPP.pEff    <- occu(~Eff~ NPP , AM_umf)
AM_m.psiHFI.pEff    <- occu(~Eff~ HFI , AM_umf)
AM_m.psiTempmin.pEff<- occu(~Eff~ Avg.Min.Temp, AM_umf) 

## Remove the mods that aren't in all species
# AM_m.psiReg.pEff    <- occu(~Eff~ Dataset , AM_umf)
# AM_m.psiWat.pEff    <- occu(~Eff~ Water , AM_umf)
# AM_m.psiDC.pEff     <- occu(~Eff~ DisjCore , AM_umf)
# AM_m.psiPD.pEff     <- occu(~Eff~ PatchDens , AM_umf)
# AM_m.psiED.pEff     <- occu(~Eff~ EdgeDens , AM_umf)
# AM_m.psiRiver.pEff  <- occu(~Eff~ d.River, AM_umf) 
# AM_m.psiFor.pEff    <- occu(~Eff~ Forest , AM_umf)

##collect in fitList
AM_detlist<-fitList(AM_m.psi1.pEff     ,
                    AM_m.psiElev.pEff  ,
                    AM_m.psiRoad.pEff  ,
                    AM_m.psiNDVI.pEff  ,
                    AM_m.psiTempmax.pEff  ,
                    AM_m.psiPrecip.pEff,
                    AM_m.psiNPP.pEff   ,
                    AM_m.psiHFI.pEff   ,
                    AM_m.psiTempmin.pEff
                    # AM_m.psiFor.pEff   ,
                    # AM_m.psiReg.pEff   ,
                    # AM_m.psiWat.pEff   ,
                    # AM_m.psiDC.pEff    ,
                    # AM_m.psiPD.pEff    ,
                    # AM_m.psiED.pEff    ,
                    # AM_m.psiRiver.pEff ,
)

##do AIC model selection
modSel(AM_detlist) 

#print p and psi
 #function in printPandPsi.R (C:/Users/chris/Tapirus-Research/Cates Stuff/Models/unicovariateModels/printPandPsi.R)
columns<- c(5:18)
sink("AM_mods.txt", append = FALSE)
print("**Lowland Tapir Models**")
modSel(AM_detlist)
cat("\n**P and 𝜓**\n")
getStats()
cat("\n***Correlation Matrix***\n")
cor(AM_cov[, columns])
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
