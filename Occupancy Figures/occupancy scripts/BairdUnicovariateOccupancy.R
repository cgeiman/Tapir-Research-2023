###------------------------------------------------------------------------------###
### Running models on baird's
### Edited by: CA 18Jul2023
###------------------------------------------------------------------------------###

#> Files needed:
#> tapir_CR.rds >> tapir occurance records
#> eff_CR.rds >> effort table
#> cv_t3.csv >> covariate table


rm(list=ls())

library(unmarked)
#setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)")#Directory of R-project "Models" on github
setwd("C:/Users/chris/Tapirus-Research/Cates Stuff/Models")
#dir()#


#occurance recs
CR_tapir<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/Scripts/tapir_CR.rds") 
#effort table
CR_eff<- readRDS("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/Scripts/eff_CR.rds")
#covariates
CR_cov<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/cv_t3.csv")

# #remove first column and scale all numerical values
# CR_cv<- cbind(CR_cv[,c(2:5, 7)], round(scale(CR_cv[,c(6, 8:ncol(CR_cv))])))

#rownames(CR_tapir) == rownames(CR_eff)
#rownames(CR_eff) == CR_cv$Station

CR_umf<- unmarkedFrameOccu(y=CR_tapir, siteCovs= as.data.frame(scale(CR_cov[,-c(1:5)])), obsCovs=list(Eff=CR_eff))
#summary(CR_umf)

#-----------------------------------------------------------------------
# Running models
CR_m.psi1.pEff		    <- occu(~Eff~ 1, CR_umf) 
CR_m.psiElev.pEff	    <- occu(~Eff~ Elev , CR_umf)
CR_m.psiRoad.pEff	    <- occu(~Eff~ d.Road , CR_umf)
CR_m.psiTempmax.pEff  <- occu(~Eff~ Avg.Max.Temp, CR_umf)
CR_m.psiTempmin.pEff  <- occu(~Eff~ Avg.Min.Temp, CR_umf) 
CR_m.psiNDVI.pEff	    <- occu(~Eff~ NDVI, CR_umf)
CR_m.psiPrecp.Eff     <- occu(~Eff~ Precip, CR_umf)
CR_m.psiNPP.pEff	    <- occu(~Eff~ NPP ,CR_umf)
CR_m.psiHFI.pEff	    <- occu(~Eff~ HFI ,CR_umf) 
## models not present in mountain and Malayan
# CR_m.psiFor.pEff	    <- occu(~Eff~Forest ,CR_umf)
# CR_m.psiDC.pEff		    <- occu(~Eff~DisjCore ,CR_umf)
# CR_m.psiPD.pEff		    <- occu(~Eff~PatchDens ,CR_umf)
# CR_m.psiED.pEff		    <- occu(~Eff~EdgeDens ,CR_umf)
# CR_m.psiRiver.pEff	  <- occu(~Eff~d.River ,CR_umf)

##>> collect in fitList
CR_detlist<-fitList(CR_m.psi1.pEff		,
                   CR_m.psiElev.pEff	,
                   CR_m.psiRoad.pEff	,
                   CR_m.psiNDVI.pEff	,
                   CR_m.psiPrecp.Eff ,
                   CR_m.psiTempmax.pEff,
                   CR_m.psiTempmin.pEff,
                   CR_m.psiNPP.pEff	,
                   CR_m.psiHFI.pEff	
                   # CR_m.psiFor.pEff	,
                   # CR_m.psiDC.pEff		,
                   # CR_m.psiPD.pEff		,
                   # CR_m.psiED.pEff		,
                   # CR_m.psiRiver.pEff	,
)

##do AIC model selection
modSel(CR_detlist) 

columns<- c(6, 8:18)
sink("CR_mods.txt", append = FALSE)
print("**Baird Tapir Models**")
modSel(CR_detlist)
cat("\n**P and 𝜓**\n")
getStats()
cat("\n***Correlation Matrix***\n")
cor(CR_cov[, columns])
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
