#clear system
rm(list=ls())

#set wd
setwd("~/Desktop/Summer Research/Tapir data")
dir()

#Load detection/non-detection data
bd <- read.csv()
low <- read.csv()
mal <- read.csv(DETECTIONston_MalayanTapir.csv, header = FALSE)
mou <- read.csv(Mountain_Detection, header = FALSE)

#Inspect detection data
head(malayan)

#Load site-level covariate data
occ_covs <- read.csv()

#Inspect site-level covariates
head(occ_covs)

#Load detection covariate data
det_covs <- read.csv()

#Inspect detection covariates
head(det_covs)

#unmarked package
library(unmarked)

#Place detection/non-detection data into a named list
y_list <- (bairds = as.matrix(bd),
           lowland = as.matrix(low), 
           malayan = as.matrix(mal),
           mountain = as.matrix(mou))

#Place detection covariates into a named list
det_list <- list(temp = det_covs)

#Combine data into an unmarkedFrameOccuMulti object
msom_data <- unmarkedFrameOccuMulti(y = y_list,
                                    siteCovs = occ_covs,
                                    obsCovs = det_list)

###Intercept-only model, assuming independence###
fit_1 <- occuMulti(detformulas = c('~1', '~1', '~1'),
                   stateformulas = c('~1', '~1', '~1'),
                   maxOrder = 1,
                   data = msom_data)
#Summary
summary(fit_1)

#Incorporating covariates [DETERMINE WHICH COVARIATES GO WHERE]
fit_3 <- occuMulti(detformulas = c('', '~1', '~1'),
                   stateformulas = c('', '~1', '~1'),
                   maxOrder = ,
                   data = msom_data)
#Marginal occupancy probability
bd_margin <- predict(fit_3, type = 'state', species = 'bairds', 
                     newdata = nd_marg)
low_margin <- predict(fit_3, type = 'state', species = 'lowland', 
                     newdata = nd_marg)
mal_margin <- predict(fit_3, type = 'state', species = 'malayan', 
                     newdata = nd_marg)
mou_margin <- predict(fit_3, type = 'state', species = 'mountain', 
                     newdata = nd_marg)

#Formatting data for ggplot2 [INCOMPLETE]
gg_df_marg <-

marg_fig <-
           