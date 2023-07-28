###Corrgram for Lowland Tapir###
library(corrgram)

round(cor(Lowland_covariates[, 5:14], use="pair"),2)

#Incorporating the covariates
vars2 <- c("Elev","HFI","Water","Forest","NPP","d.Road","d.River","EdgeDens","PatchDens","DisjCore")

#Making the corrgram
corrgram(Lowland_covariates[,vars2], order = TRUE,
         main="Lowland corrgram",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)
