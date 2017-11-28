setwd("C:/Users/David/Dropbox/JSDM/JSDM Comparison Compiled/Ovaskainen 2016 - Non-spatial - Mosquito/examples/Mosquito/posteriors")

Beta <- read.csv("beta_Mosquito.csv")

sd.vector <- apply(X = Beta, MARGIN = 2, FUN = sd)

# nrow = #spp, ncol = # covar + intercept

sd.matrix <- matrix(sd.vector, nrow = 16, ncol = 14, byrow = T)

write.csv(sd.matrix, "Beta_sd_Mosquito_OvNS.csv")
