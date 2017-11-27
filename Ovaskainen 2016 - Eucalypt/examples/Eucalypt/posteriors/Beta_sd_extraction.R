setwd("C:/Users/David/Dropbox/JSDM/JSDM Comparison Compiled/Ovaskainen 2016 - Eucalypt/examples/Eucalypt/posteriors")

Beta <- read.csv("beta_Eucalypt.csv")

sd.vector <- apply(X = Beta, MARGIN = 2, FUN = sd)

# nrow = #spp, ncol = # covar + intercept

sd.matrix <- matrix(sd.vector, nrow = 12, ncol = 8, byrow = T)

write.csv(sd.matrix, "Beta_sd_Eucalypt_Ov.csv")
