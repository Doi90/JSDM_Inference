setwd("C:/Users/David/Dropbox/JSDM/JSDM Comparison Compiled/Ovaskainen 2016 - Non-spatial - Butterfly/examples/Butterfly/posteriors")

Beta <- read.csv("beta_Butterfly.csv")

sd.vector <- apply(X = Beta, MARGIN = 2, FUN = sd)

# nrow = #spp, ncol = # covar + intercept

sd.matrix <- matrix(sd.vector, nrow = 55, ncol = 5, byrow = T)

write.csv(sd.matrix, "Beta_sd_Butterfly_OvNS.csv")
