setwd("C:/Users/David/Dropbox/JSDM/JSDM Comparison Compiled/Ovaskainen 2016 - Non-spatial - Birds/examples/Bird/posteriors")

Beta <- read.csv("beta_Bird.csv")

sd.vector <- apply(X = Beta, MARGIN = 2, FUN = sd)

# nrow = #spp, ncol = # covar + intercept

sd.matrix <- matrix(sd.vector, nrow = 370, ncol = 9, byrow = T)

write.csv(sd.matrix, "Beta_sd_Bird_OvNS.csv")
