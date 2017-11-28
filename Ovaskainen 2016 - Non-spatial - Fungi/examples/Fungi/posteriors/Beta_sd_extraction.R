setwd("C:/Users/David/Dropbox/JSDM/JSDM Comparison Compiled/Ovaskainen 2016 - Non-spatial - Fungi/examples/Fungi/posteriors")

Beta <- read.csv("beta_Fungi.csv")

sd.vector <- apply(X = Beta, MARGIN = 2, FUN = sd)

# nrow = #spp, ncol = # covar + intercept

sd.matrix <- matrix(sd.vector, nrow = 11, ncol = 16, byrow = T)

write.csv(sd.matrix, "Beta_sd_Fungi_OvNS.csv")
