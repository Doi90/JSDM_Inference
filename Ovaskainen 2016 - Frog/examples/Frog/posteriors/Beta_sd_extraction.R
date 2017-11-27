setwd("C:/Users/David/Dropbox/JSDM/JSDM Comparison Compiled/Ovaskainen 2016 - Frog/examples/Frog/posteriors")

Beta <- read.csv("beta_Frog.csv")

sd.vector <- apply(X = Beta, MARGIN = 2, FUN = sd)

# nrow = #spp, ncol = # covar + intercept

sd.matrix <- matrix(sd.vector, nrow = 9, ncol = 4, byrow = T)

write.csv(sd.matrix, "Beta_sd_Frog_Ov.csv")
