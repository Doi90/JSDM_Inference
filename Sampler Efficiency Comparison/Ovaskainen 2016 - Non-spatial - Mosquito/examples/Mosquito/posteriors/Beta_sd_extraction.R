setwd("C:/Users/David/Dropbox/JSDM/JSDM Comparison Compiled/Sampler Efficiency Comparison/Ovaskainen 2016 - Non-spatial - Mosquito/examples/Mosquito/posteriors")

Beta <- read.csv("beta_Mosquito.csv", header = F)

ess.B <- effectiveSize(Beta)
ess.B <- as.vector(ess.B)
ess.B <- matrix(ess.B, ncol = 14, nrow = 16, byrow = TRUE)

write.csv(ess.B, "ess_beta_Mosquito_OvNS.csv")



mean(ess.B)/(830.157178/60/60)
sd(ess.B)/(830.157178/60/60)
range(ess.B)/(830.157178/60/60)

Rho <- read.csv("R1_Mosquito.csv", header = F)

ess.B <- effectiveSize(Beta)

mean(ess.B)/(615.305111/60/60)/4
sd(ess.B)/(615.305111/60/60)/4
range(ess.B)/(615.305111/60/60)/4
