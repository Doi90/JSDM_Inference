library(xlsx)

####################################
############ Load Data #############
####################################

# BayesComm

BC_Bird_mean <- read.csv("Rho_mean_Bird_BayesComm.csv")
BC_Bird_mean <- BC_Bird_mean[,-1]
BC_Bird_lower <- read.csv("Rho_lower_Bird_BayesComm.csv")
BC_Bird_lower <- BC_Bird_lower[,-1]
BC_Bird_upper <- read.csv("Rho_upper_Bird_BayesComm.csv")
BC_Bird_upper <- BC_Bird_upper[,-1]
BC_Bird_quantile <- BC_Bird_upper - BC_Bird_lower

BC_But_mean <- read.csv("Rho_mean_Butterfly_BayesComm.csv")
BC_But_mean <- BC_But_mean[,-1]
BC_But_lower <- read.csv("Rho_lower_Butterfly_BayesComm.csv")
BC_But_lower <- BC_But_lower[,-1]
BC_But_upper <- read.csv("Rho_upper_Butterfly_BayesComm.csv")
BC_But_upper <- BC_But_upper[,-1]
BC_But_quantile <- BC_But_upper - BC_But_lower

BC_Euc_mean <- read.csv("Rho_mean_Eucalypt_BayesComm.csv")
BC_Euc_mean <- BC_Euc_mean[,-1]
BC_Euc_lower <- read.csv("Rho_lower_Eucalypt_BayesComm.csv")
BC_Euc_lower <- BC_Euc_lower[,-1]
BC_Euc_upper <- read.csv("Rho_upper_Eucalypt_BayesComm.csv")
BC_Euc_upper <- BC_Euc_upper[,-1]
BC_Euc_quantile <- BC_Euc_upper - BC_Euc_lower

BC_Frog_mean <- read.csv("Rho_mean_Frog_BayesComm.csv")
BC_Frog_mean <- BC_Frog_mean[,-1]
BC_Frog_lower <- read.csv("Rho_lower_Frog_BayesComm.csv")
BC_Frog_lower <- BC_Frog_lower[,-1]
BC_Frog_upper <- read.csv("Rho_upper_Frog_BayesComm.csv")
BC_Frog_upper <- BC_Frog_upper[,-1]
BC_Frog_quantile <- BC_Frog_upper - BC_Frog_lower 

BC_Fun_mean <- read.csv("Rho_mean_Fungi_BayesComm.csv")
BC_Fun_mean <- BC_Fun_mean[,-1]
BC_Fun_lower <- read.csv("Rho_lower_Fungi_BayesComm.csv")
BC_Fun_lower <- BC_Fun_lower[,-1]
BC_Fun_upper <- read.csv("Rho_upper_Fungi_BayesComm.csv")
BC_Fun_upper <- BC_Fun_upper[,-1]
BC_Fun_quantile <- BC_Fun_upper - BC_Fun_lower

BC_Mosq_mean <- read.csv("Rho_mean_Mosquito_BayesComm.csv")
BC_Mosq_mean <- BC_Mosq_mean[,-1]
BC_Mosq_lower <- read.csv("Rho_lower_Mosquito_BayesComm.csv")
BC_Mosq_lower <- BC_Mosq_lower[,-1]
BC_Mosq_upper <- read.csv("Rho_upper_Mosquito_BayesComm.csv")
BC_Mosq_upper <- BC_Mosq_upper[,-1]
BC_Mosq_quantile <- BC_Mosq_upper - BC_Mosq_lower

# Pollock

Pol_Euc_mean <- read.csv("Rho_mean_Eucalypt_Pollock.csv")
Pol_Euc_mean <- Pol_Euc_mean[,-1]
Pol_Euc_lower <- read.csv("Rho_lower_Eucalypt_Pollock.csv")
Pol_Euc_lower <- Pol_Euc_lower[,-1]
Pol_Euc_upper <- read.csv("Rho_upper_Eucalypt_Pollock.csv")
Pol_Euc_upper <- Pol_Euc_upper[,-1]
Pol_Euc_quantile <- Pol_Euc_upper - Pol_Euc_lower

Pol_Frog_mean <- read.csv("Rho_mean_Frog_Pollock.csv")
Pol_Frog_mean <- Pol_Frog_mean[,-1]
Pol_Frog_lower <- read.csv("Rho_lower_Frog_Pollock.csv")
Pol_Frog_lower <- Pol_Frog_lower[,-1]
Pol_Frog_upper <- read.csv("Rho_upper_Frog_Pollock.csv")
Pol_Frog_upper <- Pol_Frog_upper[,-1]
Pol_Frog_quantile <- Pol_Frog_upper - Pol_Frog_lower 

Pol_Fun_mean <- read.csv("Rho_mean_Fungi_Pollock.csv")
Pol_Fun_mean <- Pol_Fun_mean[,-1]
Pol_Fun_lower <- read.csv("Rho_lower_Fungi_Pollock.csv")
Pol_Fun_lower <- Pol_Fun_lower[,-1]
Pol_Fun_upper <- read.csv("Rho_upper_Fungi_Pollock.csv")
Pol_Fun_upper <- Pol_Fun_upper[,-1]
Pol_Fun_quantile <- Pol_Fun_upper - Pol_Fun_lower

Pol_Mosq_mean <- read.csv("Rho_mean_Mosquito_Pollock.csv")
Pol_Mosq_mean <- Pol_Mosq_mean[,-1]
Pol_Mosq_lower <- read.csv("Rho_lower_Mosquito_Pollock.csv")
Pol_Mosq_lower <- Pol_Mosq_lower[,-1]
Pol_Mosq_upper <- read.csv("Rho_upper_Mosquito_Pollock.csv")
Pol_Mosq_upper <- Pol_Mosq_upper[,-1]
Pol_Mosq_quantile <- Pol_Mosq_upper - Pol_Mosq_lower

# Ovaskainen 2016

Ov_Euc_mean <- openxlsx::read.xlsx("R1_Eucalypt_Ov.xlsx", sheet = 1)
Ov_Euc_mean <- Ov_Euc_mean[,-1]
Ov_Euc_lower <- openxlsx::read.xlsx("R1_Eucalypt_Ov.xlsx", sheet = 5)
Ov_Euc_lower <- Ov_Euc_lower[,-1]
Ov_Euc_upper <- openxlsx::read.xlsx("R1_Eucalypt_Ov.xlsx", sheet = 6)
Ov_Euc_upper <- Ov_Euc_upper[,-1]
Ov_Euc_quantile <- Ov_Euc_upper - Ov_Euc_lower

Ov_Frog_mean <- openxlsx::read.xlsx("R1_Frog_Ov.xlsx", sheet = 1)
Ov_Frog_mean <- Ov_Frog_mean[,-1]
Ov_Frog_lower <- openxlsx::read.xlsx("R1_Frog_Ov.xlsx", sheet = 5)
Ov_Frog_lower <- Ov_Frog_lower[,-1]
Ov_Frog_upper <- openxlsx::read.xlsx("R1_Frog_Ov.xlsx", sheet = 6)
Ov_Frog_upper <- Ov_Frog_upper[,-1]
Ov_Frog_quantile <- Ov_Frog_upper - Ov_Frog_lower

Ov_Mosq_mean <- openxlsx::read.xlsx("R1_Mosquito_Ov.xlsx", sheet = 1)
Ov_Mosq_mean <- Ov_Mosq_mean[,-1]
Ov_Mosq_lower <- openxlsx::read.xlsx("R1_Mosquito_Ov.xlsx", sheet = 5)
Ov_Mosq_lower <- Ov_Mosq_lower[,-1]
Ov_Mosq_upper <- openxlsx::read.xlsx("R1_Mosquito_Ov.xlsx", sheet = 6)
Ov_Mosq_upper <- Ov_Mosq_upper[,-1]
Ov_Mosq_quantile <- Ov_Mosq_upper - Ov_Mosq_lower

# Ovaskainen 2016 NS

OvNS_Bird_mean <- read.xlsx("R1_Bird_OvNS.xlsx", sheetIndex = 1)
OvNS_Bird_mean <- OvNS_Bird_mean[,-1]
OvNS_Bird_lower <- openxlsx::read.xlsx("R1_Bird_OvNS.xlsx", sheet = 5)
OvNS_Bird_lower <- OvNS_Bird_lower[,-1]
OvNS_Bird_upper <- openxlsx::read.xlsx("R1_Bird_OvNS.xlsx", sheet = 6)
OvNS_Bird_upper <- OvNS_Bird_upper[,-1]
OvNS_Bird_quantile <- OvNS_Bird_upper - OvNS_Bird_lower

OvNS_But_mean <- openxlsx::read.xlsx("R1_Butterfly_OvNS.xlsx", sheet = 1)
OvNS_But_mean <- OvNS_But_mean[,-1]
OvNS_But_lower <- openxlsx::read.xlsx("R1_Butterfly_OvNS.xlsx", sheet = 5)
OvNS_But_lower <- OvNS_But_lower[,-1]
OvNS_But_upper <- openxlsx::read.xlsx("R1_Butterfly_OvNS.xlsx", sheet = 6)
OvNS_But_upper <- OvNS_But_upper[,-1]
OvNS_But_quantile <- OvNS_But_upper - OvNS_But_lower

OvNS_Euc_mean <- openxlsx::read.xlsx("R1_Eucalypt_OvNS.xlsx", sheet = 1)
OvNS_Euc_mean <- OvNS_Euc_mean[,-1]
OvNS_Euc_lower <- openxlsx::read.xlsx("R1_Eucalypt_OvNS.xlsx", sheet = 5)
OvNS_Euc_lower <- OvNS_Euc_lower[,-1]
OvNS_Euc_upper <- openxlsx::read.xlsx("R1_Eucalypt_OvNS.xlsx", sheet = 6)
OvNS_Euc_upper <- OvNS_Euc_upper[,-1]
OvNS_Euc_quantile <- OvNS_Euc_upper - OvNS_Euc_lower

OvNS_Frog_mean <- openxlsx::read.xlsx("R1_Frog_OvNS.xlsx", sheet = 1)
OvNS_Frog_mean <- OvNS_Frog_mean[,-1]
OvNS_Frog_lower <- openxlsx::read.xlsx("R1_Frog_OvNS.xlsx", sheet = 5)
OvNS_Frog_lower <- OvNS_Frog_lower[,-1]
OvNS_Frog_upper <- openxlsx::read.xlsx("R1_Frog_OvNS.xlsx", sheet = 6)
OvNS_Frog_upper <- OvNS_Frog_upper[,-1]
OvNS_Frog_quantile <- OvNS_Frog_upper - OvNS_Frog_lower

OvNS_Fun_mean <- openxlsx::read.xlsx("R1_Fungi_OvNS.xlsx", sheet = 1)
OvNS_Fun_mean <- OvNS_Fun_mean[,-1]
OvNS_Fun_lower <- openxlsx::read.xlsx("R1_Fungi_OvNS.xlsx", sheet = 5)
OvNS_Fun_lower <- OvNS_Fun_lower[,-1]
OvNS_Fun_upper <- openxlsx::read.xlsx("R1_Fungi_OvNS.xlsx", sheet = 6)
OvNS_Fun_upper <- OvNS_Fun_upper[,-1]
OvNS_Fun_quantile <- OvNS_Fun_upper - OvNS_Fun_lower

OvNS_Mosq_mean <- openxlsx::read.xlsx("R1_Mosquito_OvNS.xlsx", sheet = 1)
OvNS_Mosq_mean <- OvNS_Mosq_mean[,-1]
OvNS_Mosq_lower <- openxlsx::read.xlsx("R1_Mosquito_OvNS.xlsx", sheet = 5)
OvNS_Mosq_lower <- OvNS_Mosq_lower[,-1]
OvNS_Mosq_upper <- openxlsx::read.xlsx("R1_Mosquito_OvNS.xlsx", sheet = 6)
OvNS_Mosq_upper <- OvNS_Mosq_upper[,-1]
OvNS_Mosq_quantile <- OvNS_Mosq_upper - OvNS_Mosq_lower

# Clark

Clark_Euc_mean <- read.csv("Rho_mean_Eucalypts_Clark.csv")
Clark_Euc_mean <- Clark_Euc_mean[,-1]
Clark_Euc_lower <- read.csv("Rho_lower_Eucalypts_Clark.csv")
Clark_Euc_lower <- Clark_Euc_lower[,-1]
Clark_Euc_upper <- read.csv("Rho_upper_Eucalypts_Clark.csv")
Clark_Euc_upper <- Clark_Euc_upper[,-1]
Clark_Euc_quantile <- Clark_Euc_upper - Clark_Euc_lower

Clark_Frog_mean <- read.csv("Rho_mean_Frog_Clark.csv")
Clark_Frog_mean <- Clark_Frog_mean[,-1]
Clark_Frog_lower <- read.csv("Rho_lower_Frog_Clark.csv")
Clark_Frog_lower <- Clark_Frog_lower[,-1]
Clark_Frog_upper <- read.csv("Rho_upper_Frog_Clark.csv")
Clark_Frog_upper <- Clark_Frog_upper[,-1]
Clark_Frog_quantile <- Clark_Frog_upper - Clark_Frog_lower 

Clark_Fun_mean <- read.csv("Rho_mean_Fungi_Clark.csv")
Clark_Fun_mean <- Clark_Fun_mean[,-1]
Clark_Fun_lower <- read.csv("Rho_lower_Fungi_Clark.csv")
Clark_Fun_lower <- Clark_Fun_lower[,-1]
Clark_Fun_upper <- read.csv("Rho_upper_Fungi_Clark.csv")
Clark_Fun_upper <- Clark_Fun_upper[,-1]
Clark_Fun_quantile <- Clark_Fun_upper - Clark_Fun_lower

Clark_Mosq_mean <- read.csv("Rho_mean_Mosquito_Clark.csv")
Clark_Mosq_mean <- Clark_Mosq_mean[,-1]
Clark_Mosq_lower <- read.csv("Rho_lower_Mosquito_Clark.csv")
Clark_Mosq_lower <- Clark_Mosq_lower[,-1]
Clark_Mosq_upper <- read.csv("Rho_upper_Mosquito_Clark.csv")
Clark_Mosq_upper <- Clark_Mosq_upper[,-1]
Clark_Mosq_quantile <- Clark_Mosq_upper - Clark_Mosq_lower

# boral

boral_Euc_mean <- read.csv("Rho_mean_Eucalypt_boral.csv")
boral_Euc_mean <- boral_Euc_mean[,-1]
boral_Euc_lower <- read.csv("Rho_lower_Eucalypt_boral.csv")
boral_Euc_lower <- boral_Euc_lower[,-1]
boral_Euc_upper <- read.csv("Rho_upper_Eucalypt_boral.csv")
boral_Euc_upper <- boral_Euc_upper[,-1]
boral_Euc_quantile <- boral_Euc_upper - boral_Euc_lower

boral_Frog_mean <- read.csv("Rho_mean_Frog_boral.csv")
boral_Frog_mean <- boral_Frog_mean[,-1]
boral_Frog_lower <- read.csv("Rho_lower_Frog_boral.csv")
boral_Frog_lower <- boral_Frog_lower[,-1]
boral_Frog_upper <- read.csv("Rho_upper_Frog_boral.csv")
boral_Frog_upper <- boral_Frog_upper[,-1]
boral_Frog_quantile <- boral_Frog_upper - boral_Frog_lower 

boral_Fun_mean <- read.csv("Rho_mean_Fungi_boral.csv")
boral_Fun_mean <- boral_Fun_mean[,-1]
boral_Fun_lower <- read.csv("Rho_lower_Fungi_boral.csv")
boral_Fun_lower <- boral_Fun_lower[,-1]
boral_Fun_upper <- read.csv("Rho_upper_Fungi_boral.csv")
boral_Fun_upper <- boral_Fun_upper[,-1]
boral_Fun_quantile <- boral_Fun_upper - boral_Fun_lower

boral_Mosq_mean <- read.csv("Rho_mean_Mosquito_boral.csv")
boral_Mosq_mean <- boral_Mosq_mean[,-1]
boral_Mosq_lower <- read.csv("Rho_lower_Mosquito_boral.csv")
boral_Mosq_lower <- boral_Mosq_lower[,-1]
boral_Mosq_upper <- read.csv("Rho_upper_Mosquito_boral.csv")
boral_Mosq_upper <- boral_Mosq_upper[,-1]
boral_Mosq_quantile <- boral_Mosq_upper - boral_Mosq_lower

######################################
######## Density Plot Mosaic #########
######################################

####
#### Mean
####

# Blank PNG

png(filename = "Rho_Mean_Density.png", height = 15, width = 15, units = "in", res = 300)
# Set mosaic 6 rows, 4 columns

par(mfcol = c(4,6))

# Column 1: Birds


plot(density(unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,1.7), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)]), bw = 0.1), col = "brown1")
abline(v=0, lwd = 3, lty = 2)
abline(v=quantile(density(unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)]), bw = 0.1)$x, probs = 0.25), lwd = 2, col = "gray60")
abline(v=quantile(density(unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)]), bw = 0.1)$x, probs = 0.5), lwd = 3, col = "gray60")
abline(v=quantile(density(unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)]), bw = 0.1)$x, probs = 0.75), lwd = 2, col = "gray60")
mtext(side = 2, "BayesComm", line = 3)
mtext(side = 3, "Birds", line = 2)
plot.new()
mtext(side = 2, "Pollock", line = 3)
plot.new()
mtext(side = 2, "Ovaskainen 2016 Spatial", line = 3)
plot(density(unlist(OvNS_Bird_mean[upper.tri(OvNS_Bird_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,1.7), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Bird_mean[upper.tri(OvNS_Bird_mean)]), bw = 0.1), col = "darkorchid1")
abline(v=0)
mtext(side = 2, "Ovaskainen 2016 Non-Spatial", line = 3)

# Column 2: Butterflies

plot(density(unlist(BC_But_mean[upper.tri(BC_But_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,1.7), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_But_mean[upper.tri(BC_But_mean)]), bw = 0.1), col = "brown1")
abline(v=0)
mtext(side = 3, "Butterflies", line = 2)
plot.new()
plot.new()
plot(density(unlist(OvNS_But_mean[upper.tri(OvNS_But_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,1.7), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_But_mean[upper.tri(OvNS_But_mean)]), bw = 0.1), col = "darkorchid1")
abline(v=0)

# Column 3: Eucalypts

plot(density(unlist(BC_Euc_mean[upper.tri(BC_Euc_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3.3), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Euc_mean[upper.tri(BC_Euc_mean)]), bw = 0.1), col = "brown1")
abline(v=0)
mtext(side = 3, "Eucalypts", line = 2)
plot(density(unlist(Pol_Euc_mean[upper.tri(Pol_Euc_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3.3), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Euc_mean[upper.tri(Pol_Euc_mean)]), bw = 0.1), col = "chartreuse3")
abline(v=0)
plot(density(unlist(Ov_Euc_mean[upper.tri(Ov_Euc_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3.3), main = "", ylab = "", xlab = "")
polygon(density(unlist(Ov_Euc_mean[upper.tri(Ov_Euc_mean)]), bw = 0.1), col = "cyan")
abline(v=0)
plot(density(unlist(OvNS_Euc_mean[upper.tri(OvNS_Euc_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3.3), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Euc_mean[upper.tri(OvNS_Euc_mean)]), bw = 0.1), col = "darkorchid1")
abline(v=0)

# Column 4: Frogs

plot(density(unlist(BC_Frog_mean[upper.tri(BC_Frog_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Frog_mean[upper.tri(BC_Frog_mean)]), bw = 0.1), col = "brown1")
abline(v=0)
mtext(side = 3, "Frogs", line = 2)
plot(density(unlist(Pol_Frog_mean[upper.tri(Pol_Frog_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Frog_mean[upper.tri(Pol_Frog_mean)]), bw = 0.1), col = "chartreuse3")
abline(v=0)
plot(density(unlist(Ov_Frog_mean[upper.tri(Ov_Frog_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(Ov_Frog_mean[upper.tri(Ov_Frog_mean)]), bw = 0.1), col = "cyan")
abline(v=0)
plot(density(unlist(OvNS_Frog_mean[upper.tri(OvNS_Frog_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Frog_mean[upper.tri(OvNS_Frog_mean)]), bw = 0.1), col = "darkorchid1")
abline(v=0)

# Column 5: Fungi

plot(density(unlist(BC_Fun_mean[upper.tri(BC_Fun_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Fun_mean[upper.tri(BC_Fun_mean)]), bw = 0.1), col = "brown1")
abline(v=0)
mtext(side = 3, "Fungi", line = 2)
plot(density(unlist(Pol_Fun_mean[upper.tri(Pol_Fun_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Fun_mean[upper.tri(Pol_Fun_mean)]), bw = 0.1), col = "chartreuse3")
abline(v=0)
plot.new()
plot(density(unlist(OvNS_Fun_mean[upper.tri(OvNS_Fun_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Fun_mean[upper.tri(OvNS_Fun_mean)]), bw = 0.1), col = "darkorchid1")
abline(v=0)

# Column 6: Mosquito

plot(density(unlist(BC_Mosq_mean[upper.tri(BC_Mosq_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Mosq_mean[upper.tri(BC_Mosq_mean)]), bw = 0.1), col = "brown1")
abline(v=0)
mtext(side = 3, "Mosquitos", line = 2)
plot(density(unlist(Pol_Mosq_mean[upper.tri(Pol_Mosq_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Mosq_mean[upper.tri(Pol_Mosq_mean)]), bw = 0.1), col = "chartreuse3")
abline(v=0)
plot(density(unlist(Ov_Mosq_mean[upper.tri(Ov_Mosq_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = "")
polygon(density(unlist(Ov_Mosq_mean[upper.tri(Ov_Mosq_mean)]), bw = 0.1), col = "cyan")
abline(v=0)
plot(density(unlist(OvNS_Mosq_mean[upper.tri(OvNS_Mosq_mean)]), bw = 0.1),
     xlim = c(-1,1), ylim = c(0,3), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Mosq_mean[upper.tri(OvNS_Mosq_mean)]), bw = 0.1), col = "darkorchid1")
abline(v=0)

dev.off()

####
#### Quantile range
####

# Blank PNG

png(filename = "Rho_Quantile_Density.png", height = 15, width = 15, units = "in", res = 300)
# Set mosaic 6 rows, 4 columns

par(mfcol = c(4,6))

# Column 1: Birds

plot(density(unlist(BC_Bird_quantile[upper.tri(BC_Bird_quantile)]), bw = 0.05),
     xlim = c(0,1), ylim = c(0,5.5), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Bird_quantile[upper.tri(BC_Bird_quantile)]), bw = 0.05), col = "brown1")
mtext(side = 2, "BayesComm", line = 3)
mtext(side = 3, "Birds", line = 2)
plot.new()
mtext(side = 2, "Pollock", line = 3)
plot.new()
mtext(side = 2, "Ovaskainen 2016 Spatial", line = 3)
plot(density(unlist(OvNS_Bird_quantile[upper.tri(OvNS_Bird_quantile)]), bw = 0.05),
     xlim = c(0,1), ylim = c(0,5.5), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Bird_quantile[upper.tri(OvNS_Bird_quantile)]), bw = 0.05), col = "darkorchid1")
mtext(side = 2, "Ovaskainen 2016 Non-Spatial", line = 3)

# Column 2: Butterflies

plot(density(unlist(BC_But_quantile[upper.tri(BC_But_quantile)]), bw = 0.05),
     xlim = c(0,1), ylim = c(0,5.5), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_But_quantile[upper.tri(BC_But_quantile)]), bw = 0.05), col = "brown1")
mtext(side = 3, "Butterflies", line = 2)
plot.new()
plot.new()
plot(density(unlist(OvNS_But_quantile[upper.tri(OvNS_But_quantile)]), bw = 0.05),
     xlim = c(0,1), ylim = c(0,5.5), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_But_quantile[upper.tri(OvNS_But_quantile)]), bw = 0.05), col = "darkorchid1")

# Column 3: Eucalypts

plot(density(unlist(BC_Euc_quantile[upper.tri(BC_Euc_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,3.5), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Euc_quantile[upper.tri(BC_Euc_quantile)]), bw = 0.05), col = "brown1")
mtext(side = 3, "Eucalypts", line = 2)
plot(density(unlist(Pol_Euc_quantile[upper.tri(Pol_Euc_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,3.5), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Euc_quantile[upper.tri(Pol_Euc_quantile)]), bw = 0.05), col = "chartreuse3")
plot(density(unlist(Ov_Euc_quantile[upper.tri(Ov_Euc_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,3.5), main = "", ylab = "", xlab = "")
polygon(density(unlist(Ov_Euc_quantile[upper.tri(Ov_Euc_quantile)]), bw = 0.05), col = "cyan")
plot(density(unlist(OvNS_Euc_quantile[upper.tri(OvNS_Euc_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,3.5), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Euc_quantile[upper.tri(OvNS_Euc_quantile)]), bw = 0.05), col = "darkorchid1")

# Column 4: Frogs

plot(density(unlist(BC_Frog_quantile[upper.tri(BC_Frog_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Frog_quantile[upper.tri(BC_Frog_quantile)]), bw = 0.05), col = "brown1")
mtext(side = 3, "Frogs", line = 2)
plot(density(unlist(Pol_Frog_quantile[upper.tri(Pol_Frog_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Frog_quantile[upper.tri(Pol_Frog_quantile)]), bw = 0.05), col = "chartreuse3")
plot(density(unlist(Ov_Frog_quantile[upper.tri(Ov_Frog_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(Ov_Frog_quantile[upper.tri(Ov_Frog_quantile)]), bw = 0.05), col = "cyan")
plot(density(unlist(OvNS_Frog_quantile[upper.tri(OvNS_Frog_quantile)]), bw = 0.05),
     xlim = c(0,1.5), ylim = c(0,4), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Frog_quantile[upper.tri(OvNS_Frog_quantile)]), bw = 0.05), col = "darkorchid1")

# Column 5: Fungi

plot(density(unlist(BC_Fun_quantile[upper.tri(BC_Fun_quantile)]), bw = 0.05),
     xlim = c(0,1.2), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Fun_quantile[upper.tri(BC_Fun_quantile)]), bw = 0.05), col = "brown1")
mtext(side = 3, "Fungi", line = 2)
plot(density(unlist(Pol_Fun_quantile[upper.tri(Pol_Fun_quantile)]), bw = 0.05),
     xlim = c(0,1.2), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Fun_quantile[upper.tri(Pol_Fun_quantile)]), bw = 0.05), col = "chartreuse3")
plot.new()
plot(density(unlist(OvNS_Fun_quantile[upper.tri(OvNS_Fun_quantile)]), bw = 0.05),
     xlim = c(0,1.2), ylim = c(0,4), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Fun_quantile[upper.tri(OvNS_Fun_quantile)]), bw = 0.05), col = "darkorchid1")

# Column 6: Mosquito

plot(density(unlist(BC_Mosq_quantile[upper.tri(BC_Mosq_quantile)]), bw = 0.05),
     xlim = c(0,1.2), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(BC_Mosq_quantile[upper.tri(BC_Mosq_quantile)]), bw = 0.05), col = "brown1")
mtext(side = 3, "Mosquitos", line = 2)
plot(density(unlist(Pol_Mosq_quantile[upper.tri(Pol_Mosq_quantile)]), bw = 0.05),
     xlim = c(0,1.2), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(Pol_Mosq_quantile[upper.tri(Pol_Mosq_quantile)]), bw = 0.05), col = "chartreuse3")
plot(density(unlist(Ov_Mosq_quantile[upper.tri(Ov_Mosq_quantile)]), bw = 0.05),
     xlim = c(0,1.2), ylim = c(0,4), main = "", ylab = "", xlab = "")
polygon(density(unlist(Ov_Mosq_quantile[upper.tri(Ov_Mosq_quantile)]), bw = 0.05), col = "cyan")
plot(density(unlist(OvNS_Mosq_quantile[upper.tri(OvNS_Mosq_quantile)]), bw = 0.05),
     xlim = c(0,1.2), ylim = c(0,4), main = "", ylab = "", xlab = NULL)
polygon(density(unlist(OvNS_Mosq_quantile[upper.tri(OvNS_Mosq_quantile)]), bw = 0.05), col = "darkorchid1")

dev.off()



