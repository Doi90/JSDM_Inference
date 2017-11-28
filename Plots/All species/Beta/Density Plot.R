####################################
############ Load Data #############
####################################

# BayesComm

BC_Bird <- read.csv("Beta_df_Birds_BayesComm.csv")
BC_But <- read.csv("Beta_df_Butterfly_BayesComm.csv")
BC_Euc <- read.csv("Beta_df_Eucalypt_BayesComm.csv")
BC_Frog <- read.csv("Beta_df_Frog_BayesComm.csv")
BC_Fun <- read.csv("Beta_df_Fungi_BayesComm.csv")
BC_Mosq <- read.csv("Beta_df_Mosquito_BayesComm.csv")

# Pollock

Pol_Euc <- read.csv("Beta_Eucalypt_Pollock.csv")
Pol_Frog <- read.csv("Beta_Frog_Pollock.csv")
Pol_Fun <- read.csv("Beta_Fungi_Pollock.csv")
Pol_Mosq <- read.csv("Beta_Mosquito_Pollock.csv")

# Ovaskainen 2016

Ov_Euc <- read.csv("Beta_Eucalypt_Ovaskainen2016.csv")
Ov_Frog <- read.csv("Beta_Frog_Ovaskainen2016.csv")
Ov_Mosq <- read.csv("Beta_Mosquito_Ovaskainen2016.csv")

# Ovaskainen 2016 NS

OvNS_Bird <- read.csv("Beta_Bird_Ovaskainen2016NS.csv")
OvNS_But <- read.csv("Beta_Butterfly_Ovaskainen2016NS.csv")
OvNS_Euc <- read.csv("Beta_Eucalypt_Ovaskainen2016NS.csv")
OvNS_Frog <- read.csv("Beta_Frog_Ovaskainen2016NS.csv")
OvNS_Fun <- read.csv("Beta_Fungi_Ovaskainen2016NS.csv")
OvNS_Mosq <- read.csv("Beta_Mosquito_Ovaskainen2016NS.csv")

# Clark

Clark_But <- read.csv("Beta_Butterflies_Clark.csv")
Clark_Euc <- read.csv("Beta_Eucalypts_Clark.csv")
Clark_Frog <- read.csv("Beta_Frog_Clark.csv")
Clark_Fun <- read.csv("Beta_Fungi_Clark.csv")
Clark_Mosq <- read.csv("Beta_Mosquito_Clark.csv")

# boral

boral_Euc <- read.csv("Beta_Eucalypt_boral.csv")
boral_Frog <- read.csv("Beta_Frog_boral.csv")
boral_Fun <- read.csv("Beta_Fungi_boral.csv")
boral_Mosq <- read.csv("Beta_Mosquito_boral.csv")

######################################
######## Density Plot Mosaic #########
######################################

####
#### Beta mean
####

# Blank PNG

png(filename = "Beta_Mean_Density.png", height = 15, width = 15, units = "in", res = 300)
# Set mosaic 6 rows, 4 columns

par(mfcol = c(4,6))

# Column 1: Birds

plot(density(BC_Bird$posterior.mean, bw = 1), xlim = c(-10,10), ylim = c(0,0.4), main = "", ylab = "", xlab = "")
polygon(density(BC_Bird$posterior.mean, bw = 1), col = "brown1")
mtext(side = 2, "BayesComm", line = 3)
mtext(side = 3, "Birds", line = 2)
abline(v = 0)
plot.new()
mtext(side = 2, "Pollock", line = 3)
plot.new()
mtext(side = 2, "Ovaskainen 2016 Spatial", line = 3)
plot(density(OvNS_Bird$posterior.mean, bw = 1), xlim = c(-10,10), ylim = c(0,0.4), main = "", ylab = "")
polygon(density(OvNS_Bird$posterior.mean, bw = 1), col = "darkorchid1")
mtext(side = 2, "Ovaskainen 2016 Non-Spatial", line = 3)
abline(v = 0)

# Column 2: Butterflies

plot(density(BC_But$posterior.mean, bw = 1), xlim = c(-10,20), ylim = c(0,0.2), main = "", ylab = "", xlab = "")
polygon(density(BC_But$posterior.mean, bw = 1), col = "brown1")
mtext(side = 3, "Butterflies", line = 2)
abline(v = 0)
plot.new()
plot.new()
plot(density(OvNS_But$posterior.mean, bw = 1), xlim = c(-10,20), ylim = c(0,0.2), main = "", ylab = "")
polygon(density(OvNS_But$posterior.mean, bw = 1), col = "darkorchid1")
abline(v = 0)

# Column 3: Eucalypts

plot(density(BC_Euc$posterior.mean, bw = 1), xlim = c(-10,10), ylim = c(0,0.4), main = "", ylab = "", xlab = "")
polygon(density(BC_Euc$posterior.mean, bw = 1), col = "brown1")
abline(v = 0)
mtext(side = 3, "Eucalypts", line = 2)
plot(density(Pol_Euc$posterior.mean, bw = 1), xlim = c(-10,10), ylim = c(0,0.4), main = "", ylab = "", xlab = "")
polygon(density(Pol_Euc$posterior.mean, bw = 1), col = "chartreuse3")
abline(v = 0)
plot(density(Ov_Euc$posterior.mean, bw = 1), xlim = c(-10,10), ylim = c(0,0.4), main = "", ylab = "", xlab = "")
polygon(density(Ov_Euc$posterior.mean, bw = 1), col = "cyan")
abline(v = 0)
plot(density(OvNS_Euc$posterior.mean, bw = 1), xlim = c(-10,10), ylim = c(0,0.4), main = "", ylab = "")
polygon(density(OvNS_Euc$posterior.mean, bw = 1), col = "darkorchid1")
abline(v = 0)

# Column 4: Frogs

plot(density(BC_Frog$posterior.mean, bw = 1), xlim = c(-20,5), ylim = c(0,0.3), main = "", ylab = "", xlab = "")
polygon(density(BC_Frog$posterior.mean, bw = 1), col = "brown1")
mtext(side = 3, "Frogs", line = 2)
abline(v = 0)
plot(density(Pol_Frog$posterior.mean, bw = 1), xlim = c(-20,5), ylim = c(0,0.3), main = "", ylab = "", xlab = "")
polygon(density(Pol_Frog$posterior.mean, bw = 1), col = "chartreuse3")
abline(v = 0)
plot(density(Ov_Frog$posterior.mean, bw = 1), xlim = c(-20,5), ylim = c(0,0.3), main = "", ylab = "", xlab = "")
polygon(density(Ov_Frog$posterior.mean, bw = 1), col = "cyan")
abline(v = 0)
plot(density(OvNS_Frog$posterior.mean, bw = 1), xlim = c(-20,5), ylim = c(0,0.3), main = "", ylab = "")
polygon(density(OvNS_Frog$posterior.mean, bw = 1), col = "darkorchid1")
abline(v = 0)

# Column 5: Fungi

plot(density(BC_Fun$posterior.mean, bw = 1), xlim = c(-6,4), ylim = c(0,0.35), main = "", ylab = "", xlab = "")
polygon(density(BC_Fun$posterior.mean, bw = 1), col = "brown1")
mtext(side = 3, "Fungi", line = 2)
abline(v = 0)
plot(density(Pol_Fun$posterior.mean, bw = 1), xlim = c(-6,4), ylim = c(0,0.35), main = "", ylab = "", xlab = "")
polygon(density(Pol_Fun$posterior.mean, bw = 1), col = "chartreuse3")
abline(v = 0)
plot.new()
plot(density(OvNS_Fun$posterior.mean, bw = 1), xlim = c(-6,4), ylim = c(0,0.35), main = "", ylab = "")
polygon(density(OvNS_Fun$posterior.mean, bw = 1), col = "darkorchid1")
abline(v = 0)

# Column 6: Mosquito

plot(density(BC_Mosq$posterior.mean, bw = 1), xlim = c(-7,3), ylim = c(0,0.4), main = "", ylab = "", xlab = "")
polygon(density(BC_Mosq$posterior.mean, bw = 1), col = "brown1")
mtext(side = 3, "Mosquito", line = 2)
abline(v = 0)
plot(density(Pol_Mosq$posterior.mean, bw = 1), xlim = c(-7,3), ylim = c(0,0.4), main = "", ylab = "", xlab = "")
polygon(density(Pol_Mosq$posterior.mean, bw = 1), col = "chartreuse3")
abline(v = 0)
plot(density(Ov_Mosq$posterior.mean, bw = 1), xlim = c(-7,3), ylim = c(0,0.4), main = "", ylab = "", xlab = "")
polygon(density(Ov_Mosq$posterior.mean, bw = 1), col = "cyan")
abline(v = 0)
plot(density(OvNS_Mosq$posterior.mean, bw = 1), xlim = c(-7,3), ylim = c(0,0.4), main = "", ylab = "")
polygon(density(OvNS_Mosq$posterior.mean, bw = 1), col = "darkorchid1")
abline(v = 0)

dev.off()


####
#### Beta quantile range
####

# Blank PNG

png(filename = "Beta_Quantile_Density.png", height = 15, width = 15, units = "in", res = 300)
# Set mosaic 6 rows, 4 columns

par(mfcol = c(4,6))

# Column 1: Birds

plot(density((BC_Bird$upper - BC_Bird$lower), bw = 0.1), xlim = c(0,5), ylim = c(0,3.5),main = "", ylab = "", xlab = "")
mtext(side = 2, "BayesComm", line = 3)
mtext(side = 3, "Birds", line = 2)
plot.new()
mtext(side = 2, "Pollock", line = 3)
plot.new()
mtext(side = 2, "Ovaskainen 2016 Spatial", line = 3)
plot(density((OvNS_Bird$upper - OvNS_Bird$lower), bw = 0.1), xlim = c(0,5), ylim = c(0,3.5), main = "", ylab = "")
mtext(side = 2, "Ovaskainen 2016 Non-Spatial", line = 3)

# Column 2: Butterflies

plot(density((BC_But$upper - BC_But$lower), bw = 0.1), xlim = c(0,12), ylim = c(0,1), main = "", ylab = "", xlab = "")
mtext(side = 3, "Butterflies", line = 2)
plot.new()
plot.new()
plot(density((OvNS_But$upper - OvNS_But$lower), bw = 0.1), xlim = c(0,12), ylim = c(0,1), main = "", ylab = "")

# Column 3: Eucalypts

plot(density((BC_Euc$upper - BC_Euc$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2), main = "", ylab = "", xlab = "")
mtext(side = 3, "Eucalypts", line = 2)
plot(density((Pol_Euc$upper - Pol_Euc$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2), main = "", ylab = "", xlab = "")
plot(density((Ov_Euc$upper - Ov_Euc$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2), main = "", ylab = "", xlab = "")
plot(density((OvNS_Euc$upper - OvNS_Euc$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2), main = "", ylab = "")

# Column 4: Frogs

plot(density((BC_Frog$upper - BC_Frog$lower), bw = 0.5), xlim = c(0,30), ylim = c(0,0.5), main = "", ylab = "", xlab = "")
mtext(side = 3, "Frogs", line = 2)
plot(density((Pol_Frog$upper - Pol_Frog$lower), bw = 0.5), xlim = c(0,30), ylim = c(0,0.5), main = "", ylab = "", xlab = "")
plot(density((Ov_Frog$upper - Ov_Frog$lower), bw = 0.5), xlim = c(0,30), ylim = c(0,0.5), main = "", ylab = "", xlab = "")
plot(density((OvNS_Frog$upper - OvNS_Frog$lower), bw = 0.5), xlim = c(0,30), ylim = c(0,0.5), main = "", ylab = "")

# Column 5: Fungi

plot(density((BC_Fun$upper - BC_Fun$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,1.2), main = "", ylab = "", xlab = "")
mtext(side = 3, "Fungi", line = 2)
plot(density((Pol_Fun$upper - Pol_Fun$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,1.2), main = "", ylab = "", xlab = "")
plot.new()
plot(density((OvNS_Fun$upper - OvNS_Fun$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,1.2), main = "", ylab = "")

# Column 6: Mosquito

plot(density((BC_Mosq$upper - BC_Mosq$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2.5), main = "", ylab = "", xlab = "")
mtext(side = 3, "Mosquito", line = 2)
plot(density((Pol_Mosq$upper - Pol_Mosq$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2.5), main = "", ylab = "", xlab = "")
plot(density((Ov_Mosq$upper - Ov_Mosq$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2.5), main = "", ylab = "", xlab = "")
plot(density((OvNS_Mosq$upper - OvNS_Mosq$lower), bw = 0.1), xlim = c(0,15), ylim = c(0,2.5), main = "", ylab = "")


dev.off()

