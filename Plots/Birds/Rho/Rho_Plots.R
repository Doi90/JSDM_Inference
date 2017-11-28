###########################
###### Load Packages ######
###########################

library(corrplot)
library(openxlsx)
library(ggplot2)
library(RColorBrewer)

###########################
###### Read in csvs #######
###########################

# # Pollock mean/lower/upper/uncertainty
# 
# Pollock_Rho_Mean <- read.csv("Rho_mean_Eucalypt_Pollock.csv")
# rownames(Pollock_Rho_Mean) <- Pollock_Rho_Mean[,1]
# Pollock_Rho_Mean <- Pollock_Rho_Mean[,-1]
# Pollock_Rho_Mean <- as.matrix(Pollock_Rho_Mean)
# colnames(Pollock_Rho_Mean) <- rownames(Pollock_Rho_Mean) <- 1:ncol(Pollock_Rho_Mean)
# 
# Pollock_Rho_Lower <- read.csv("Rho_lower_Eucalypt_Pollock.csv")
# rownames(Pollock_Rho_Lower) <- Pollock_Rho_Lower[,1]
# Pollock_Rho_Lower <- Pollock_Rho_Lower[,-1]
# Pollock_Rho_Lower <- as.matrix(Pollock_Rho_Lower)
# colnames(Pollock_Rho_Lower) <- rownames(Pollock_Rho_Lower) <- 1:ncol(Pollock_Rho_Lower)
# 
# Pollock_Rho_Upper <- read.csv("Rho_upper_Eucalypt_Pollock.csv")
# rownames(Pollock_Rho_Upper) <- Pollock_Rho_Upper[,1]
# Pollock_Rho_Upper <- Pollock_Rho_Upper[,-1]
# Pollock_Rho_Upper <- as.matrix(Pollock_Rho_Upper)
# colnames(Pollock_Rho_Upper) <- rownames(Pollock_Rho_Upper) <- 1:ncol(Pollock_Rho_Upper)
# 
# Uncertainty_Pollock <- Pollock_Rho_Upper - Pollock_Rho_Lower
# colnames(Uncertainty_Pollock) <- rownames(Uncertainty_Pollock) <- 1:ncol(Uncertainty_Pollock)
# Uncertainty_Pollock <- as.matrix(Uncertainty_Pollock)

# BayesComm mean/lower/upper/uncertainty

BayesComm_Rho_Mean <- read.csv("Rho_mean_Bird_BayesComm.csv")
BayesComm_Rho_Mean <- BayesComm_Rho_Mean[,-1]
BayesComm_Rho_Mean <- as.matrix(BayesComm_Rho_Mean)
colnames(BayesComm_Rho_Mean) <- rownames(BayesComm_Rho_Mean) <- 1:ncol(BayesComm_Rho_Mean)

BayesComm_Rho_Lower <- read.csv("Rho_lower_Bird_BayesComm.csv")
BayesComm_Rho_Lower <- BayesComm_Rho_Lower[,-1]
BayesComm_Rho_Lower <- as.matrix(BayesComm_Rho_Lower)
colnames(BayesComm_Rho_Lower) <- rownames(BayesComm_Rho_Lower) <- 1:ncol(BayesComm_Rho_Lower)

BayesComm_Rho_upper <- read.csv("Rho_upper_Bird_BayesComm.csv")
BayesComm_Rho_upper <- BayesComm_Rho_upper[,-1]
BayesComm_Rho_upper <- as.matrix(BayesComm_Rho_upper)
colnames(BayesComm_Rho_upper) <- rownames(BayesComm_Rho_upper) <- 1:ncol(BayesComm_Rho_upper)

Uncertainty_BayesComm <- BayesComm_Rho_upper - BayesComm_Rho_Lower
colnames(Uncertainty_BayesComm) <- rownames(Uncertainty_BayesComm) <- 1:ncol(Uncertainty_BayesComm)
Uncertainty_BayesComm <- as.matrix(Uncertainty_BayesComm)

# # Ovaskainen mean/lower/upper/uncertainty
# 
# Ovaskainen_Rho_Mean <- read.xlsx("R1_Bird_Ovaskainen.xlsx",sheetIndex = 1, header = T)
# rownames(Ovaskainen_Rho_Mean) <- Ovaskainen_Rho_Mean[,1]
# Ovaskainen_Rho_Mean <- Ovaskainen_Rho_Mean[,-1]
# Ovaskainen_Rho_Mean <- as.matrix(Ovaskainen_Rho_Mean)
# colnames(Ovaskainen_Rho_Mean) <- rownames(Ovaskainen_Rho_Mean) <- 1:ncol(Ovaskainen_Rho_Mean)
# 
# Ovaskainen_Rho_Lower <- read.xlsx("R1_Bird_Ovaskainen.xlsx",sheetIndex = 5, header = T)
# rownames(Ovaskainen_Rho_Lower) <- Ovaskainen_Rho_Lower[,1]
# Ovaskainen_Rho_Lower <- Ovaskainen_Rho_Lower[,-1]
# Ovaskainen_Rho_Lower <- as.matrix(Ovaskainen_Rho_Lower)
# colnames(Ovaskainen_Rho_Lower) <- rownames(Ovaskainen_Rho_Lower) <- 1:ncol(Ovaskainen_Rho_Lower)
# 
# Ovaskainen_Rho_Upper <- read.xlsx("R1_Bird_Ovaskainen.xlsx",sheetIndex = 6, header = T)
# rownames(Ovaskainen_Rho_Upper) <- Ovaskainen_Rho_Upper[,1]
# Ovaskainen_Rho_Upper <- Ovaskainen_Rho_Upper[,-1]
# Ovaskainen_Rho_Upper <- as.matrix(Ovaskainen_Rho_Upper)
# colnames(Ovaskainen_Rho_Upper) <- rownames(Ovaskainen_Rho_Upper) <- 1:ncol(Ovaskainen_Rho_Upper)
# 
# Uncertainty_Ovaskainen <- Ovaskainen_Rho_Upper - Ovaskainen_Rho_Lower
# colnames(Uncertainty_Ovaskainen) <- rownames(Uncertainty_Ovaskainen) <- 1:ncol(Uncertainty_Ovaskainen)
# Uncertainty_Ovaskainen <- as.matrix(Uncertainty_Ovaskainen)

# Ovaskainen mean/lower/upper/uncertainty

OvaskainenNS_Rho_Mean <- openxlsx::read.xlsx("R1_Bird_OvaskainenNS.xlsx",
                                             sheet = 1,
                                             colNames = TRUE)
OvaskainenNS_Rho_Mean <- OvaskainenNS_Rho_Mean[,-1]
OvaskainenNS_Rho_Mean <- as.matrix(OvaskainenNS_Rho_Mean)
colnames(OvaskainenNS_Rho_Mean) <- rownames(OvaskainenNS_Rho_Mean) <- 1:ncol(OvaskainenNS_Rho_Mean)

OvaskainenNS_Rho_Lower <- openxlsx::read.xlsx("R1_Bird_OvaskainenNS.xlsx",
                                              sheet = 5,
                                              colNames = TRUE)
OvaskainenNS_Rho_Lower <- OvaskainenNS_Rho_Lower[,-1]
OvaskainenNS_Rho_Lower <- as.matrix(OvaskainenNS_Rho_Lower)
colnames(OvaskainenNS_Rho_Lower) <- rownames(OvaskainenNS_Rho_Lower) <- 1:ncol(OvaskainenNS_Rho_Lower)

OvaskainenNS_Rho_Upper <- openxlsx::read.xlsx("R1_Bird_OvaskainenNS.xlsx",
                                              sheet = 6,
                                              colNames = TRUE)
OvaskainenNS_Rho_Upper <- OvaskainenNS_Rho_Upper[,-1]
OvaskainenNS_Rho_Upper <- as.matrix(OvaskainenNS_Rho_Upper)
colnames(OvaskainenNS_Rho_Upper) <- rownames(OvaskainenNS_Rho_Upper) <- 1:ncol(OvaskainenNS_Rho_Upper)

Uncertainty_OvaskainenNS <- OvaskainenNS_Rho_Upper - OvaskainenNS_Rho_Lower
colnames(Uncertainty_OvaskainenNS) <- rownames(Uncertainty_OvaskainenNS) <- 1:ncol(Uncertainty_OvaskainenNS)
Uncertainty_OvaskainenNS <- as.matrix(Uncertainty_OvaskainenNS)

# Clark mean/lower/upper/uncertainty

Clark_Rho_Mean <- read.csv("Rho_mean_Bird_Clark.csv")
Clark_Rho_Mean <- Clark_Rho_Mean[,-1]
Clark_Rho_Mean <- as.matrix(Clark_Rho_Mean)
colnames(Clark_Rho_Mean) <- rownames(Clark_Rho_Mean) <- 1:ncol(Clark_Rho_Mean)

Clark_Rho_Lower <- read.csv("Rho_lower_Bird_Clark.csv")
Clark_Rho_Lower <- Clark_Rho_Lower[,-1]
Clark_Rho_Lower <- as.matrix(Clark_Rho_Lower)
colnames(Clark_Rho_Lower) <- rownames(Clark_Rho_Lower) <- 1:ncol(Clark_Rho_Lower)

Clark_Rho_upper <- read.csv("Rho_upper_Bird_Clark.csv")
Clark_Rho_upper <- Clark_Rho_upper[,-1]
Clark_Rho_upper <- as.matrix(Clark_Rho_upper)
colnames(Clark_Rho_upper) <- rownames(Clark_Rho_upper) <- 1:ncol(Clark_Rho_upper)

Uncertainty_Clark <- Clark_Rho_upper - Clark_Rho_Lower
colnames(Uncertainty_Clark) <- rownames(Uncertainty_Clark) <- 1:ncol(Uncertainty_Clark)
Uncertainty_Clark <- as.matrix(Uncertainty_Clark)

# boral mean/lower/upper/uncertainty

boral_Rho_Mean <- read.csv("Rho_mean_Bird_boral.csv")
rownames(boral_Rho_Mean) <- boral_Rho_Mean[,1]
boral_Rho_Mean <- boral_Rho_Mean[,-1]
boral_Rho_Mean <- as.matrix(boral_Rho_Mean)
colnames(boral_Rho_Mean) <- rownames(boral_Rho_Mean) <- 1:ncol(boral_Rho_Mean)

boral_Rho_Lower <- read.csv("Rho_lower_Bird_boral.csv")
rownames(boral_Rho_Lower) <- boral_Rho_Lower[,1]
boral_Rho_Lower <- boral_Rho_Lower[,-1]
boral_Rho_Lower <- as.matrix(boral_Rho_Lower)
colnames(boral_Rho_Lower) <- rownames(boral_Rho_Lower) <- 1:ncol(boral_Rho_Lower)

boral_Rho_upper <- read.csv("Rho_upper_Bird_boral.csv")
rownames(boral_Rho_upper) <- boral_Rho_upper[,1]
boral_Rho_upper <- boral_Rho_upper[,-1]
boral_Rho_upper <- as.matrix(boral_Rho_upper)
colnames(boral_Rho_upper) <- rownames(boral_Rho_upper) <- 1:ncol(boral_Rho_upper)

Uncertainty_boral <- boral_Rho_upper - boral_Rho_Lower
colnames(Uncertainty_boral) <- rownames(Uncertainty_boral) <- 1:ncol(Uncertainty_boral)
Uncertainty_boral <- as.matrix(Uncertainty_boral)

###########################
#### Correlation plots ####
###########################

#png(filename = "Correlation Plot Frogs.png", units = "in", width = 12, height = 7, res = 72)

pdf(file = "Correlation Plot Birds.pdf", width = 14, height = 5.5)
par(mfrow = c(2,4), mar = c(4,2,4,2), oma = c(0.5,3,0,3))

corrplot(BayesComm_Rho_Mean,                       # BayesComm correlation
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")
mtext("MPR", side = 3, line = 2.5)
mtext("Correlation", side = 2, outer = F, line = 2)

# corrplot(Pollock_Rho_Mean,                          # Pollock correlation
#          type = "upper",
#          method = "color",
#          tl.col = "black",
#          tl.srt = 0,
#          tl.cex = 1.2,
#          tl.offset = 0.6,
#          diag = F,
#          outline = T,
#          cl.cex = 1,
#          cl.align.text = "l",
#          cl.pos = "n")
# mtext("Pollock", side = 3, line = 2.5)

corrplot(boral_Rho_Mean,                          # boral correlation
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")
mtext("LPR", side = 3, line = 2.5)

corrplot(Clark_Rho_Mean,                          # Clark correlation
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")
mtext("DPR", side = 3, line = 2.5)

# corrplot(Ovaskainen_Rho_Mean,                       # Ovaskainen correlation
#          type = "upper",
#          method = "color",
#          tl.col = "black",
#          tl.srt = 0,
#          tl.cex = 1.2,
#          tl.offset = 0.6,
#          diag = F,
#          outline = T,
#          cl.cex = 1,
#          cl.align.text = "l",
#          cl.pos = "n")
# mtext("Ovaskainen", side = 3, line = 2.5)

corrplot(OvaskainenNS_Rho_Mean,                     # OvaskainenNS correlation
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")
mtext("HLR-NS", side = 3, line = 2.5)

# Uncertainty plots

corrplot(Uncertainty_BayesComm,                     # BayesComm uncertainty
         is.corr = F,
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         cl.lim = c(0,1.5),
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")
mtext("Uncertainty", side = 2, outer = F, line = 2)

# corrplot(Uncertainty_Pollock,                       # Pollock uncertainty
#          is.corr = F,
#          type = "upper",
#          method = "color",
#          tl.col = "black",
#          tl.srt = 0,
#          tl.cex = 1.2,
#          tl.offset = 0.6,
#          diag = F,
#          cl.lim = c(0,1.5),
#          outline = T,
#          cl.cex = 1,
#          cl.align.text = "l",
#          cl.pos = "n")

corrplot(Uncertainty_boral,                       # boral uncertainty
         is.corr = F,
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         cl.lim = c(0,2),
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")

corrplot(Uncertainty_Clark,                       # Clark uncertainty
         is.corr = F,
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         cl.lim = c(0,1.5),
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")

# corrplot(Uncertainty_Ovaskainen,                    # Ovaskainen uncertainty
#          is.corr = F,
#          type = "upper",
#          method = "color",
#          tl.col = "black",
#          tl.srt = 0,
#          tl.cex = 1.2,
#          tl.offset = 0.6,
#          diag = F,
#          cl.lim = c(0,1.5),
#          outline = T,
#          cl.cex = 1,
#          cl.align.text = "l",
#          cl.pos = "n")

corrplot(Uncertainty_OvaskainenNS,                  # OvaskainenNS uncertainty
         is.corr = F,
         type = "upper",
         method = "color",
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         diag = F,
         cl.lim = c(0,1.5),
         outline = T,
         cl.cex = 1,
         cl.align.text = "l",
         cl.pos = "n")

dev.off()
