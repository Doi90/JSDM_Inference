#####################
### Load Packages ###
#####################

library(ggplot2)
library(RColorBrewer)
library(cowplot)

#################
### Load Data ###
################

## BayesComm ----

BayesComm_ESS_Beta <- read.csv("ess_Mosquito_BayesComm.csv", header = TRUE)
BayesComm_ESS_Beta <- BayesComm_ESS_Beta[,-1]

BayesComm_ESS_Rho <- read.csv("essRho_Mosquito_BayesComm.csv", header = TRUE)
BayesComm_ESS_Rho <- BayesComm_ESS_Rho[,-1]

## boral ----

boral_ESS_Beta <- read.csv("ess_beta_Mosq_boral.csv", header = TRUE)
boral_ESS_Beta <- boral_ESS_Beta[,-1]

boral_ESS_Rho <- read.csv("ess_Rho_Mosq_boral.csv", header = TRUE)
boral_ESS_Rho <- boral_ESS_Rho[,-1]

## Clark ----

Clark_ESS_Beta <- read.csv("ess_Mosquito_Clark.csv", header = TRUE)
Clark_ESS_Beta <- Clark_ESS_Beta[,-1]

Clark_ESS_Rho <- read.csv("ess_Rho_Mosquito_Clark.csv", header = TRUE)
Clark_ESS_Rho <- Clark_ESS_Rho[,-1]

## Ovaskainen ----

Ov_ESS_Beta <- read.csv("ess_beta_Mosquito_Ov.csv", header = TRUE)
Ov_ESS_Beta <- Ov_ESS_Beta[,-1]

Ov_ESS_Rho <- read.csv("ess_rho_Mosquito_Ov.csv", header = TRUE)
Ov_ESS_Rho <- Ov_ESS_Rho[,-1]

## Ovaskainen NS ----

OvNS_ESS_Beta <- read.csv("ess_beta_Mosquito_OvNS.csv", header = TRUE)
OvNS_ESS_Beta <- OvNS_ESS_Beta[,-1]

OvNS_ESS_Rho <- read.csv("ess_rho_Mosquito_OvNS.csv", header = TRUE)
OvNS_ESS_Rho <- OvNS_ESS_Rho[,-1]

## Pollock ----

Pollock_ESS_Beta <- read.csv("ess_Mosquito_Pollock.csv", header = TRUE)
Pollock_ESS_Beta <- Pollock_ESS_Beta[,-1]

Pollock_ESS_Rho <- read.csv("ess_Rho_Mosquito_Pollock.csv", header = TRUE)
Pollock_ESS_Rho <- Pollock_ESS_Rho[,-1]

# ----

##########################
### Set Colour Palette ###
##########################

col_palette <- brewer.pal(6, "Dark2")

################
### ESS Beta ###
################

## Convert to vectors ----

BayesComm_ESS_Beta <- as.vector(unlist(BayesComm_ESS_Beta))
boral_ESS_Beta <- as.vector(unlist(boral_ESS_Beta))
Clark_ESS_Beta <- as.vector(unlist(Clark_ESS_Beta))
Ov_ESS_Beta <- as.vector(unlist(Ov_ESS_Beta))
OvNS_ESS_Beta <- as.vector(unlist(OvNS_ESS_Beta))
Pollock_ESS_Beta <- as.vector(unlist(Pollock_ESS_Beta))

## Create Data Frame ----

ESS_Beta <- data.frame(BayesComm = BayesComm_ESS_Beta,
                       boral = boral_ESS_Beta,
                       Clark = Clark_ESS_Beta,
                       Ovaskainen = Ov_ESS_Beta,
                       OvaskainenNS = OvNS_ESS_Beta,
                       Pollock = Pollock_ESS_Beta)

## Correct for processing time (to ESS per hour) ----

ESS_Beta$BayesComm <- ESS_Beta$BayesComm / (28.21/60/60)
ESS_Beta$boral <- ESS_Beta$boral / (298.1/60/60)
ESS_Beta$Clark <- ESS_Beta$Clark / (474.48/60/60)
ESS_Beta$Ovaskainen <- ESS_Beta$Ovaskainen / (830.157178/60/60)
ESS_Beta$OvaskainenNS <- ESS_Beta$OvaskainenNS / (135.884902/60/60)
ESS_Beta$Pollock <- ESS_Beta$Pollock / (8.67539/60)

## Convert to ggplot dataframe format ----

ESS_Beta_ggplot <- data.frame(ESS = c(ESS_Beta$BayesComm,
                                      ESS_Beta$Pollock,
                                      ESS_Beta$boral,
                                      ESS_Beta$Clark,
                                      ESS_Beta$Ovaskainen,
                                      ESS_Beta$OvaskainenNS),
                              Model = c(rep("MPR", length(ESS_Beta$BayesComm)),
                                        rep("HPR", length(ESS_Beta$Pollock)),
                                        rep("LPR", length(ESS_Beta$boral)),
                                        rep("DPR", length(ESS_Beta$Clark)),
                                        rep("HLR-S", length(ESS_Beta$Ovaskainen)),
                                        rep("HLR-NS", length(ESS_Beta$OvaskainenNS)))) 

ESS_Beta_ggplot$Model <- factor(ESS_Beta_ggplot$Model,
                                levels = c("MPR","HPR","LPR","DPR",
                                           "HLR-S","HLR-NS"))
## Barplot ----

essBeta <- ggplot(ESS_Beta_ggplot, aes(x = Model, y = ESS)) +
  geom_boxplot(aes(fill = Model), colour = "black",
               outlier.size = 0.5) +
  scale_fill_manual(values = rev(col_palette)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Model",
       y = "Effective sample size") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

essBeta

ggsave(filename = "ESS_Beta.pdf", width = 7, height = 7, units = "in")

# ----

###############
### ESS Rho ###
###############

## Convert to vectors ----

# BayesComm_ESS_Rho already in format 
boral_ESS_Rho <- boral_ESS_Rho[upper.tri(boral_ESS_Rho)]
Clark_ESS_Rho <- Clark_ESS_Rho[upper.tri(Clark_ESS_Rho)]
Ov_ESS_Rho <- Ov_ESS_Rho[upper.tri(Ov_ESS_Rho)]
OvNS_ESS_Rho <- OvNS_ESS_Rho[upper.tri(OvNS_ESS_Rho)]
Pollock_ESS_Rho <- Pollock_ESS_Rho[upper.tri(Pollock_ESS_Rho)]

## Create Data Frame ----

ESS_Rho <- data.frame(BayesComm = BayesComm_ESS_Rho,
                      boral = boral_ESS_Rho,
                      Clark = Clark_ESS_Rho,
                      Ovaskainen = Ov_ESS_Rho,
                      OvaskainenNS = OvNS_ESS_Rho,
                      Pollock = Pollock_ESS_Rho)

## Correct for processing time (to ESS per hour) ----

ESS_Rho$BayesComm <- ESS_Rho$BayesComm / (28.21/60/60)
ESS_Rho$boral <- ESS_Rho$boral / (298.1/60/60)
ESS_Rho$Clark <- ESS_Rho$Clark / (474.48/60/60)
ESS_Rho$Ovaskainen <- ESS_Rho$Ovaskainen / (830.157178/60/60)
ESS_Rho$OvaskainenNS <- ESS_Rho$OvaskainenNS / (135.884902/60/60)
ESS_Rho$Pollock <- ESS_Rho$Pollock / (8.67539/60)

## Convert to ggplot dataframe format ----

ESS_Rho_ggplot <- data.frame(ESS = c(ESS_Rho$BayesComm,
                                     ESS_Rho$Pollock,
                                     ESS_Rho$boral,
                                     ESS_Rho$Clark,
                                     ESS_Rho$Ovaskainen,
                                     ESS_Rho$OvaskainenNS),
                             Model = c(rep("MPR", length(ESS_Rho$BayesComm)),
                                       rep("HPR", length(ESS_Rho$Pollock)),
                                       rep("LPR", length(ESS_Rho$boral)),
                                       rep("DPR", length(ESS_Rho$Clark)),
                                       rep("HLR-S", length(ESS_Rho$Ovaskainen)),
                                       rep("HLR-NS", length(ESS_Rho$OvaskainenNS)))) 

ESS_Rho_ggplot$Model <- factor(ESS_Rho_ggplot$Model,
                               levels = c("MPR","HPR","LPR","DPR",
                                          "HLR-S","HLR-NS"))

## Barplot ----

essRho <- ggplot(ESS_Rho_ggplot, aes(x = Model, y = ESS)) +
  geom_boxplot(aes(fill = Model), colour = "black",
               outlier.size = 0.5) +
  scale_fill_manual(values = rev(col_palette)) +
  scale_y_continuous(limits = c(0, 1000000),
                     labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Model",
       y = "Effective sample size")

essRho

ggsave(filename = "ESS_Rho.pdf", width = 7, height = 7, units = "in")

# ----

## Both plots in one image

plot_grid(essBeta, essRho, align = "h", labels = "auto")
ggsave(filename = "essSideBySide.pdf", width = 8, height = 4, units = "in")

