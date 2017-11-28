library(ggplot2)

###################
#### Load data ####
###################

# BayesComm

ess_beta_BayesComm <- read.csv("ess_Mosquito_BayesComm.csv")
ess_beta_BayesComm <- ess_beta_BayesComm[,-1]
colnames(ess_beta_BayesComm) <- NULL

ess_rho_BayesComm <- read.csv("essRho_Mosquito_BayesComm.csv")
ess_rho_BayesComm <- ess_rho_BayesComm[,2]
tmp <- matrix(NA, ncol=16, nrow = 16)
tmp[lower.tri(tmp)] <- ess_rho_BayesComm
tmp[upper.tri(tmp)] <- t(tmp)[upper.tri(tmp)]
ess_rho_BayesComm <- tmp

# boral

ess_beta_boral <- read.csv("ess_beta_Mosq_boral.csv")
ess_beta_boral <- ess_beta_boral[,-1]
colnames(ess_beta_boral) <- NULL

ess_rho_boral <- read.csv("ess_Rho_Mosq_boral.csv")
ess_rho_boral <- ess_rho_boral[,-1]

# Clark

ess_beta_Clark <- read.csv("ess_Mosquito_Clark.csv")
ess_beta_Clark <- ess_beta_Clark[,-1]
colnames(ess_beta_Clark) <- NULL

ess_rho_Clark <- read.csv("ess_Rho_Mosquito_Clark.csv")
ess_rho_Clark <- ess_rho_Clark[,-1]
colnames(ess_rho_Clark) <- NULL

# Ovaskainen 2016 Spatial

ess_beta_Ov <- read.csv("ess_beta_Mosquito_Ov.csv")
ess_beta_Ov <- ess_beta_Ov[,-1]
colnames(ess_beta_Ov) <- NULL

ess_rho_Ov <- read.csv("ess_rho_Mosquito_Ov.csv")
ess_rho_Ov <- ess_rho_Ov[,-1]
colnames(ess_rho_Ov) <- NULL

# Ovaskainen 2016 Non-spatial

ess_beta_OvNS <- read.csv("ess_beta_Mosquito_OvNS.csv")
ess_beta_OvNS <- ess_beta_OvNS[,-1]
colnames(ess_beta_OvNS) <- NULL

ess_rho_OvNS <- read.csv("ess_rho_Mosquito_OvNS.csv")
ess_rho_OvNS <- ess_rho_OvNS[,-1]
colnames(ess_rho_OvNS) <- NULL

# Pollock

ess_beta_Pollock <- read.csv("ess_Mosquito_Pollock.csv")
ess_beta_Pollock <- ess_beta_Pollock[,-1]
colnames(ess_beta_Pollock) <- NULL

ess_rho_Pollock <- read.csv("ess_Rho_Mosquito_Pollock.csv")
ess_rho_Pollock <- ess_rho_Pollock[,-1]
colnames(ess_rho_Pollock) <- NULL

###################################
#### Correction for time/cores ####
###################################

ess_beta_BayesComm <- apply(ess_beta_BayesComm, c(1,2), function(x) (x/(28.21/60/60))) 
ess_rho_BayesComm <- apply(ess_rho_BayesComm, c(1,2), function(x) (x/(28.21/60/60)))

ess_beta_boral <- apply(ess_beta_boral, c(1,2), function(x) (x/(116.86/60/60)))
ess_rho_boral <- apply(ess_rho_boral, c(1,2), function(x) (x/(116.86/60/60)))

ess_beta_Clark <- apply(ess_beta_Clark, c(1,2), function(x) (x/(474.48/60/60)))
ess_rho_Clark <- apply(ess_rho_Clark, c(1,2), function(x) (x/(474.48/60/60)))

ess_beta_Ov <- apply(ess_beta_Ov, c(1,2), function(x) (x/(830.12/60/60)))
ess_rho_Ov <- apply(ess_rho_Ov, c(1,2), function(x) (x/(830.12/60/60)))

ess_beta_OvNS <- apply(ess_beta_OvNS, c(1,2), function(x) (x/(135.89/60/60)))
ess_rho_OvNS <- apply(ess_rho_OvNS, c(1,2), function(x) (x/(135.89/60/60)))

ess_beta_Pollock <- apply(ess_beta_Pollock, c(1,2), function(x) (x/(13.42/60)))
ess_rho_Pollock <- apply(ess_rho_Pollock, c(1,2), function(x) (x/(13.42/60)))

##################
#### Boxplots ####
##################

ess.Beta <- list(ess_beta_BayesComm, ess_beta_boral, ess_beta_Clark, ess_beta_Ov, ess_beta_OvNS, ess_beta_Pollock)
ess.Rho <- list(ess_rho_BayesComm, ess_rho_boral, ess_rho_Clark, ess_rho_Ov, ess_rho_OvNS, ess_rho_Pollock)

boxplot(ess.Beta, outline = F)
boxplot(ess.Rho, outline = F)


ess.Beta.df <- data.frame(ESS = c(ess_beta_BayesComm,
                                  ess_beta_boral,
                                  ess_beta_Clark,
                                  ess_beta_Ov,
                                  ess_beta_OvNS,
                                  ess_beta_Pollock),
                          Model = c(rep("BayesComm", length(ess_beta_BayesComm)),
                                    rep("boral", length(ess_beta_boral)),
                                    rep("Clark", length(ess_beta_Clark)),
                                    rep("Ovaskainen 2016 Spatial", length(ess_beta_Ov)),
                                    rep("Ovaskainen 2016 Non-spatial", length(ess_beta_OvNS)),
                                    rep("Pollock", length(ess_beta_Pollock))))

jpeg(filename = "ESS - Beta.jpg", width = 7, height = 7, units = "in", res = 72)
ggplot(ess.Beta.df, aes(Model, ESS)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_boxplot() +
  ylim(0,400000) +
  ggtitle("Effective Sample Size - Beta") +
  ylab(expression(atop("Effective Number of MCMC samples", paste("(per hour, per unit of computational time)")))) +
  xlab(NULL)
dev.off()


ess.Rho.df <- data.frame(ESS = c(ess_rho_BayesComm[upper.tri(ess_rho_BayesComm, diag = FALSE)],
                                  ess_rho_boral[upper.tri(ess_rho_boral, diag = FALSE)],
                                  ess_rho_Clark[upper.tri(ess_rho_Clark, diag = FALSE)],
                                  ess_rho_Ov[upper.tri(ess_rho_Ov, diag = FALSE)],
                                  ess_rho_OvNS[upper.tri(ess_rho_OvNS, diag = FALSE)],
                                  ess_rho_Pollock[upper.tri(ess_rho_Pollock, diag = FALSE)]),
                          Model = c(rep("BayesComm", length(ess_rho_BayesComm[upper.tri(ess_rho_BayesComm, diag = FALSE)])),
                                    rep("boral", length(ess_rho_boral[upper.tri(ess_rho_boral, diag = FALSE)])),
                                    rep("Clark", length(ess_rho_Clark[upper.tri(ess_rho_Clark, diag = FALSE)])),
                                    rep("Ovaskainen 2016 Spatial", length(ess_rho_Ov[upper.tri(ess_rho_Ov, diag = FALSE)])),
                                    rep("Ovaskainen 2016 Non-spatial", length(ess_rho_OvNS[upper.tri(ess_rho_OvNS, diag = FALSE)])),
                                    rep("Pollock", length(ess_rho_Pollock[upper.tri(ess_rho_Pollock, diag = FALSE)]))))

jpeg(filename = "ESS - Rho.jpg", width = 7, height = 7, units = "in", res = 72)
ggplot(ess.Rho.df, aes(Model, ESS)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_boxplot() +
  ylim(0,800000) +
  ggtitle("Effective Sample Size - Rho") +
  ylab(expression(atop("Effective Number of MCMC samples", paste("(per hour, per unit of computational time)")))) +
  xlab(NULL)
dev.off()  
