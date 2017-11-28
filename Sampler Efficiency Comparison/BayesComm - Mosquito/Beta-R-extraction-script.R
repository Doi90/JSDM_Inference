library(tibble)
library(BayesComm)

##############################
## Set up species and model ##
##############################

species <- colnames(fauna)
model <- "BayesComm"

##############################
### Create blank dataframe ###
##############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), model = numeric(0), species = numeric(0))
colnames(df) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", "model", "species")

##############################
## For loop data extraction ##
##############################

for(i in 1:length(species)){
  command <- sprintf('summary(%s, "%s$%s")', 'full', 'B', species[i])
  tmp <- eval(parse(text = command))
  sum.full <- tmp
  stat.full <- sum.full$statistics
  quan.full <- sum.full$quantiles
  dfr <- cbind(c("intercept", colnames(preds)), stat.full[,1], quan.full[,1], quan.full[,5],
               stat.full[,2], rep(model, ncol(preds)+1), rep(species[i], ncol(preds)+1))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", "model", "species")
  dfr <- as.data.frame(dfr)
  dfr$coefficient <- c("intercept", colnames(preds))
  df <- rbind(df, dfr)
}

rownames(df) <- NULL
head(df)

##############################
#### Extract correlation #####
##############################

correl.summ <- summary(full, 'R')
n.species <- ncol(fauna)

Sigma.mean <- matrix(0, nrow = n.species, ncol = n.species)
Sigma.mean[upper.tri(Sigma.mean)] <- correl.summ$statistics[, 'Mean']
Sigma.mean <- Sigma.mean + t(Sigma.mean)
diag(Sigma.mean) <- 1
rownames(Sigma.mean) <- colnames(fauna)
colnames(Sigma.mean) <- colnames(fauna)

Sigma.lower <- matrix(0, nrow = n.species, ncol = n.species)
Sigma.lower[upper.tri(Sigma.lower)] <- correl.summ$quantiles[, "2.5%"]
Sigma.lower <- Sigma.lower + t(Sigma.lower)
diag(Sigma.lower) <- 1
rownames(Sigma.lower) <- colnames(fauna)
colnames(Sigma.lower) <- colnames(fauna)

Sigma.upper <- matrix(0, nrow = n.species, ncol = n.species)
Sigma.upper[upper.tri(Sigma.upper)] <- correl.summ$quantiles[, "97.5%"]
Sigma.upper <- Sigma.upper + t(Sigma.upper)
diag(Sigma.upper) <- 1
rownames(Sigma.upper) <- colnames(fauna)
colnames(Sigma.upper) <- colnames(fauna)

Sigma.sd <- matrix(0, nrow = n.species, ncol = n.species)
Sigma.sd[upper.tri(Sigma.sd)] <- correl.summ$statistics[, 'SD']
Sigma.sd <- Sigma.sd + t(Sigma.sd)
diag(Sigma.sd) <- 1
rownames(Sigma.sd) <- colnames(fauna)
colnames(Sigma.sd) <- colnames(fauna)

##############################
### Effective Sample Size ####
##############################

ess <- data.frame(a = numeric(0), b = numeric(0), c = numeric(0), d = numeric(0), e = numeric(0),
                  f = numeric(0), g = numeric(0), h = numeric(0), i = numeric(0), j = numeric(0),
                  k = numeric(0), l = numeric(0), m = numeric(0), n = numeric(0))
colnames(ess) <- c("intercept", colnames(preds))

for(i in 1:length(species)){
  command <- sprintf('effectiveSize(full[1]$trace$B$%s)', species[i])
  tmp <- eval(parse(text = command))
  ess <- rbind(ess, tmp)
  colnames(ess) <- c("intercept", colnames(preds))
}

ess.Rho <- effectiveSize(full[1]$trace$R)

##############################
######### Write csv ##########
##############################

write.csv(df, "Beta_df_Mosquito_BayesComm.csv")
write.csv(Sigma.mean, "Rho_mean_Mosquito_BayesComm.csv")
write.csv(Sigma.lower, "Rho_lower_Mosquito_BayesComm.csv")
write.csv(Sigma.upper, "Rho_upper_Mosquito_BayesComm.csv")
write.csv(Sigma.sd, "Rho_sd_Mosquito_BayesComm.csv")
write.csv(ess, "ess_Mosquito_BayesComm.csv")
write.csv(ess.Rho, "essRho_Mosquito_BayesComm.csv")
