library(tibble)
library(BayesComm)
library(ineq)

##############################
## Set up species and model ##
##############################

species <- colnames(fauna)
model <- "BayesComm"

#######################################
### Standardise posteriors post-hoc ###
#######################################

dataset_sd <- read.csv("Eucalypts_sd.csv")   # load in original data sd
dataset_sd <- dataset_sd[,2]
dataset_sd <- c(1, dataset_sd) # add intercept

full_standardised <- full

for(i in seq(length(full_standardised$trace$B))){   # species
  for(j in seq(length(dataset_sd))){                #covariate
    full_standardised$trace$B[[i]][,j] <- (full_standardised$trace$B[[i]][,j])/dataset_sd[j]
  }
}

##############################
### Create blank dataframe ### not standardised
##############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                 qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))
colnames(df) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                  "coefVar", "qcd", "qcd2", "gini", "model", "species")

##############################
## For loop data extraction ## not standardised
##############################

for(i in 1:length(species)){
  command <- sprintf('summary(%s, "%s$%s")', 'full', 'B', species[i])
  tmp <- eval(parse(text = command))
  sum.full <- tmp
  stat.full <- sum.full$statistics
  quan.full <- sum.full$quantiles
  gini.command <- sprintf('full$trace$B$%s', species[i])
  gini.eval <- eval(parse(text = gini.command))
  gini <- numeric(0)
  for(j in seq(ncol(gini.eval))){
    tmp <- ineq(gini.eval[,j], type = "Gini")
    gini <- c(gini, tmp)
  }
  dfr <- cbind(c("intercept", colnames(preds)), stat.full[,1], quan.full[,1], quan.full[,5],
               stat.full[,2], stat.full[,2]/stat.full[,1],
               (quan.full[,4] - quan.full[, 2]) / (quan.full[,4] + quan.full[, 2]),
               (quan.full[,5] - quan.full[, 1]) / (quan.full[,5] + quan.full[, 1]),
               gini, rep(model, ncol(preds)+1), rep(species[i], ncol(preds)+1))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  dfr$coefficient <- c("intercept", colnames(preds))
  df <- rbind(df, dfr)
}

rownames(df) <- NULL
head(df)

##############################
### Create blank dataframe ### standardised
##############################

df2 <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                  upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                  qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))
colnames(df2) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                   "coefVar", "qcd", "qcd2", "gini", "model", "species")

##############################
## For loop data extraction ##
##############################

for(i in 1:length(species)){
  command <- sprintf('summary(%s, "%s$%s")', 'full_standardised', 'B', species[i])
  tmp <- eval(parse(text = command))
  sum.full <- tmp
  stat.full <- sum.full$statistics
  quan.full <- sum.full$quantiles
  gini.command <- sprintf('full_standardised$trace$B$%s', species[i])
  gini.eval <- eval(parse(text = gini.command))
  gini <- numeric(0)
  for(j in seq(ncol(gini.eval))){
    tmp <- ineq(gini.eval[,j], type = "Gini")
    gini <- c(gini, tmp)
  }
  dfr <- cbind(c("intercept", colnames(preds)), stat.full[,1], quan.full[,1], quan.full[,5],
               stat.full[,2], stat.full[,2]/stat.full[,1],
               (quan.full[,4] - quan.full[, 2]) / (quan.full[,4] + quan.full[, 2]),
               (quan.full[,5] - quan.full[, 1]) / (quan.full[,5] + quan.full[, 1]),
               gini, rep(model, ncol(preds)+1), rep(species[i], ncol(preds)+1))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  dfr$coefficient <- c("intercept", colnames(preds))
  df2 <- rbind(df2, dfr)
}

rownames(df2) <- NULL
head(df2)

#############################################
### Combine relevant sections of df & df2 ###
#############################################

df[,5:9] <- df2[,5:9]

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

##############################
######### Write csv ##########
##############################

write.csv(df, "Beta_df_Eucalypt_BayesComm.csv")
write.csv(Sigma.mean, "Rho_mean_Eucalypt_BayesComm.csv")
write.csv(Sigma.lower, "Rho_lower_Eucalypt_BayesComm.csv")
write.csv(Sigma.upper, "Rho_upper_Eucalypt_BayesComm.csv")
write.csv(Sigma.sd, "Rho_sd_Eucalypt_BayesComm.csv")
write.csv(ess, "ess_Eucalypt_BayesComm.csv")
