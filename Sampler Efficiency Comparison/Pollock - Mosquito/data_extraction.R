library(tibble)

###################################
#### Set up species/coef names ####
###################################

c.name <- colnames(Occur)
coef.name <- c("intercept", colnames(X))

###################################
###### Extract posterior mean #####
###################################

mean.df <- t(as.data.frame(SUMMARY(Beta, mean)))
colnames(mean.df) <- c.name
mean.df <- cbind(coef.name, mean.df)

###################################
####### Extract posterior sd ######
###################################

sd.df <- t(as.data.frame(SUMMARY(Beta, sd)))
colnames(sd.df) <- c.name
sd.df <- cbind(coef.name, sd.df)

###################################
######### Extract lower CI ########
###################################

lower.df <- SUMMARY2(Beta, quantile, probs = 0.025)
lower.df <- t(lower.df)
lower.df <- cbind(c("intercept", colnames(X)), lower.df)
rownames(lower.df) <- NULL
colnames(lower.df) <- c("coefficients", colnames(Occur))

###################################
######### Extract upper CI ########
###################################

upper.df <- SUMMARY2(Beta, quantile, probs = 0.975)
upper.df <- t(upper.df)
upper.df <- cbind(c("intercept", colnames(X)), upper.df)
rownames(upper.df) <- NULL
colnames(upper.df) <- c("coefficients", colnames(Occur))

###################################
##### Create blank data frame #####
###################################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), model = numeric(0), species = numeric(0))
colnames(df) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", "model", "species")

###################################
### Extract data into dataframe ###
###################################

for(i in colnames(Occur)){
  dfr <- cbind(mean.df[, 1], mean.df[,i], lower.df[,i], upper.df[,i], sd.df[,i],
               rep("Pollock", ncol(X)+1), rep(i, ncol(X)+1))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", "model", "species")
  dfr <- as.data.frame(dfr)
  dfr$coefficient <- mean.df[, 1]
  df <- rbind(df, dfr)
}

###################################
############## Rho ################
###################################

rho.mean <- SUMMARY(Rho, mean)
colnames(rho.mean) <- colnames(Occur)
rownames(rho.mean) <- colnames(Occur)

rho.lower <- SUMMARY2(Rho, quantile, probs = 0.025)
colnames(rho.lower) <- colnames(Occur)
rownames(rho.lower) <- colnames(Occur)

rho.upper <- SUMMARY2(Rho, quantile, probs = 0.975)
colnames(rho.upper) <- colnames(Occur)
rownames(rho.upper) <- colnames(Occur)

rho.sd <- SUMMARY(Rho, sd)
colnames(rho.sd) <- colnames(Occur)
rownames(rho.sd) <- colnames(Occur)

###################################
###### Effective Sample Size ######
###################################

ess <- Diagnose(Beta, "effn")

mean(ess)/(13.41568/60)
sd(ess)/(13.41568/60)
range(ess)/(13.41568/60)

ess.R <- Diagnose(Rho, "effn")

mean(ess.R[upper.tri(ess.R)])/(13.41568/60)
sd(ess.R[upper.tri(ess.R)])/(13.41568/60)
range(ess.R[upper.tri(ess.R)])/(13.41568/60)

-###################################
########### Write csvs ############
###################################

write.csv(df, "Beta_Mosquito_Pollock.csv")
write.csv(rho.mean, "Rho_mean_Mosquito_Pollock.csv")
write.csv(rho.lower, "Rho_lower_Mosquito_Pollock.csv")
write.csv(rho.upper, "Rho_upper_Mosquito_Pollock.csv")
write.csv(rho.sd, "Rho_sd_Mosquito_Pollock.csv")
write.csv(ess, "ess_Mosquito_Pollock.csv")
write.csv(ess.R, "ess_Rho_Mosquito_Pollock.csv")
