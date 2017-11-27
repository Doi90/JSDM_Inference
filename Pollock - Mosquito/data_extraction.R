library(tibble)
library(ineq)

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
#### Coefficient of Variation #####
###################################

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

cv.df <- SUMMARY2(Beta, coefVar)
cv.df <- t(cv.df)
cv.df <- cbind(c("intercept", colnames(X)), cv.df)
rownames(cv.df) <- NULL
colnames(cv.df) <- c("coefficients", colnames(Occur))

###################################
### Quantile Coef of Dispersion ###
########### 0.25 / 0.75 ###########
###################################

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

qcd.df <- SUMMARY2(Beta, qcd)
qcd.df <- t(qcd.df)
qcd.df <- cbind(c("intercept", colnames(X)), qcd.df)
rownames(qcd.df) <- NULL
colnames(qcd.df) <- c("coefficients", colnames(Occur))

###################################
### Quantile Coef of Dispersion ###
########### 0.25 / 0.75 ###########
###################################

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

qcd2.df <- SUMMARY2(Beta, qcd2)
qcd2.df <- t(qcd2.df)
qcd2.df <- cbind(c("intercept", colnames(X)), qcd2.df)
rownames(qcd2.df) <- NULL
colnames(qcd2.df) <- c("coefficients", colnames(Occur))

###################################
####### Gini's Coefficient ########
###################################

gini.df <- SUMMARY2(Beta, ineq::ineq, type = "Gini")
gini.df <- t(gini.df)
gini.df <- cbind(c("intercept", colnames(X)), gini.df)
rownames(gini.df) <- NULL
colnames(gini.df) <- c("coefficients", colnames(Occur))

###################################
##### Create blank data frame #####
###################################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                 qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))
colnames(df) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                  "coefVar", "qcd", "qcd2", "gini", "model", "species")

###################################
### Extract data into dataframe ###
###################################

for(i in colnames(Occur)){
  dfr <- cbind(mean.df[, 1], mean.df[,i], lower.df[,i], upper.df[,i], sd.df[,i],
               cv.df[, i], qcd.df[, i], qcd2.df[, i], gini.df[, i],
               rep("Pollock", ncol(X)+1), rep(i, ncol(X)+1))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
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

###################################
########### Write csvs ############
###################################

write.csv(df, "Beta_Mosquito_Pollock.csv")
write.csv(rho.mean, "Rho_mean_Mosquito_Pollock.csv")
write.csv(rho.lower, "Rho_lower_Mosquito_Pollock.csv")
write.csv(rho.upper, "Rho_upper_Mosquito_Pollock.csv")
write.csv(rho.sd, "Rho_sd_Mosquito_Pollock.csv")
write.csv(ess, "ess_Mosquito.csv")
