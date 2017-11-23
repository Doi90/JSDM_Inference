#########################################                          
#########################################
### DATA EXTRACTION FROM boral MODELS ###
#########################################
#########################################

library(boral)
library(abind)
library(matrixStats)
library(ineq)

################################################
### Define species and coefficient names/ids ###
################################################

species.id <- 1:ncol(PA)  # vector of species ids [for extraction]
species.names <- colnames(PA) # vector of species names

Env.id <- 1:ncol(Env)  # vector of covariate ids (no intercept) [for extraction]
Env.names <- c("intercept", colnames(Env)) # vector of covariate names incl. intercept

model <- "boral"

############################################
### Extract MCMC samples from jags model ###
############################################

mcmc.samp <- as.mcmc(JSDM$jags.model) # extracts mcmc from boral object

colnames(mcmc.samp[[1]]) # column names from mcmc object

#######################################
### Standardise posteriors post-hoc ###
#######################################

dataset_sd <- read.csv("Mosquitos_sd.csv")   # load in original data sd
dataset_sd <- dataset_sd[,2]
# dataset_sd <- c(1, dataset_sd) # add intercept ##NOT REQUIRED FOR BORAL

mcmc.samp_standardised <- mcmc.samp

# for(i in seq(length(full_standardised$trace$B))){   # species  ## EASIER TO DO DURING EXTRACTON FOR BORAL
#   for(j in seq(length(dataset_sd))){                #covariate
#     full_standardised$trace$B[[i]][,j] <- (full_standardised$trace$B[[i]][,j])/dataset_sd[j]
#   }
# }

##############################################
### Extract regression coefficient samples ###
##############################################

# Not standardised

beta.samp <- matrix(nrow = 1000)

col.names <- vector()

for(i in species.id){ # species
  
  column.name <- sprintf("lv.coefs[%s,1]", i)  # 3 lines code to get species intercept samples
  col.names <- c(col.names, column.name) 
  command <- sprintf('mcmc.samp[[1]][1:1000,"%s"]', column.name)
  tmp <- eval(parse(text = command))
  
  for(j in Env.id){ # variables
    
    column.name2 <- sprintf("X.coefs[%s,%s]", i, j) # 3 lines code to get variables samples (by species)
    col.names <- c(col.names, column.name2)
    command2 <- sprintf('mcmc.samp[[1]][1:1000, "%s"]', column.name2)
    tmp2 <- eval(parse(text = command2))
    
    tmp <- cbind(tmp, tmp2) # create entire matrix for one species
    
  }
 
  beta.samp <- cbind(beta.samp, tmp) # bind all species matrcies together 
  
}

beta.samp <- beta.samp[,-(1)] # remove NA column from beginning
colnames(beta.samp) <- col.names # set column names

# Standardised

beta.samp_standardised <- matrix(nrow = 1000)

col.names_standardised <- vector()

for(i in species.id){ # species
  
  column.name <- sprintf("lv.coefs[%s,1]", i)  # 3 lines code to get species intercept samples
  col.names_standardised <- c(col.names_standardised, column.name) 
  command <- sprintf('mcmc.samp[[1]][1:1000,"%s"]', column.name)
  tmp <- eval(parse(text = command))
  
  for(j in Env.id){ # variables
    
    column.name2 <- sprintf("X.coefs[%s,%s]", i, j) # 3 lines code to get variables samples (by species)
    col.names_standardised <- c(col.names_standardised, column.name2)
    command2 <- sprintf('(mcmc.samp[[1]][1:1000, "%s"])/dataset_sd[%s]', column.name2, j) # added sd correction
    tmp2 <- eval(parse(text = command2))
    
    tmp <- cbind(tmp, tmp2) # create entire matrix for one species
    
  }
  
  beta.samp_standardised <- cbind(beta.samp_standardised, tmp) # bind all species matrcies together 
  
}

beta.samp_standardised <- beta.samp_standardised[,-(1)] # remove NA column from beginning
colnames(beta.samp_standardised) <- col.names_standardised # set column names

#####################################################
### Extract Mean/SD/Quantiles and into Data Frame ###
#####################################################

# Not standardised

beta.mean <- colMeans(beta.samp)
beta.mean <- matrix(beta.mean, ncol = length(Env.names), byrow = T)
colnames(beta.mean) <- Env.names
rownames(beta.mean) <- species.names

beta.sd <- colSds(beta.samp)
beta.sd <- matrix(beta.sd, ncol = length(Env.names), byrow = T)
colnames(beta.sd) <- Env.names
rownames(beta.sd) <- species.names

beta.lower <- colQuantiles(beta.samp, probs = 0.025)
beta.lower <- matrix(beta.lower, ncol = length(Env.names), byrow = T)
colnames(beta.lower) <- Env.names
rownames(beta.lower) <- species.names

beta.upper <- colQuantiles(beta.samp, probs = 0.975)
beta.upper <- matrix(beta.upper, ncol = length(Env.names), byrow = T)
colnames(beta.upper) <- Env.names
rownames(beta.upper) <- species.names

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

beta.cv <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- coefVar(beta.samp[,i])
  beta.cv <- c(beta.cv, tmp)
}
beta.cv <- matrix(beta.cv, ncol = length(Env.names), byrow = TRUE)
colnames(beta.cv) <- Env.names
rownames(beta.cv) <- species.names

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- qcd(beta.samp[,i])
  beta.qcd <- c(beta.qcd, tmp)
}
beta.qcd <- matrix(beta.qcd, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd) <- Env.names
rownames(beta.qcd) <- species.names

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd2 <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- qcd2(beta.samp[,i])
  beta.qcd2 <- c(beta.qcd2, tmp)
}
beta.qcd2 <- matrix(beta.qcd2, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd2) <- Env.names
rownames(beta.qcd2) <- species.names

beta.gini <- numeric(0)
for(i in seq(ncol(beta.samp))){
  tmp <- ineq(beta.samp[,i], type = "Gini")
  beta.gini <- c(beta.gini, tmp)
}
beta.gini <- matrix(beta.gini, ncol = length(Env.names), byrow = TRUE)
colnames(beta.gini) <- Env.names
rownames(beta.gini) <- species.names

# Standardised

beta.mean_standardised <- colMeans(beta.samp_standardised)
beta.mean_standardised <- matrix(beta.mean_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.mean_standardised) <- Env.names
rownames(beta.mean_standardised) <- species.names

beta.sd_standardised <- colSds(beta.samp_standardised)
beta.sd_standardised <- matrix(beta.sd_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.sd_standardised) <- Env.names
rownames(beta.sd_standardised) <- species.names

beta.lower_standardised <- colQuantiles(beta.samp_standardised, probs = 0.025)
beta.lower_standardised <- matrix(beta.lower_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.lower_standardised) <- Env.names
rownames(beta.lower_standardised) <- species.names

beta.upper_standardised <- colQuantiles(beta.samp_standardised, probs = 0.975)
beta.upper_standardised <- matrix(beta.upper_standardised, ncol = length(Env.names), byrow = T)
colnames(beta.upper_standardised) <- Env.names
rownames(beta.upper_standardised) <- species.names

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

beta.cv_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- coefVar(beta.samp_standardised[,i])
  beta.cv_standardised <- c(beta.cv_standardised, tmp)
}
beta.cv_standardised <- matrix(beta.cv_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.cv_standardised) <- Env.names
rownames(beta.cv_standardised) <- species.names

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- qcd(beta.samp_standardised[,i])
  beta.qcd_standardised <- c(beta.qcd_standardised, tmp)
}
beta.qcd_standardised <- matrix(beta.qcd_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd_standardised) <- Env.names
rownames(beta.qcd_standardised) <- species.names

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

beta.qcd2_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- qcd2(beta.samp_standardised[,i])
  beta.qcd2_standardised <- c(beta.qcd2_standardised, tmp)
}
beta.qcd2_standardised <- matrix(beta.qcd2_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.qcd2_standardised) <- Env.names
rownames(beta.qcd2_standardised) <- species.names

beta.gini_standardised <- numeric(0)
for(i in seq(ncol(beta.samp_standardised))){
  tmp <- ineq(beta.samp_standardised[,i], type = "Gini")
  beta.gini_standardised <- c(beta.gini_standardised, tmp)
}
beta.gini_standardised <- matrix(beta.gini_standardised, ncol = length(Env.names), byrow = TRUE)
colnames(beta.gini_standardised) <- Env.names
rownames(beta.gini_standardised) <- species.names

##############################
### Create blank dataframe ### # Not standardised
##############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                 qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

############################
### Extract to dataframe ### # Not standardised
############################

for(i in 1:ncol(PA)){
  dfr <- cbind(Env.names, beta.mean[i,], beta.lower[i,], beta.upper[i,], beta.sd[i,],
               beta.cv[i,], beta.qcd[i,], beta.qcd2[i,], beta.gini[i,],
               rep("boral", (length(Env.names))), rep(species.names[i], length(Env.names)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df <- rbind(df, dfr)
}

rownames(df) <- NULL

##############################
### Create blank dataframe ### # Standardised
##############################

df_standardised <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                              upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                              qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

############################
### Extract to dataframe ### # Standardised
############################

for(i in 1:ncol(PA)){
  dfr <- cbind(Env.names, beta.mean_standardised[i,], beta.lower_standardised[i,],
               beta.upper_standardised[i,], beta.sd_standardised[i,],
               beta.cv_standardised[i,], beta.qcd_standardised[i,], beta.qcd2_standardised[i,],
               beta.gini_standardised[i,], rep("boral", (length(Env.names))),
               rep(species.names[i], length(Env.names)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df_standardised <- rbind(df_standardised, dfr)
}

rownames(df_standardised) <- NULL

df_merge <- df
df_merge[,5:9] <- df_standardised[,5:9]

#############################
### Rho Mean/Sd/Quantiles ###
#############################

source("get_residual_corr_new_function.R") # implement modified version of get.residual.cor() to provide sd and quantile values

rho.mean <- get.residual.cor.new(JSDM, est = "mean")$cor
rho.sd <- get.residual.cor.new(JSDM, est = "sd")$cor
rho.lower <- get.residual.cor.new(JSDM, est = "q_lower")$cor
rho.upper <- get.residual.cor.new(JSDM, est = "q_upper")$cor

####################
#### Write CSVs ####
####################

write.csv(df_merge, "Beta_Mosquito_boral.csv")
write.csv(rho.mean, "Rho_mean_Mosquito_boral.csv")
write.csv(rho.lower, "Rho_lower_Mosquito_boral.csv")
write.csv(rho.upper, "Rho_upper_Mosquito_boral.csv")
write.csv(rho.sd, "Rho_sd_Mosquito_boral.csv")