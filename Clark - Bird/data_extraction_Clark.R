library(matrixStats)
library(abind)
library(ineq)

###############################
## Set up species/coef names ##
###############################

c.name <- colnames(PA)  # species
coef.name <- c("intercept", colnames(Env))

##########
##########
## BETA ##
##########
##########

beta.test <- JSDM$parameterTables$betaMu

beta.chain <- JSDM$chains$bgibbs  # store beta chain data
beta.chain <- beta.chain[-(1:JSDM$modelList$burnin),] # remove burnin
beta.chain <- beta.chain[seq(1,nrow(beta.chain), 50),] # implement thinning

#######################################
### Standardise posteriors post-hoc ###
#######################################

dataset_sd <- read.csv("Birds_sd.csv")   # load in original data sd
dataset_sd <- dataset_sd[,2]
dataset_sd <- c(1, dataset_sd) # add intercept

beta.chain_standardised <- t(t(beta.chain)/dataset_sd)

# for(i in seq(length(full_standardised$trace$B))){   # species  ## EASIER TO DO DURING EXTRACTON FOR BORAL
#   for(j in seq(length(dataset_sd))){                #covariate
#     full_standardised$trace$B[[i]][,j] <- (full_standardised$trace$B[[i]][,j])/dataset_sd[j]
#   }
# }

# Non-standardised extraction setup

mean.beta <- colMeans(beta.chain) # mean beta
sd.beta <- colSds(beta.chain) # sd beta
lower.beta <- colQuantiles(beta.chain, probs = 0.025) # lower CI beta
upper.beta <- colQuantiles(beta.chain, probs = 0.975) # upper CI beta

coefVar <- function(vector){
  sd.cv <- sd(vector)
  mean.cv <- mean(vector)
  cv <- sd.cv/mean.cv
  return(cv)
}
cv.beta <- numeric(0)
for(i in seq(ncol(beta.chain))){
  tmp <- coefVar(beta.chain[,i])
  cv.beta <- c(cv.beta, tmp)
}

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}
qcd.beta <- numeric(0)
for(i in seq(ncol(beta.chain))){
  tmp <- qcd(beta.chain[,i])
  qcd.beta <- c(qcd.beta, tmp)
}

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd2 <- (q3-q1)/(q3+q1)
  return(qcd2)
}
qcd2.beta <- numeric(0)
for(i in seq(ncol(beta.chain))){
  tmp <- qcd2(beta.chain[,i])
  qcd2.beta <- c(qcd2.beta, tmp)
}

gini.beta <- numeric(0)
for(i in seq(ncol(beta.chain))){
  tmp <- ineq(beta.chain[,i], type = "Gini")
  gini.beta <- c(gini.beta, tmp)
}

# Standardised extraction setup

mean.beta_standardised <- colMeans(beta.chain_standardised) # mean beta
sd.beta_standardised <- colSds(beta.chain_standardised) # sd beta
lower.beta_standardised <- colQuantiles(beta.chain_standardised, probs = 0.025) # lower CI beta
upper.beta_standardised <- colQuantiles(beta.chain_standardised, probs = 0.975) # upper CI beta

cv.beta_standardised <- numeric(0)
for(i in seq(ncol(beta.chain_standardised))){
  tmp <- coefVar(beta.chain_standardised[,i])
  cv.beta_standardised <- c(cv.beta_standardised, tmp)
}

qcd.beta_standardised <- numeric(0)
for(i in seq(ncol(beta.chain_standardised))){
  tmp <- qcd(beta.chain_standardised[,i])
  qcd.beta_standardised <- c(qcd.beta_standardised, tmp)
}

qcd2.beta_standardised <- numeric(0)
for(i in seq(ncol(beta.chain_standardised))){
  tmp <- qcd2(beta.chain_standardised[,i])
  qcd2.beta_standardised <- c(qcd2.beta_standardised, tmp)
}

gini.beta_standardised <- numeric(0)
for(i in seq(ncol(beta.chain_standardised))){
  tmp <- ineq(beta.chain_standardised[,i], type = "Gini")
  gini.beta_standardised <- c(gini.beta_standardised, tmp)
}

############################
## Create blank dataframe ## # Non-standardised
############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                 qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

##########################
## Extract to dataframe ## # Non-standardised
##########################

for(i in 1:ncol(PA)){
  dfr <- cbind(coef.name,
               mean.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               lower.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               upper.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               sd.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               cv.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               qcd.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               qcd2.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               gini.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               rep("Clark", (length(coef.name))), rep(c.name[i], length(coef.name)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df <- rbind(df, dfr)
}

############################
## Create blank dataframe ## # Standardised
############################

df_standardised <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                              upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                              qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))

##########################
## Extract to dataframe ## # Dtandardised
##########################

for(i in 1:ncol(PA)){
  dfr <- cbind(coef.name,
               mean.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               lower.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               upper.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               sd.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               cv.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               qcd.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               qcd2.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               gini.beta_standardised[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               rep("Clark", (length(coef.name))), rep(c.name[i], length(coef.name)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  df_standardised <- rbind(df_standardised, dfr)
}

# Combine required parts of standardised/non-standardised

df_merge <- df
df_merge[,5:9] <- df_standardised[,5:9]
rownames(df_merge) <- NULL
#########
#########
## RHO ##
#########
#########

rho.test <- JSDM$parameterTables$corMu

corr.chain <- JSDM$chains$sgibbs  # store correlation chain data
corr.chain <- corr.chain[-(1:JSDM$modelList$burnin),]  # remove burnin
corr.chain <- corr.chain[seq(1, nrow(corr.chain), 50),] # implement thinning

corr.array <- matrix(nrow = ncol(PA), ncol = ncol(PA)) # create blank matrix to start array

for (i in 1:nrow(corr.chain)){
  x <- corr.chain[i,]   # extract first covariance table as vector
  
  a <- diag(nrow = ncol(PA)) # build diag matrix
  a[lower.tri(a, diag = T)] <- x # fill lower.tri with covariance 
  a <- t(a) # transpose to fill upper.tri
  
  corr.array <- abind(corr.array, cov2cor(a), along = 3) # bind matrix to array, convert to correlation matrix
} 

corr.array <- corr.array[,,-1]  # remove NA matrix from beginning of array

## now can apply functions over each cell in array

upper.corr.array <- t(apply(corr.array, 3, '[', upper.tri(corr.array[,,1], diag = T))) # returns matrix of col = each element from upper (diag=T), rows = #samples, diag = T to track future shape changes

dim(upper.corr.array) # check dimension

mean.upper.corr.array <- colMeans(upper.corr.array) # take mean value for each correlation element

mean.corr <- diag(nrow = ncol(PA)) # blank data.frame
mean.corr[upper.tri(mean.corr, diag=T)] <- mean.upper.corr.array # fill with mean value
mean.corr

lower.upper.corr.array <- colQuantiles(upper.corr.array, probs = 0.025) # take lower quantile value for each correlation element

lower.corr <- diag(nrow = ncol(PA)) # blank data.frame
lower.corr[upper.tri(lower.corr, diag=T)] <- lower.upper.corr.array
lower.corr

upper.upper.corr.array <- colQuantiles(upper.corr.array, probs = 0.975) # take upper quantile value for each correlation element

upper.corr <- diag(nrow = ncol(PA)) # blank data.frame
upper.corr[upper.tri(upper.corr, diag=T)] <- upper.upper.corr.array
upper.corr

sd.upper.corr.array <- colSds(upper.corr.array) # take sd value for each correlation element

sd.corr <- diag(nrow = ncol(PA)) # blank data.frame
sd.corr[upper.tri(sd.corr, diag=T)] <- sd.upper.corr.array
sd.corr

####################
#### Write CSVs ####
####################

write.csv(df_merge, "Beta_Bird_Clark.csv")
write.csv(mean.corr, "Rho_mean_Bird_Clark.csv")
write.csv(lower.corr, "Rho_lower_Bird_Clark.csv")
write.csv(upper.corr, "Rho_upper_Bird_Clark.csv")
write.csv(sd.corr, "Rho_sd_Bird_Clark.csv")
