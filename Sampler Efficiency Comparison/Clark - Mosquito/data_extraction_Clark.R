library(matrixStats)
library(abind)

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

mean.beta <- colMeans(beta.chain) # mean beta
sd.beta <- colSds(beta.chain) # sd beta
lower.beta <- colQuantiles(beta.chain, probs = 0.025) # lower CI beta
upper.beta <- colQuantiles(beta.chain, probs = 0.975) # upper CI beta

############################
## Create blank dataframe ##
############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), model = numeric(0), species = numeric(0))

##########################
## Extract to dataframe ##
##########################

for(i in 1:ncol(PA)){
  dfr <- cbind(coef.name, mean.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))], lower.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               upper.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))], sd.beta[(i*length(coef.name)-(length(coef.name)-1)):(i*length(coef.name))],
               rep("Clark", (length(coef.name))), rep(c.name[i], length(coef.name)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", "model", "species")
  dfr <- as.data.frame(dfr)
  df <- rbind(df, dfr)
}


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

#############
#### ESS ####
#############

library(coda)

species <- colnames(PA)
preds <- colnames(Env)

# beta

tmp <- effectiveSize(JSDM$chains$bgibbs)

ess <- matrix(tmp, byrow = FALSE, ncol = length(preds)+1)
colnames(ess) <- c("intercept", preds)

# rho

tmp <- effectiveSize(JSDM$chains$sgibbs)
ess.Rho <- matrix(0, ncol = length(species), nrow = length(species))
ess.Rho[lower.tri(ess.Rho, diag = TRUE)] <- tmp
ess.Rho[upper.tri(ess.Rho, diag = FALSE)] <- t(ess.Rho)[upper.tri(ess.Rho, diag = FALSE)]
colnames(ess.Rho) <- rownames(ess.Rho) <- species


####################
#### Write CSVs ####
####################

write.csv(df, "Beta_Mosquito_Clark.csv")
write.csv(mean.corr, "Rho_mean_Mosquito_Clark.csv")
write.csv(lower.corr, "Rho_lower_Mosquito_Clark.csv")
write.csv(upper.corr, "Rho_upper_Mosquito_Clark.csv")
write.csv(sd.corr, "Rho_sd_Mosquito_Clark.csv")
write.csv(ess, "ess_Mosquito_Clark.csv")
write.csv(ess.Rho, "ess_Rho_Mosquito_Clark.csv")
