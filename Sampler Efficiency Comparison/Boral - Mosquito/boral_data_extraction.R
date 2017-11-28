#########################################                          
#########################################
### DATA EXTRACTION FROM boral MODELS ###
#########################################
#########################################

library(boral)
library(abind)
library(matrixStats)

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

##############################################
### Extract regression coefficient samples ###
##############################################

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

#####################################################
### Extract Mean/SD/Quantiles and into Data Frame ###
#####################################################

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

##############################
### Create blank dataframe ###
##############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), model = numeric(0), species = numeric(0))

############################
### Extract to dataframe ###
############################

for(i in 1:ncol(PA)){
  dfr <- cbind(Env.names, beta.mean[i,], beta.lower[i,], beta.upper[i,], beta.sd[i,],
               rep("boral", (length(Env.names))), rep(species.names[i], length(Env.names)))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", "model", "species")
  dfr <- as.data.frame(dfr)
  df <- rbind(df, dfr)
}

rownames(df) <- NULL


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

write.csv(df, "Beta_Mosquito_boral.csv")
write.csv(rho.mean, "Rho_mean_Mosquito_boral.csv")
write.csv(rho.lower, "Rho_lower_Mosquito_boral.csv")
write.csv(rho.upper, "Rho_upper_Mosquito_boral.csv")
write.csv(rho.sd, "Rho_sd_Mosquito_boral.csv")