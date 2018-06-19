#########################################################
#########################################################    
###                                                   ###
###                  RUN boral JSDM                   ###    
###                                                   ###
#########################################################
#########################################################

### Load Data ----

## Pres/Abs data

command <- sprintf("read.csv('Datasets/Sim$s/pa_%s_%s_%s.csv')", 
                   sim,
                   beta_id,
                   corr_id,
                   cov_id)                           

y <- eval(parse(text = command))                        # Evaluate command to read in data

## Site data

if(cov_id == "mo"){
  
  filename <-  sprintf("Datasets/Sim%s/meas_cov.cov",
                       sim)
  
  X <- read.csv(filename)
  
}

if(cov_id == "mu"){
  
  filename <- sprintf("Datasets/Sim%s/meas_unmeas_cov.cov",
                      sim)
  
  X <- read.csv(filename)
  
}

X <- X[ , -1]   # Remove Intercept column     

### Run boral JSDM ----

JSDM <- boral(y,                                        # Pres/Abs data
              X = X,                                    # Predictors. DO NOT INCLUDE INTERCEPT COLUMN
              family = "binomial",                      # Makes model use PA data, probit link
              num.lv = 2,                               # Set number of latent factors
              save.model = TRUE,                        # Saves JAGS model as a txt file, allows coda package to analyse MCMC
              mcmc.control = list(n.burnin = 10000,     # Burn in
                                  n.iteration = 60000,  # Total number of samples
                                  n.thin = 50,          # Amount of thinning
                                  seed = 28041948),     # Seed
              model.name = NULL)                        # Name of saved txt file. Can change, but default means dont have to change code between models

### Extract posteriors ----

## Beta

Beta_extract <- JSDM$jags.model$BUGSoutput$sims.list    # Extract Beta posterior from model (wrong shape)

n_samples <- (JSDM$mcmc.control$n.iteration -
                JSDM$mcmc.control$n.burnin) / JSDM$call$mcmc.control$n.thin  # Determine number of samples in posterior

Beta_posterior <- array(NA,                     # Create empty array of required shape
                        dim = c(ncol(X) + 1,
                                ncol(y),
                                n_samples))

for(j in seq_len(ncol(Beta_extract$X.coefs))){  # Fill correct shape with posterior values
  #  j = species
  for(i in seq_len(n_samples)){                 # Extract samples and fill Beta_posterior
    #  i = sample
    tmp <- c(Beta_extract$lv.coefs[i, j, 1],    # Intercept
             Beta_extract$X.coefs[i, j, ])      # Non-intercept regression coefficients
    
    Beta_posterior[ , j, i] <- tmp              # Fill Beta_posterior
    
  }
}

## R

R_extract <- JSDM$jags.model$BUGSoutput$sims.list$lv.coefs  # Extract factor loadings posterior from model
R_extract <- R_extract[ , , -1]                             # Drop the Intercept values

R_posterior <- array(NA,                        # Create empty array of required shape
                     dim = c(ncol(y),
                             ncol(y),
                             n_samples))

for(i in seq_len(dim(R_posterior)[3])){         # Fill correct shape with posterior values
  #  i=samples
  lambda <- R_extract[i, , ]                    # Extract factor loadings matrix
  #  lambda = J rows, H col
  tmp <- lambda %*% t(lambda) + diag(ncol(y))   # Convert factor loadings to covariance matrix
  
  tmp <- cov2cor(tmp)                           # Convert covariance matrix to correlation matrix
  
  R_posterior[ , , i] <- tmp                    # Save each full correlation matrix to an array slice
  
}

### Save Posteriors ----

filename <- sprintf("%1$s.%2$s.%3$s/Sim%5$s/beta_posterior.%1$s.%2$s.%3$s.%4$s.rds",
                    beta_id,
                    corr_id,
                    cov_id,
                    model_id,
                    sim)

saveRDS(Beta_posterior,
        filename)

filename <- sprintf("%1$s.%2$s.%3$s/Sim%5$s/rho_posterior.%1$s.%2$s.%3$s.%4$s.rds",
                    beta_id,
                    corr_id,
                    cov_id,
                    model_id,
                    sim)
saveRDS(R_posterior,
        filename)