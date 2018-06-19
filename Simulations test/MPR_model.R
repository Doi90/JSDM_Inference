#########################################################
#########################################################    
###                                                   ###
###               RUN BayesComm JSDM                  ###    
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

y <- as.matrix(y)

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

X <- as.matrix(X)

### Run BayesComm JSDM ----

set.seed(28041948)                  # Creator's Birthday

JSDM <- BC(Y = y,                   # Pres/Abs data
           X = X,                   # Predictors
           model = "full",          # Coef+Corr model
           its = 11000,             # Number of MCMC samples
           burn = 1000,             # Size of burn-in
           thin = 10)               # Amount of thinning

### Extract posteriors ----

## Beta

Beta_extract <- JSDM$trace$B                    # Extract Beta posterior from model (wrong shape)

n_samples <- (JSDM$call$its - JSDM$call$start + 1) / JSDM$call$thin  # determine number of samples in posterior

Beta_posterior <- array(NA,                     # Create empty array of required shape
                        dim = c(ncol(X) + 1,
                                ncol(y),
                                n_samples))

for(j in seq_len(length(Beta_extract))){        # Fill correct shape with posterior values
  # j = species
  tmp <- Beta_extract[[j]]                      # Extract single species from list
  
  for(i in seq_len(n_samples)){                 # Extract samples and fill Beta_posterior
    # i = sample
    Beta_posterior[ , j, i] <- tmp[i, ]         # Fill Beta_posterior
    
  }
}

## R

R_extract <- JSDM$trace$R                       # Extract R posterior from model (wrong shape)

R_posterior <- array(NA,                        # Create empty array of required shape
                     dim = c(ncol(y),
                             ncol(y),
                             nrow(R_extract)))

for(i in seq_len(dim(R_posterior)[3])){         # Fill correct shape with posterior values
  
  tmp <- matrix(0,                              # 0 not NA so we can use addition later
                nrow = ncol(y),
                ncol = ncol(y))
  
  tmp[upper.tri(tmp)] <- R_extract[i, ]         # Fill upper.tri of matrix
  
  tmp <- tmp + t(tmp)                           # Fill lower.tri of matrix
  
  diag(tmp) <- 1                                # Ones on diagonals
  
  R_posterior[ , , i] <- tmp                    # Save each full correlation matrix to an 
  # array slice
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