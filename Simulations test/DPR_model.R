#########################################################
#########################################################    
###                                                   ###
###                  RUN gjam JSDM                    ###    
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

### Run gjam JSDM ----

form <- as.formula(paste("~ ", paste(colnames(X),       # Build formula from X column names
                                     collapse = "+")))

set.seed(28041948)                                      # Creator's birthday

JSDM <- gjam(formula = form,                              # Model formula
             xdata = X,                                   # Environment data
             ydata = y,                                   # Species data
             modelList = list(ng = 60000,                 # Total iterations
                              burnin = 10000,             # Burn-in
                              thin = 50,                  # Thinning
                              typeNames = 'PA',           # Data type = pres/abs
                              notStandard = colnames(X))) # Don't standardise

### Extract posteriors ----

## Beta

Beta_extract <- JSDM$chains$bgibbs                         # Extract Beta posterior from model (wrong shape)

Beta_extract <- Beta_extract[-(1:JSDM$modelList$burnin), ] # Remove burnin

Beta_extract <- Beta_extract[seq(1,                        # Implement thinning
                                 nrow(Beta_extract),
                                 JSDM$modelList$thin),] 

n_samples <- ((JSDM$modelList$ng - JSDM$modelList$burnin) / JSDM$modelList$thin)  # determine number of samples in posterior

Beta_posterior <- array(NA,                     # Create empty array of required shape
                        dim = c(ncol(X) + 1,
                                ncol(y),
                                n_samples))

for(i in seq_len(nrow(Beta_extract))){        # Fill correct shape with posterior values
  
  tmp <- Beta_extract[i, ]                      # Extract single species from list
  
  Beta_posterior[ , , i] <- tmp
  
}

## R

R_extract <- JSDM$chains$sgibbs                 # Extract R posterior from model (wrong shape)

R_extract <- R_extract[-(1:JSDM$modelList$burnin), ] # Remove burn-in

R_extract <- R_extract[seq(1, nrow(R_extract),       # Implement thinning
                           JSDM$modelList$thin), ]

R_posterior <- array(NA,                        # Create empty array of required shape
                     dim = c(ncol(y),
                             ncol(y),
                             nrow(R_extract)))

for(i in seq_len(dim(R_posterior)[3])){         # Fill correct shape with posterior values
  
  tmp <- matrix(0,                              # 0 not NA so we can use addition later
                nrow = ncol(y),
                ncol = ncol(y))
  
  tmp[lower.tri(tmp,                            # Fill upper.tri of matrix
                diag = TRUE)] <- R_extract[i, ] #  gjam includes diagonal
  
  tmp_diag <- diag(tmp)                         # Save diag because it gets
  #  doubled in next step
  tmp <- tmp + t(tmp)                           # Fill upper.tri of matrix
  
  diag(tmp) <- tmp_diag                         # Replace diagonal
  
  tmp <- cov2cor(tmp + diag(ncol(y)))           # Covariance to correlation. Diag for identifiability
  
  R_posterior[ , , i] <- tmp                    # Save each full correlation matrix to an 
  #  array slice
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