#########################################################
#########################################################    
###                                                   ###
###                 RUN Pollock JSDM                  ###    
###                                                   ###
#########################################################
#########################################################

### Load Data ----

## Pres/Abs data

command <- sprintf("read.csv('Datasets/Sim%s/pa_%s_%s_%s.csv')", 
                   sim,
                   beta_id,
                   corr_id,
                   cov_id)                             

y <- eval(parse(text = command))                        # Evaluate command to read in data

## Site data

if(cov_id == "mo"){
  
  filename <-  sprintf("Datasets/Sim%s/meas_cov.csv",
                       sim)
  
  X <- read.csv(filename)
  
}

if(cov_id == "mu"){
  
  filename <- sprintf("Datasets/Sim%s/meas_unmeas_cov.csv",
                      sim)
  
  X <- read.csv(filename)
  
}

X <- X[ , -1]   # Remove Intercept column

### Run Pollock JSDM ----

Occur <- as.matrix(y)

X <- as.matrix(X)

## Create regression coefficient matrix

n_species <- ncol(y)

n_env_vars <- ncol(X)

coefs <- matrix(runif(n_species * (n_env_vars + 1), -1, 1),
                ncol=n_env_vars + 1)

## Set up MCMC

n.chains <- 3

n.iter <- 1000000

n.burn <- 15000

n.thin <- 1000

## Set df for inverse-Wishart prior for correlation matrix

df <- 1

## Set name for R object for raw model output

model_name <- "JSDM"

## Run the model

set.seed(28041948)          # Creator's birthday

source('fit_JSDM.R')

### Extract posteriors ----

# Pollock model runs with multiple chains that need to be combined.
# Each subsequent chain is rbind()'d after predecessor.

n_samples <- JSDM[[1]]$BUGSoutput$n.keep        # number of samples per chain

## Beta

Beta_posterior <- array(NA,                     # Create empty array of required shape
                        dim = c(ncol(X) + 1,
                                ncol(y),
                                n_samples * n.chains))

for(c in seq_len(n.chains)){                               # For each chain
  
  Beta_extract <- JSDM[[c]]$BUGSoutput$sims.list$Beta.raw  # Extract Beta posterior in wrong shape
  
  for(k in seq_len(n_env_vars + 1)){                       # For each covariate including intercept
    
    for(j in seq_len(n_species)){                          # For each species
      
      for(i in seq_len(n_samples)){                        # For each sample in chain
        
        array_dim3_id <- ((c * n_samples) - n_samples) + i  # Need to define a new array
        # Index so successive chains don't overwrite each other
        Beta_posterior[k, j, array_dim3_id] <- Beta_extract[i, j, k]  # Fill empty array with correct values
        
      }
    }
  }
}

## R

R_posterior <- array(NA,                        # Create empty array of required shape
                     dim = c(ncol(y),
                             ncol(y),
                             n_samples * n.chains))

for(c in seq_len(n.chains)){                            # For each chain
  
  R_extract <- JSDM[[c]]$BUGSoutput$sims.list$Tau       # Extract posterior in wrong shape
  
  for(i in seq_len(n_samples)){                         # For each sample
    
    array_dim3_id <- ((c * n_samples) - n_samples) + i  # Need to define a new array
    # Index so successive chains don't overwrite each other
    R_posterior[ , , array_dim3_id] <- cov2cor(R_extract[i, , ])
    
  }
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