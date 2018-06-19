###############################################
###############################################
###                                         ###
###      HLR MODE POSTERIOR EXTRACTION      ###
###                                         ###
###   Extract the HLR model posteriors with ###
### the correct format. Save to same        ###
### location as the other models [Outputs]  ###
###                                         ###
###############################################
###############################################

##################################
### Command line arguments and ###
###       defining indices     ###
##################################

## Read in the command line arguments

command_args <- commandArgs(trailingOnly = TRUE)

beta_index <- as.numeric(command_args[1])
corr_index <- as.numeric(command_args[2])
cov_index <- as.numeric(command_args[3])
model_index <- as.numeric(command_args[4])
sim <- as.numeric(command_args[5])

## Possible ID options

beta_options <- c("nh",
                  "hi")

corr_options <- c("fc",
                  "ac")

cov_options <- c("mo",
                 "mu")

model_options <- c("MPR",
                   "HPR",
                   "LPR",
                   "DPR",
                   "HLR")

## Extract IDs corresponding to command line argument indices

beta_id <- beta_options[beta_index]

corr_id <- corr_options[corr_index]

cov_id <- cov_options[cov_index]

model_id <- model_options[model_index]

#############################
### Load in model outputs ###
#############################

beta_file <- sprintf("MATLAB/examples/%1$s_%2$s_%3$s_%4$s/posteriors/beta_%1$s_%2$s_%3$s_%4$s.csv",
                     beta_id,
                     corr_id,
                     cov_id,
                     sim)

beta <- read.csv(beta_file,
                 header = FALSE)

rho_file <- sprintf("MATLAB/examples/%1$s_%2$s_%3$s_%4$s/posteriors/lambda_%1$s_%2$s_%3$s_%4$s.csv",
                    beta_id,
                    corr_id,
                    cov_id,
                    sim)

rho <- read.csv(rho_file,
                header = FALSE)

spp_file <- sprintf("MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/species_%1$s_%2$s_%3$s_%4$s.txt",
                    beta_id,
                    corr_id,
                    cov_id,
                    sim)

spp_list <- as.character(read.table(spp_file,
                                    header = FALSE))

cov_file <- sprintf("MATLAB/examples/%1$s_%2$s_%3$s_%4$s/data/covariates_%1$s_%2$s_%3$s_%4$s.txt",
                    beta_id,
                    corr_id,
                    cov_id,
                    sim)

cov_list <- as.character(read.table(cov_file,
                                    header = FALSE))

########################
### Define constants ###
########################

n_species <- 10

n_covariates <- dim(beta)[2]/n_species #includes Intercept

n_samples <- dim(beta)[1]

n_lf <- dim(rho)[2]/n_samples

##########################
### Extract posteriors ###
##########################

### Beta ----

Beta_posterior <- array(NA,                     # Create empty array of required shape
                        dim = c(n_covariates,
                                n_species,
                                n_samples))

for(i in seq_len(n_species)){
  
  for(j in seq_len(n_samples)){
    
    col_id <- (i * n_covariates) - n_covariates + 1  # column id to move through dataframe by sample_id
    
    tmp <- beta[j, col_id:(col_id + n_covariates - 1)] # Extract betas for spp i in samp j
    
    Beta_posterior[ , n_species, n_samples] <- tmp  # Fill correct format posterior
    
  }
}

### Rho ----

R_posterior <- array(NA,                        # Create empty array of required shape
                     dim = c(n_species,
                             n_species,
                             n_samples))

for(i in seq_len(n_samples)){
  
  col_id <- (i * n_lf) - n_lf + 1 # column id to move through dataframe by sample_id
  
  tmp <- rho[ , col_id:(col_id + n_lf - 1)] # Extract lambdas for sample i
  
  tmp <- tmp %*% t(tmp) + diag(n_species) # Convert lambdas to covariance matrix
  
  tmp <- cov2cor(tmp) # Convert covariance matrix to correlation matrix

  R_posterior[ , , i] <- tmp  # Fill correct format posterior

}

### Save Posteriors ----

filename <- sprintf("outputs/%1$s.%2$s.%3$s/Sim%5$s/beta_posterior.%1$s.%2$s.%3$s.%4$s.rds",
                    beta_id,
                    corr_id,
                    cov_id,
                    model_id,
                    sim)

saveRDS(Beta_posterior,
        filename)

filename <- sprintf("outputs/%1$s.%2$s.%3$s/Sim%5$s/rho_posterior.%1$s.%2$s.%3$s.%4$s.rds",
                    beta_id,
                    corr_id,
                    cov_id,
                    model_id,
                    sim)
saveRDS(R_posterior,
        filename)