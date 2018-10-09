###########################################################
###########################################################
###                                                     ###
###        SIMULATE DATASETS FOR JSDM COMPARISON        ###
###                                                     ###
###   This script simulates the different dataset       ###
### combinations for our simulated dataset comparison.  ###
### This generates all the required combinations of:    ###
###   - Hierarchical/Non-herarchical betas              ###
###   - Full/Latent factor approximated correlation     ###
###       matrix                                        ###
###   - All measured/measured + unmeasured covariates   ###
###                                                     ###
###########################################################
###########################################################

#####################
### Load Packages ###
#####################

library(tmvtnorm)

#########################
### Defined Functions ###
#########################

#----
## Full Correlation Matrix

rfull <- function (nsp, df = 2 * nsp) {
  # wishart distribution
  Sigma <- rWishart(1, df, diag(nsp))[, , 1]
  # inverse wishart distribution
  iSigma <- solve(Sigma)
  # rescale
  cov2cor(iSigma)
}

## LF Approximated Correlation Matrix

rlv <- function (nsp, nlv = 3) {
  Lambda <- matrix(rnorm(nsp * nlv), nsp, nlv)
  Sigma <- Lambda %*% t(Lambda) + diag(nsp)
  cov2cor(Sigma)
}

## Non-hierarchical Regression Coefficients

rcoef <- function (nsp, ncoef = 4, variance = 1) {
  matrix(rnorm(nsp * ncoef, 0, variance),
         ncoef,nsp)
}

## Hierarchical Regression Coefficients

rcoef_hier <- function (nsp, ncoef = 4, variance = 1, hierarchical_variance = 1) {
  means <- rnorm(ncoef, 0, hierarchical_variance)
  baseline <- rcoef(nsp, ncoef, variance)
  sweep(baseline, 1, means, "+")
}

## Measured Covariates
rcov <- function (nsite, ncov = 4) {
  matrix(rnorm(nsite * ncov),
         nsite, ncov)
}

#----

##########################
### Create Directories ###
##########################

for(i in seq_len(10)){
  
  dir_name <- sprintf("Simulations test/Datasets/Sim%s",
                      i)
  
  if(!dir.exists(dir_name)){
    
    dir.create(dir_name)
    
  }
}

#########################
### Simulate Datasets ###
#########################

for(sim in seq_len(10)){
  
  ## 10 Species, 4 Measured Covariates ----
  
  ### Full Correlation Matrix
  
  corr_full <- rfull(nsp = 10)
  
  hist(corr_full[upper.tri(corr_full)])
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/corr_full.csv",
                      sim)
  
  write.csv(corr_full,
            filename,
            row.names = FALSE)
  
  ### LF Approximated Correlation Matrix
  
  corr_approx <- rlv(nsp = 10,
                     nlv = 3)
  
  hist(corr_approx[upper.tri(corr_approx)])
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/corr_approx.csv",
                      sim)
  
  write.csv(corr_approx,
            filename,
            row.names = FALSE)
  
  ### Non-hierarchical Regression Coefficients
  
  coef_non_hier <- rcoef(nsp = 10,
                         ncoef = 5,     # 4 + Intercept
                         variance = 1)
  
  hist(coef_non_hier)
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/coef_non_hier_meas.csv",
                      sim)
  
  write.csv(coef_non_hier,
            filename,
            row.names = FALSE)
  
  ### Hierarchical Regression Coefficients
  
  coef_hier <- rcoef_hier(nsp = 10,
                          ncoef = 5,  # 4 + Intercept
                          variance = 1,
                          hierarchical_variance = 1)
  
  hist(coef_hier)
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/coef_hier_meas.csv",
                      sim)
  
  write.csv(coef_hier,
            filename,
            row.names = FALSE)
  
  ### Measured Covariates
  
  meas_cov <- rcov(nsite = 100,
                   ncov = 4) 
  
  hist(meas_cov)
  
  meas_cov <- cbind(rep(1,nrow(meas_cov)),   # Add Intercept column
                    meas_cov)
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/meas_cov.csv",
                      sim)
  
  write.csv(meas_cov,
            filename,
            row.names = FALSE)
  
  #----
  
  ## 10 Species, 4 Measured Covariates, 2 Unmeasured Covariates ----
  
  # Most of these do not need to be re-defined under this scenario
  
  ### Full Correlation Matrix
  
  # corr_full <- rfull(nsp = 10)
  # 
  # hist(corr_full[upper.tri(corr_full)])
  # 
  # write.csv(corr_full,
  #           "Simulations test/Datasets/corr_full.csv",
  #           row.names = FALSE)
  
  ### LF Approximated Correlation Matrix
  
  # corr_approx <- rlv(nsp = 10,
  #                    nlv = 3)
  # 
  # hist(corr_approx[upper.tri(corr_approx)])
  # 
  # write.csv(corr_approx,
  #           "Simulations test/Datasets/corr_approx.csv",
  #           row.names = FALSE)
  
  ### Non-hierarchical Regression Coefficients
  
  coef_non_hier_unmeas <- rcoef(nsp = 10,
                                ncoef = 7, # 4 + 2 + Intercept
                                variance = 1)
  
  hist(coef_non_hier_unmeas)
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/coef_non_hier_unmeas.csv",
                      sim)
  
  write.csv(coef_non_hier_unmeas,
            filename,
            row.names = FALSE)
  
  ### Hierarchical Regression Coefficients
  
  coef_hier_unmeas <- rcoef_hier(nsp = 10,
                                 ncoef = 7,    # 4 + 2 + Intercept
                                 variance = 1,
                                 hierarchical_variance = 1)
  
  hist(coef_hier_unmeas)
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/coef_hier_unmeas.csv",
                      sim)
  
  write.csv(coef_hier_unmeas,
            filename,
            row.names = FALSE)
  
  ### Measured Covariates
  
  meas_unmeas_cov <- rcov(nsite = 100,
                          ncov = 6) 
  
  hist(meas_unmeas_cov)
  
  meas_unmeas_cov <- cbind(rep(1, nrow(meas_unmeas_cov)),
                           meas_unmeas_cov)
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/meas_unmeas_cov.csv",
                      sim)
  
  write.csv(meas_unmeas_cov[ , 1:5], # Don't write unmeasured variables to data file
            filename,
            row.names = FALSE)
  
  #----
  
  ## Simulate Pres/Abs Data
  
  ### Measured only. Non-Hierarchical. Full Correlation. ----
  
  pa_nh_fc_mo <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_non_hier * meas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_full,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_nh_fc_mo[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_nh_fc_mo.csv",
                      sim)
  
  write.csv(pa_nh_fc_mo,
            filename,
            row.names = FALSE)
  
  ### Measured only. Non-hierarchical. Approx Correlation. ----
  
  pa_nh_ac_mo <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_non_hier * meas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_approx,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_nh_ac_mo[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_nh_ac_mo.csv",
                      sim)
  
  write.csv(pa_nh_ac_mo,
            filename,
            row.names = FALSE)
  
  ### Measured only. Hierarcichal. Full Correlation ----
  
  pa_hi_fc_mo <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_hier * meas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_full,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_hi_fc_mo[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_hi_fc_mo.csv",
                      sim)
  
  write.csv(pa_hi_fc_mo,
            filename,
            row.names = FALSE)
  
  ### Measured only. Hierachical. Approx Correlation ----
  
  pa_hi_ac_mo <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_hier * meas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_approx,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_hi_ac_mo[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_hi_ac_mo.csv",
                      sim)
  
  write.csv(pa_hi_ac_mo,
            filename,
            row.names = FALSE)
  
  ### Meas + Unmeas. Non-Hierarchical. Full Correaltion ----
  
  pa_nh_fc_mu <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_non_hier_unmeas * meas_unmeas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_full,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_nh_fc_mu[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_nh_fc_mu.csv",
                      sim)
  
  write.csv(pa_nh_fc_mu,
            filename,
            row.names = FALSE)
  
  ### Meas + Unmeas. Non-Hierarchical. Approx correlation ----
  
  pa_nh_ac_mu <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_non_hier_unmeas * meas_unmeas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_approx,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_nh_ac_mu[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_nh_ac_mu.csv",
                      sim)
  
  write.csv(pa_nh_ac_mu,
            filename,
            row.names = FALSE)
  
  ### Meas + unmeas. Hierarchical. Full correlation ----
  
  pa_hi_fc_mu <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_hier_unmeas * meas_unmeas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_full,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_hi_fc_mu[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_hi_fc_mu.csv",
                      sim)
  
  write.csv(pa_hi_fc_mu,
            filename,
            row.names = FALSE)
  
  ### Meas + Unmeas. Hierarchical. Approx correlation ----
  
  pa_hi_ac_mu <- matrix(NA,
                        nrow = 100,
                        ncol = 10)
  
  for(i in seq_len(100)){
    
    ## Calculate mean values
    
    mean_values <- coef_hier_unmeas * meas_unmeas_cov[i, ]
    
    ## Lower/Upper
    
    lower <- rep(-Inf, 10)
    upper <- rep(Inf, 10)
    
    ## Random LV draws
    
    sim_LV <- rtmvnorm(n = 1,
                       mean = colSums(mean_values),
                       sigma = corr_approx,
                       lower =  lower,
                       upper = upper)
    
    ## Convert LV draws to P/A
    
    sim_PA <- ifelse(sim_LV > 0, 1, 0)
    
    ## Save
    
    pa_hi_ac_mu[i, ] <- sim_PA
    
  }
  
  filename <- sprintf("Simulations test/Datasets/Sim%s/pa_hi_ac_mu.csv",
                      sim)
  
  write.csv(pa_hi_ac_mu,
            filename,
            row.names = FALSE)
  
  #----
  
}
