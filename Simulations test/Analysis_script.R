#######################################################
#######################################################
###                                                 ###
###                 ANALYSIS SCRIPT                 ###
###                                                 ###
###   This script takes the posterior distributions ###
### from each dataset/simulation combo and extracts ###
### all of our relevant summary statistics.         ###
###                                                 ###
#######################################################
#######################################################

#####################
### Load packages ###
#####################

library(Metrics)

###########################
### Combination options ###
###########################

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

sim_options <- 1:10

##########################
### Build empty arrays ###
##########################

### Regression coefficients

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(sim in sim_options){
        
        # Rows = covariates (including intercept)
        # Columns = species
        # Slices = model (including true values)
        
        command <- sprintf("array_%s.%s.%s.%s_beta <- array(NA,
                                                            dim = c(5,
                                                                    10,
                                                                    6))",
                           beta,
                           corr,
                           cov,
                           sim)
        
        eval(parse(text = command))
        
      }
    }
  }
}

### Correlation coefficients

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(sim in sim_options){
        
        # Rows = species
        # Columns = species
        # Slices = model (including true values)
        
        command <- sprintf("array_%s.%s.%s.%s_rho <- array(NA,
                                                           dim = c(10,
                                                                   10,
                                                                   6))",
                           beta,
                           corr,
                           cov,
                           sim)
        
        eval(parse(text = command))
        
      }
    }
  }
}

#######################
### Get true values ###
#######################

### Regression coefficients

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(sim in sim_options){
        
        ### Read in true values
        
        if(beta == "nh" & cov == "mo"){
          
          filename <- sprintf("Simulations test/Datasets/Sim%s/coef_non_hier_meas.csv",
                              sim)
          
          tmp_tv <- read.csv(filename,
                             header = TRUE)
          
          rownames(tmp_tv) <- colnames(tmp_tv) <- NULL
          
          tmp_tv <- as.matrix(tmp_tv)
          
          command <- sprintf("array_%s.%s.%s.%s_beta[ , , 6] <- tmp_tv",
                             beta,
                             corr,
                             cov,
                             sim)
          
          eval(parse(text = command))
          
        }
        
        if(beta == "nh" & cov == "mu"){
          
          filename <- sprintf("Simulations test/Datasets/Sim%s/coef_non_hier_unmeas.csv",
                              sim)
          
          tmp_tv <- read.csv(filename,
                             header = TRUE)
          
          rownames(tmp_tv) <- colnames(tmp_tv) <- NULL
          
          tmp_tv <- as.matrix(tmp_tv)
          
          tmp_tv <- tmp_tv[1:5, ] # Don't need coefficients for unmeasured variables here
          
          command <- sprintf("array_%s.%s.%s.%s_beta[ , , 6] <- tmp_tv",
                             beta,
                             corr,
                             cov,
                             sim)
          
          eval(parse(text = command))
          
        }
        
        if(beta == "hi" & cov == "mo"){
          
          filename <- sprintf("Simulations test/Datasets/Sim%s/coef_hier_meas.csv",
                              sim)
          
          tmp_tv <- read.csv(filename,
                             header = TRUE)
          
          rownames(tmp_tv) <- colnames(tmp_tv) <- NULL
          
          tmp_tv <- as.matrix(tmp_tv)
          
          command <- sprintf("array_%s.%s.%s.%s_beta[ , , 6] <- tmp_tv",
                             beta,
                             corr,
                             cov,
                             sim)
          
          eval(parse(text = command))
          
        }
        
        if(beta == "hi" & cov == "mu"){
          
          filename <- sprintf("Simulations test/Datasets/Sim%s/coef_hier_unmeas.csv",
                              sim)
          
          tmp_tv <- read.csv(filename,
                             header = TRUE)
          
          rownames(tmp_tv) <- colnames(tmp_tv) <- NULL
          
          tmp_tv <- as.matrix(tmp_tv)
          
          tmp_tv <- tmp_tv[1:5, ] # Don't need coefficients for unmeasured variables here
          
          command <- sprintf("array_%s.%s.%s.%s_beta[ , , 6] <- tmp_tv",
                             beta,
                             corr,
                             cov,
                             sim)
          
          eval(parse(text = command))
          
        }
      }    
    }
  }
}

### Correlation coefficients

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(sim in sim_options){
        
        if(corr == "fc"){
          
          filename <- sprintf("Simulations test/Datasets/Sim%s/corr_full.csv",
                              sim)
          
          tmp_tv <- read.csv(filename)
          
          rownames(tmp_tv) <- colnames(tmp_tv) <- NULL
          
          tmp_tv <- as.matrix(tmp_tv)
          
          command <- sprintf("array_%s.%s.%s.%s_rho[ , , 6] <- tmp_tv",
                             beta,
                             corr,
                             cov,
                             sim)
          
          eval(parse(text = command))
          
        }
        
        if(corr == "ac"){
          
          filename <- sprintf("Simulations test/Datasets/Sim%s/corr_approx.csv",
                              sim)
          
          tmp_tv <- read.csv(filename)
          
          rownames(tmp_tv) <- colnames(tmp_tv) <- NULL
          
          tmp_tv <- as.matrix(tmp_tv)
          
          command <- sprintf("array_%s.%s.%s.%s_rho[ , , 6] <- tmp_tv",
                             beta,
                             corr,
                             cov,
                             sim)
          
          eval(parse(text = command))
          
        }
      }
    }
  }
}

###########################
### Get model estimates ###
###########################

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(model in model_options){
        
        for(sim in sim_options){
          
          ### Regression coefficients
          
          filename <- sprintf("Simulations test/outputs/%1$s.%2$s.%3$s/Sim%5$s/beta_posterior.%1$s.%2$s.%3$s.%4$s.rds",
                              beta,
                              corr,
                              cov,
                              model,
                              sim)
          
          if(file.exists(filename)){
            
            beta_pos <- readRDS(filename)
            
            beta_mean <- apply(beta_pos,
                               c(1,2),
                               mean,
                               na.rm = TRUE)
            
          } else {
            
            beta_mean <- matrix(NA,
                                nrow = 5,
                                ncol = 10)
            
          }
          
          model_id <- which(model_options == model)
          
          command <- sprintf("array_%s.%s.%s.%s_beta[ , , %s] <- beta_mean",
                             beta,
                             corr,
                             cov,
                             sim,
                             model_id)
          
          eval(parse(text = command))
          
          ### Correlation coefficients
          
          filename <- sprintf("Simulations test/outputs/%1$s.%2$s.%3$s/Sim%5$s/rho_posterior.%1$s.%2$s.%3$s.%4$s.rds",
                              beta,
                              corr,
                              cov,
                              model,
                              sim)
          
          if(file.exists(filename)){
            
            rho_pos <- readRDS(filename)
            
            rho_mean <- apply(rho_pos,
                              c(1,2),
                              mean,
                              na.rm = TRUE)
            
          } else {
            
            rho_mean <- matrix(NA,
                               nrow = 5,
                               ncol = 10)
            
          }
          
          model_id <- which(model_options == model)
          
          command <- sprintf("array_%s.%s.%s.%s_rho[ , , %s] <- rho_mean",
                             beta,
                             corr,
                             cov,
                             sim,
                             model_id)
          
          eval(parse(text = command))
          
        }
      }
    }
  }
}

#############################################
### Calculate difference from true values ###
#############################################

### Regression coefficients

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(sim in sim_options){
        
        array_name <- sprintf("array_%s.%s.%s.%s_beta",
                              beta,
                              corr,
                              cov,
                              sim)
        
        command <- sprintf("%1$s_diff <- %1$s",
                           array_name)
        
        eval(parse(text = command))
        
        for(i in seq_len(length(model_options))){
          
          command <- sprintf("%1$s_diff[ , , i] <- %1$s_diff[ , , i] - %1$s_diff[ , , 6]",
                             array_name)
          
          eval(parse(text = command))
          
        }
        
      }
    }
  }
}

### Correlation coefficients

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(sim in sim_options){
        
        array_name <- sprintf("array_%s.%s.%s.%s_rho",
                              beta,
                              corr,
                              cov,
                              sim)
        
        command <- sprintf("%1$s_diff <- %1$s",
                           array_name)
        
        eval(parse(text = command))
        
        for(i in seq_len(length(model_options))){
          
          command <- sprintf("%1$s_diff[ , , i] <- %1$s_diff[ , , i] - %1$s_diff[ , , 6]",
                             array_name)
          
          eval(parse(text = command))
          
        }
        
      }
    }
  }
}

### Save to tables

#### Create empty tables

##### beta_mean

true_beta_mean <- matrix(NA,
                         nrow = 8,
                         ncol = 5)

rownames(true_beta_mean) <- c("nh_fc_mo",
                              "nh_fc_mu",
                              "nh_ac_mo",
                              "nh_ac_mu",
                              "hi_fc_mo",
                              "hi_fc_mu",
                              "hi_ac_mo",
                              "hi_ac_mu")

colnames(true_beta_mean) <- c("MPR",
                              "HPR",
                              "LPR",
                              "DPR",
                              "HLR")

##### beta_sd

true_beta_sd <- matrix(NA,
                       nrow = 8,
                       ncol = 5)

rownames(true_beta_sd) <- c("nh_fc_mo",
                            "nh_fc_mu",
                            "nh_ac_mo",
                            "nh_ac_mu",
                            "hi_fc_mo",
                            "hi_fc_mu",
                            "hi_ac_mo",
                            "hi_ac_mu")

colnames(true_beta_sd) <- c("MPR",
                            "HPR",
                            "LPR",
                            "DPR",
                            "HLR")

##### rho_mean

true_rho_mean <- matrix(NA,
                        nrow = 8,
                        ncol = 5)

rownames(true_rho_mean) <- c("nh_fc_mo",
                             "nh_fc_mu",
                             "nh_ac_mo",
                             "nh_ac_mu",
                             "hi_fc_mo",
                             "hi_fc_mu",
                             "hi_ac_mo",
                             "hi_ac_mu")

colnames(true_rho_mean) <- c("MPR",
                             "HPR",
                             "LPR",
                             "DPR",
                             "HLR")

##### rho_sd

true_rho_sd <- matrix(NA,
                      nrow = 8,
                      ncol = 5)

rownames(true_rho_sd) <- c("nh_fc_mo",
                           "nh_fc_mu",
                           "nh_ac_mo",
                           "nh_ac_mu",
                           "hi_fc_mo",
                           "hi_fc_mu",
                           "hi_ac_mo",
                           "hi_ac_mu")

colnames(true_rho_sd) <- c("MPR",
                           "HPR",
                           "LPR",
                           "DPR",
                           "HLR")

### Fill tables with summary values

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(model in model_options){
        
        for(type in c("beta", "rho")){
          
          model_id <- which(model_options == model)
          
          esti_vec <- c(eval(parse(text = sprintf("array_%s.%s.%s.1_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.2_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.3_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.4_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.5_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.6_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.7_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.8_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.9_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.10_%s_diff[ , , %s]",
                                                  beta,
                                                  corr,
                                                  cov,
                                                  type,
                                                  model_id))))
          
          dataset <- sprintf("%s_%s_%s",
                             beta,
                             corr,
                             cov)
          
          command <- sprintf("true_%s_mean['%s', '%s'] <- mean(esti_vec,
                                                               na.rm = TRUE)",
                             type,
                             dataset,
                             model)
          
          eval(parse(text = command))
          
          command <- sprintf("true_%s_sd['%s', '%s'] <- sd(esti_vec,
                                                           na.rm = TRUE)",
                             type,
                             dataset,
                             model)
          
          eval(parse(text = command))
          
        }
      }
    }
  }
}

######################
### Calculate RMSE ###
######################

### Save to tables

#### Create empty tables

##### rmse_beta

rmse_beta <- matrix(NA,
                    nrow = 8,
                    ncol = 5)

rownames(rmse_beta) <- c("nh_fc_mo",
                         "nh_fc_mu",
                         "nh_ac_mo",
                         "nh_ac_mu",
                         "hi_fc_mo",
                         "hi_fc_mu",
                         "hi_ac_mo",
                         "hi_ac_mu")

colnames(rmse_beta) <- c("MPR",
                         "HPR",
                         "LPR",
                         "DPR",
                         "HLR")

##### rmse_rho

rmse_rho <- matrix(NA,
                   nrow = 8,
                   ncol = 5)

rownames(rmse_rho) <- c("nh_fc_mo",
                        "nh_fc_mu",
                        "nh_ac_mo",
                        "nh_ac_mu",
                        "hi_fc_mo",
                        "hi_fc_mu",
                        "hi_ac_mo",
                        "hi_ac_mu")

colnames(rmse_rho) <- c("MPR",
                        "HPR",
                        "LPR",
                        "DPR",
                        "HLR")

### Fill tables with summary values

for(beta in beta_options){
  
  for(corr in corr_options){
    
    for(cov in cov_options){
      
      for(model in model_options){
        
        for(type in c("beta", "rho")){
          
          model_id <- which(model_options == model)
          
          true_vec <- c(eval(parse(text = sprintf("array_%s.%s.%s.1_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.2_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.3_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.4_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.5_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.6_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.7_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.8_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.9_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))),
                                eval(parse(text = sprintf("array_%s.%s.%s.10_%s[ , , 6]",
                                beta,
                                corr,
                                cov,
                                type))))
          
          esti_vec <- c(eval(parse(text = sprintf("array_%s.%s.%s.1_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.2_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.3_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.4_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.5_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.6_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.7_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.8_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.9_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))),
                        eval(parse(text = sprintf("array_%s.%s.%s.10_%s[ , , %s]",
                                beta,
                                corr,
                                cov,
                                type,
                                model_id))))
          
          dataset <- sprintf("%s_%s_%s",
                             beta,
                             corr,
                             cov)
          
          command <- sprintf("rmse_%s['%s', '%s'] <- rmse(actual = true_vec,
                                                          predicted = esti_vec)",
                             type,
                             dataset,
                             model)
          
          eval(parse(text = command))
          
        }
      }
    }
  }
}
