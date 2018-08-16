###############################
### Working Directory stuff ###
###############################

old_wd <- getwd()

setwd("Simulations test/")

################################
### Missing run combinations ###
################################

iterator_list <- list(c(2,2,2,2,1),
                      #c(2,2,2,2,3),
                      #c(2,2,2,2,4),
                      #c(2,2,2,2,10),
                      #c(2,1,2,2,1),
                      c(2,1,2,2,6)
                      #c(2,1,2,2,8),
                      #c(2,1,2,2,10),
                      #c(1,2,1,2,3))
)

####################
### mclapply fix ###
####################

source("mclapply-hack fix.R")

################################
### Run missing combinations ###
################################

for(m in seq_len(length(iterator_list))){
  
  print(sprintf("Start combination %s",
                m))
  
  ##################################
  ### Read iterator combinations ###
  ##################################
  
  tmp <- iterator_list[[m]]
  
  beta_index <- as.numeric(tmp[1])
  corr_index <- as.numeric(tmp[2])
  cov_index <- as.numeric(tmp[3])
  model_index <- as.numeric(tmp[4])
  sim <- as.numeric(tmp[5])
  
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
                     "DPR")
  
  ## Extract IDs corresponding to command line argument indices
  
  beta_id <- beta_options[beta_index]
  
  corr_id <- corr_options[corr_index]
  
  cov_id <- cov_options[cov_index]
  
  model_id <- model_options[model_index]
  
  #####################
  ### Load packages ###
  #####################
  
  #.libPaths("/home/davidpw/R/lib/3.4")
  
  ### Model script packages
  
  if(model_id == "MPR"){
    
    library(BayesComm)
    library(coda)
    
  } else if(model_id == "HPR"){
    
    library(R2jags)
    library(parallel)
    library(random)
    library(abind)
    library(MCMCpack)
    library(MASS)
    library(mclust)
    
  } else if(model_id == "LPR"){
    
    library(boral)
    library(coda)
    
  } else if(model_id == "DPR"){
    
    library(gjam)
    
  }
  
  ######################################
  ### Ensure Output Directory Exists ###
  ######################################
  
  dir_name <- sprintf("outputs/%1$s.%2$s.%3$s",
                      beta_id,
                      corr_id,
                      cov_id)
  
  if(!dir.exists(dir_name)){
    
    dir.create(dir_name)
    
  }
  
  #for(sim in seq_len(10)){
    
    dir_name <- sprintf("outputs/%1$s.%2$s.%3$s/Sim%4$s",
                        beta_id,
                        corr_id,
                        cov_id,
                        sim)
    
    if(!dir.exists(dir_name)){
      
      dir.create(dir_name)
      
    }
  #}
  
  ########################
  ### Run Model Script ###
  ########################
  
  command <- sprintf("source('%s_model.R')",
                     model_id)
    
  tryCatch(expr = eval(parse(text = command)),
           error = function(err){
             print(sprintf("Sim %s %s%s%s: %s",
                           sim,
                           beta_id,
                           corr_id,
                           cov_id, 
                           err))
           }) 
  
  print(sprintf("End combination %s",
                m))
  
}

setwd(old_wd)
