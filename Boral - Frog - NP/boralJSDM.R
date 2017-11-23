# Load packages

library(boral)

# Read in data

PA <- read.csv("Frogs_PA.csv")
PA <- PA[,-10]

Env <- read.csv("Frogs_Covar.csv")
Env <- Env[,-1]

# model

system.time(
  
JSDM <- boral(PA,  # PA data
              X = Env,  # Covariates, DO NOT INCLUDE INTERCEPT COLUMN
              family = "binomial",  # makes model use PA data, probit link
              num.lv = 2, # set number of latent variables
              save.model = TRUE, # saves JAGS model as a txt file, allows coda package to analyse MCMC
              mcmc.control = list(n.burnin = 10000,  # mcmc set up. using set up from Warton TREE paper
                                  n.iteration = 60000,
                                  n.thin = 50,
                                  seed = 28041948),
              model.name = NULL,
              prior.control = list(type = c("normal","normal","normal","uniform"), 
                                   hypparams = c(100, 20, 1, 50), ssvs.index = -1, ssvs.g = 1e-6))  # name of saved txt file. Can change, but default means dont have to change code between models

)
