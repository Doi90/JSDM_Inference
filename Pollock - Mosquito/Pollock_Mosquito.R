START <- Sys.time()

### Create P/A Matrix

Occur <- read.csv('Mosquito_PA.csv')
Occur <- as.matrix(Occur)

### Create Environmental Variables Matrix

X <- read.csv('Mosquito_Covar.csv')
X <- as.matrix(X)
  
### Create Regression Coefs Matrix
  
n_species <- ncol(Occur)
n_env_vars <- ncol(X)
coefs <- matrix(runif(n_species * (n_env_vars + 1),-1,1), ncol=n_env_vars + 1)

  
### Set up MCMC

n.chains <- 3
n.iter <- 1000000
n.burn <- 15000
n.thin <- 1000
  
  
### Set df for inverse-Wishart prior for correlation matrix
  
df <- 1
  
### Set name for R object for raw model output
  
model_name <- 'Pollock_Mosquito'

### Run the model

source('fit_JSDM.r')

difftime(Sys.time(), START)

write.csv(Rho, "Rho.csv")
write.csv(EnvRho, "EnvRho.csv")
write.csv(Beta, "Beta.csv")
write.csv(Mu, "Mu.csv")

save.image(file="Pollock_Mosquito.Rdata")


