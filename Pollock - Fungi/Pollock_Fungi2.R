START <- Sys.time()

### Create P/A Matrix

Occur <- read.csv('Fungi_Compiled.csv')
Occur <- as.matrix(Occur[, 1:11])

### Create Environmental Variables Matrix

X <- read.csv('Fungi_Compiled.csv')
X <- as.matrix(X[, 12:26])
  
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
  
model_name <- 'Pollock_Fungi'

### Run the model

source('fit_JSDM.r')

difftime(Sys.time(), START)

write.csv(Rho, "Rho.csv")
write.csv(EnvRho, "EnvRho.csv")
write.csv(Beta, "Beta.csv")
write.csv(Mu, "Mu.csv")

save.image(file="Pollock_Fungi.Rdata")


