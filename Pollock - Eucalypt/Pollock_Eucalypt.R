START <- Sys.time()

library(gdata)

Eucs <- read.csv("Eucalypts_Complete.csv")

Pre.Occur <- data.frame(Eucs[, 44:63])
Pre.Occur$ALP <- ifelse((Pre.Occur[,15] == 1 | Pre.Occur[,16] == 1 | Pre.Occur[,17] == 1),1,0)
Pre.Occur$VIM <- ifelse((Pre.Occur[,18] == 1 | Pre.Occur[,19] == 1),1,0)
Pre.Occur$ARO.SAB <- ifelse((Pre.Occur[,3] == 1 | Pre.Occur[,14] == 1),1,0)

Occur <- as.matrix(Pre.Occur[, c(1,2,4,5,6,8,9,10,20,21:23)])
head(Occur)

X <- cbind(Eucs$Rockiness, Eucs$Sandiness, Eucs$VallyBotFlat, Eucs$PPTann, Eucs$Loaminess, Eucs$cvTemp, Eucs$T0)
colnames(X) <- c("Rockiness", "Sandiness", "VallyBotFlat", "PPTann", "Loaminess", "cvTemp", "T0")
head(X)

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
  
model_name <- 'Pollock_Eucalypt'

### Run the model

source('fit_JSDM.r')

write.csv(Rho, "Rho.csv")
write.csv(EnvRho, "EnvRho.csv")
write.csv(Beta, "Beta.csv")
write.csv(Mu, "Mu.csv")

save.image(file="Pollock_Eucalypt.Rdata")


difftime(Sys.time(), START)