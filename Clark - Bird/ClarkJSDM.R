# Packages

library(gjam)

# Load Data

data <- read.csv("Birds_Compiled.csv")

PA <- data[,1:370]
Env <- data[,371:378]

# Model

form <- as.formula(~ bio2 + bio3 + bio5 + bio8 + bio9 + bio15 + bio16 + bio18)

system.time(
JSDM <- gjam(formula = form,
             xdata = Env,
             ydata = PA,
             modelList = list(ng = 60000,
                              burnin = 10000,
                              thin = 50,
                              typeNames = 'PA',
                              notStandard = colnames(Env)))
)

