# Packages

library(gjam)

# Load Data

data <- read.csv("Butterfly_Compiled.csv")

PA <- data[,1:55]
Env <- data[,56:59]
colnames(Env) <- c("climate", "blwood", "conwood", "chalkLimestone")
# Model

form <- as.formula(~ climate + blwood + conwood + chalkLimestone)

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

