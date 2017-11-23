# Packages

library(gjam)

# Load Data

data <- read.csv("Fungi_Compiled.csv")

PA <- data[,1:11]
Env <- data[,12:23]

# Model

form <- as.formula(~ diam + dc1 + dc2 + dc3 + dc4 + dc5 + quality3 + quality4 + ground3 +
                     ground4 + epi + bark)

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

