# Packages

library(gjam)

# Load Data

data <- read.csv("Eucalypt_Compiled.csv")

PA <- data[,1:12]
Env <- data[,13:19]

# Model

form <- as.formula(~ Rockiness + Sandiness + VallyBotFlat + PPTann + Loaminess + cvTemp + T0)

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

