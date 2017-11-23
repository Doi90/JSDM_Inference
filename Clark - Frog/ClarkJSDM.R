# Packages

library(gjam)

# Load Data

PA <- read.csv("Frogs_PA.csv")
PA <- PA[,-10]

Env <- read.csv("Frogs_Covar.csv")
Env <- Env[,-1]
colnames(Env) <- c("area", "VerticalWall", "road")

# Model

form <- as.formula(~ area + VerticalWall + road)

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

