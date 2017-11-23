# Packages

library(gjam)

# Load Data

PA <- read.csv("Mosquito_PA.csv")

Env <- read.csv("Mosquito_Covar.csv")
colnames(Env) <- c("depthCM", "temperatureC", "oxidationReductionPotentialMv", "salinityPPT", "waterCrowfootRanunculus", "rushesJuncusOrScirpus",
                   "filamentousAlgae", "emergentGrass", "ivyLeafedDuckweedLemnaTriscula", "bulrushesTypha", "reedsPhragmites",
                   "marestailHippuris", "commonDuckweedLemnaMinor")
# Model

form <- as.formula(~ depthCM + temperatureC + oxidationReductionPotentialMv + salinityPPT + waterCrowfootRanunculus + 
                     rushesJuncusOrScirpus + filamentousAlgae + emergentGrass + ivyLeafedDuckweedLemnaTriscula +
                     bulrushesTypha + reedsPhragmites + marestailHippuris + commonDuckweedLemnaMinor)

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

