#~~~~~~~~~~~~~~~~~~~~~~~~
# 1. housekeeping
rm(list = ls())

library(BayesComm)  # Bayesian multivariate binomial models
library(spdep)  # Moran's I test
library(ncf)  # semivariograms
library(coda)

# for plotting
library(igraph)  # networks
library(gridExtra) # ggplot layouts

#~~~~~~~~~~~~~~~~~~~~~~~~
# 2. dataset manipulation and diversity indices

# load data
dat <- read.csv("mosquito_data.csv")

# convert fauna to presence/absence
fauna <- as.matrix(ifelse(dat[, 7:22] > 0, 1, 0))

# covariates (scale is data set dependant)
preds <- as.matrix(dat[, 23:35])
preds[, 1:4] <- scale(preds[, 1:4])

#~~~~~~~~~~~~~~~~~~~~~~~~
# 4. selection of environmental predictors

# create a new dataframe with PC values and round/site factors
df <- data.frame(preds, dat$field_site)

# run forward stepwise selection across all covariates for each species
# select the model that gives the lowest AIC and tabulate which were selected

#create matrix of results (whether PC was selected)
res <- matrix(0, ncol(fauna), ncol(preds))
colnames(res) <- colnames(preds)
rownames(res) <- colnames(fauna)

# an empty list for the selected models
mods <- list()

# run the selection and record results for each species
for (i in 1:ncol(fauna)) {
  # starting model (forces in round and site)
  mlow <- glm(fauna[, i] ~ factor(dat.field_site),
              data = df, family = binomial(link = "probit"))
  # potential final model (includes all PCs)
  mup <- glm(fauna[, i] ~ factor(dat.field_site) + .,
             data = df, family = binomial(link = "probit"))
  # run model selection
  s <- step(mlow, formula(mup), direction = "forward")
  # add selected model to the list
  mods <- c(mods, list(s))
  # get the names of the selected coefficients and update the results matrix
  sel <- names(s$coefficients)
  ind <- (1:length(sel))[-grep('\\(', sel)]
  #  ind <- which(substring(sel, 1, 4) == "Comp")
  #   nos <- as.numeric(substring(sel[ind], 6, 7))
  res[i, sel[ind]] <- 1
  #   res[i, nos] <- 1
}

# matrix indicating whether a variable was selected
res

# number of times each component was selected for a species:
# sort(colSums(res), decreasing = TRUE)

# create a list of the components to use for each species in the modelling
covlist <- apply(res, 1, function(x) which(as.logical(x)))

# # ~~~~~~~~~~~~~~~~~~~~~~~~
# # 5. Bayesian multivariate binomial analysis
# 
# # set the number of iterations, how many to discard
# # and how much to thin the resulting chains
iterations <- 11000
burnin = 1000
thin = 1

# # now run the models!
# # (warning: the following take several minutes to run
# # and can be quite memory-intensive, hence the thinning)
# 
# # null model (ignores X, but we include it for consistency)
# system.time(
# 	null <- BC(Y = fauna, X = preds, model = "null",
# 		covlist = covlist,
# 		its = iterations, burn = burnin, thin = thin)
# 	)
# 
# # community model
# system.time(
# 	comm <- BC(Y = fauna, X = preds, model = "community",
# 		covlist = covlist,
# 		its = iterations, burn = burnin, thin = thin)
# 	)
# 
# # environment model
# system.time(
# 	env <- BC(Y = fauna, X = preds, model = "environment",
#     covlist = covlist,
# 		its = iterations, burn = burnin, thin = thin)
# 	)
# 
# # full model
# system.time(
#   full <- BC(Y = fauna, X = preds, model = "full",
#              covlist = covlist,
#              its = iterations, burn = burnin, thin = thin)
# )
# 
system.time(
  full <- BC(Y = fauna, X = preds, model = "full",
             its = iterations, burn = burnin, thin = thin)
)

#~~~~~~~~~~~~~~~~~~~~~~~~
# 6. deviance partitioning and network plotting

# # deviance partitioning
# dp <- devpart(null, env, comm, full)
# dp[[1]]



## Prediction

preds.new <- preds
mosq.pred <- predict(full, preds.new)

