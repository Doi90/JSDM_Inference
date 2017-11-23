#####################
### Load Packages ###
#####################

library(stargazer)

#################
### Run Times ###
#################

runTimes <- data.frame(
  MPR = c(3.80, 0.23, "<0.02", "<0.02", "<0.02", "<0.02"),
  MLR = c(">168", ">168", 142.12, 14.05, ">168", ">168"),
  HPR = c("NA", ">168", 7.58, 0.94, 15.79, 6.42),
  LPR = c(120.41, 13.86, 0.33, 0.04, 0.62, 0.14),
  DPR = c(27.26, 6.48, 0.25, 0.06, 0.67, 0.73),
  HLR_S = c(">168", ">168", 50.02, 1.35, "NA", 1.98),
  HLR_NS = c(15.21, 2.10, 0.21, 0.13, 0.26, 0.20)
)

rownames(runTimes) <- c("Birds", "Butterflies", "Eucalypts", "Frogs", "Fungi", "Mosquitos")

stargazer(runTimes,
          summary = FALSE,
          title = "Model runtimes (in hours)")

###########################
### Compatible Datasets ###
###########################

compatibleDatasets <- data.frame(
  MPR = c("X", "X", "X", "X", "X", "X"),
  MLR = c(" ", " ", "X", "X", " ", " "),
  HPR = c(" ", " ", "X", "X", "X", "X"),
  LPR = c("X", "X", "X", "X", "X", "X"),
  DPR = c("X", "X", "X", "X", "X", "X"),
  HLR_S = c(" ", " ", "X", "X", " ", "X"),
  HLR_NS = c("X", "X", "X", "X", "X", "X")
)

rownames(compatibleDatasets) <- c("Birds", "Butterflies", "Eucalypts", "Frogs", "Fungi", "Mosquitos")

stargazer(compatibleDatasets,
          summary = FALSE,
          title = "Model compatability with datasets")

#######################
### Dataset Summary ###
#######################

datasetSummary <- data.frame(
  Source = c("Harris (2015)", "Ovaskainen et al (2016)",
             "Pollock et al (2014)", "Pollock et al (2014)",
             "Ovaskainen et al (2010)", "Golding et al (2015)"),
  Geographic_location = c("North America", "Great Britain",
                          "Grampians National Park, Australia",
                          "Melbourne, Australia",
                          "Southern Finland",
                          "South-East England"),
  Species = c(370, 55, 12, 9, 11, 16),
  Sites = c(2752, 2609, 458, 104, 800, 167),
  Covariates = c(8, 4, 7, 3, 15, 13)
)

rownames(datasetSummary) <- c("Birds", "Butterflies", "Eucalypts", "Frogs", "Fungi", "Mosquitos")

stargazer(datasetSummary,
          summary = FALSE,
          title = "Dataset summary")

############################
### Model Features Table ###
############################

modelFeatures <- data.frame(
  Source = c("Golding et al (2015)", "Ovaskainen et al (2010)",
             "Pollock et al (2014)", "Hui (2016)", "Clark et al (2017)",
             "Ovaskainen et al (2016)", "Ovaskainen et al (2016)"),
  R_package = c("Bayescomm", "NA", "NA", "boral", "GJAM", "NA", "NA"),
  Hierarchical_regression_coefficients = c(" ", " ", "X", " ", " ", "X", "X"),
  Dirichlet_process = c(" ", " ", " ", " ", "*", " ", " "),
  Latent_factors = c(" ", " ", " ", "X", "*", "X", "X"),
  Multivariate = c("X", "X", "X", "X", "X", "X", "X"),
  Probit = c("X", " ", "X", "X", "X", "X", "X"),
  Logistic = c(" ", "X", " ", " ", " ", " ", " "),
  Regression = c("X", "X", "X", "X", "X", "X", "X"),
  Spatially_explicit = c(" ", " ", " ", " ", " ", "X", " ")
)  

rownames(modelFeatures) <- c("MPR", "MLR", "HPR", "LPR",
                             "DPR", "HLR-S", "HLR-NS")  

stargazer(modelFeatures,
          summary = FALSE,
          title = "Modelling method components")

summary(modelFeatures)

############################
### Mathematical Symbols ###
############################

