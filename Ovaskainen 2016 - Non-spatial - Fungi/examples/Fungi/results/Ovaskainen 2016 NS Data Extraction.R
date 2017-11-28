library(openxlsx)
library(ineq)

############################
# Set up a blank data frame#
############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), sd = numeric(0), coefVar = numeric(0), qcd = numeric(0),
                 qcd2 = numeric(0), gini = numeric(0), model = numeric(0), species = numeric(0))
colnames(df) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                  "coefVar", "qcd", "qcd2", "gini", "model", "species")

#####################################
# Add Ovaskainen 2016 to data.frame #
#####################################

# Read xlsx files

file.mean <- openxlsx::read.xlsx("beta_Fungi.xlsx", 1, colNames = TRUE)
file.lower <- read.xlsx("beta_Fungi.xlsx",5, colNames = TRUE)
file.upper <- read.xlsx("beta_Fungi.xlsx",6, colNames = TRUE)
beta <- read.csv("beta_Fungi.csv", header = FALSE)

# Load standardisation data

dataset_sd <- read.csv("Fungi_sd.csv")
dataset_sd <- dataset_sd[,2]
dataset_sd <- c(1, dataset_sd)

# Species names

species <-  as.character(file.mean[,1])

# Standardise beta

dataset_sd_long_form <- numeric(0)

for(i in seq(length(dataset_sd))){
  tmp <- rep(dataset_sd[i], length(species))
  dataset_sd_long_form <- c(dataset_sd_long_form, tmp)
}

beta_standardised <- beta
beta_standardised <- t(t(beta_standardised)/rep(dataset_sd_long_form))

# Transpose data frames

file.mean <- do.call(rbind, c(file.mean))
file.mean <- file.mean[-1,]
colnames(file.mean) <- species
file.lower <- do.call(rbind, c(file.lower))
file.lower <- file.lower[-1,]
colnames(file.lower) <- species
file.upper <- do.call(rbind, c(file.upper))
file.upper <- file.upper[-1,]
colnames(file.upper) <- species

# Set model name

model <- "Ovaskainen 2016 NS"

row.rep <- nrow(file.mean)

# define functions

coefVar <- function(vector){
  sd.vec <- sd(vector)
  mean.vec <- mean(vector)
  cv <- sd.vec/mean.vec
  return(cv)
}

qcd <- function(vector){
  q1 <- quantile(vector, probs = 0.25)
  q3 <- quantile(vector, probs = 0.75)
  qcd <- (q3-q1)/(q3+q1)
  return(qcd)
}

qcd2 <- function(vector){
  q1 <- quantile(vector, probs = 0.025)
  q3 <- quantile(vector, probs = 0.975)
  qcd2 <- (q3-q1)/(q3+q1)
  return(qcd2)
}

# create matrix of function values

sd.vec <- apply(X = beta_standardised, MARGIN = 2, FUN = sd)
sd.matrix <- matrix(sd.vec, ncol = ncol(file.mean), nrow = nrow(file.mean), byrow = TRUE)

coefVar.vec <- apply(X = beta_standardised, MARGIN = 2, FUN = coefVar)
coefVar.matrix <- matrix(coefVar.vec, ncol = ncol(file.mean), nrow = nrow(file.mean), byrow = TRUE)

qcd.vec <- apply(X = beta_standardised, MARGIN = 2, FUN = qcd)
qcd.matrix <- matrix(qcd.vec, ncol = ncol(file.mean), nrow = nrow(file.mean), byrow = TRUE)

qcd2.vec <- apply(X = beta_standardised, MARGIN = 2, FUN = qcd2)
qcd2.matrix <- matrix(qcd2.vec, ncol = ncol(file.mean), nrow = nrow(file.mean), byrow = TRUE)

gini.vec <- apply(X = beta_standardised, MARGIN = 2, FUN = ineq, type = "Gini")
gini.matrix <- matrix(gini.vec, ncol = ncol(file.mean), nrow = nrow(file.mean), byrow = TRUE)

rownames(sd.matrix) <- rownames(coefVar.matrix) <- rownames(qcd.matrix) <-
  rownames(qcd2.matrix) <- rownames(gini.matrix) <- rownames(file.mean)

colnames(sd.matrix) <- colnames(coefVar.matrix) <- colnames(qcd.matrix) <-
  colnames(qcd2.matrix) <- colnames(gini.matrix) <- colnames(file.mean)
# Create single data.frame

for(i in colnames(file.mean)){
  name <- i
  dfr <- cbind(c(rownames(file.mean)), file.mean[,i], file.lower[,i], file.upper[,i],
               sd.matrix[,i], coefVar.matrix[,i], qcd.matrix[,i], qcd2.matrix[,i],
               gini.matrix[,i], rep(model, row.rep), rep(name, row.rep))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "sd", 
                     "coefVar", "qcd", "qcd2", "gini", "model", "species")
  dfr <- as.data.frame(dfr)
  dfr$coefficient <- file.mean[, 1]
  df <- rbind(df, dfr)
}
df$coefficient <- rep(rownames(file.mean, unique(species)))
rownames(df) <- NULL

################################
########## Write csv ###########
################################

write.csv(df,"Beta_Fungi_Ovaskainen2016NS.csv")
