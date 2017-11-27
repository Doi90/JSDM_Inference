library(xlsx)

############################
# Set up a blank data frame#
############################

df <- data.frame(coefficient = numeric(0), posterior.mean = numeric(0), lower = numeric(0),
                 upper = numeric(0), model = numeric(0), species = numeric(0))
colnames(df) <- c("coefficient", "posterior.mean", "lower", "upper", "model", "species")

#####################################
# Add Ovaskainen 2016 to data.frame #
#####################################

file.mean <- read.xlsx("beta_Frog2.xlsx",1, header=T)
file.lower <- read.xlsx("beta_Frog2.xlsx",5, header=T)
file.upper <- read.xlsx("beta_Frog2.xlsx",6, header=T)

model <- "Ovaskainen 2016"

row.rep <- nrow(file.mean)

for(i in colnames(file.mean)[-1]){
  name <- i
  dfr <- cbind(file.mean[, 1], file.mean[,i], file.lower[,i], file.upper[,i],
               rep(model, row.rep), rep(name, row.rep))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "model", "species")
  dfr <- as.data.frame(dfr)
  dfr$coefficient <- file.mean[, 1]
  df <- rbind(df, dfr)
}


################################
########## Write csv ###########
################################

write.csv(df,"Beta_Frog_Ovaskainen2016.csv")