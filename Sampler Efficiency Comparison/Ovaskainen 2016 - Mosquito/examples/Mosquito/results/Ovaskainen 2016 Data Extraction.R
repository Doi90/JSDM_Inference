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

# Read xlsx files

file.mean <- read.xlsx("beta_Mosquito.xlsx",1, header=T)
file.lower <- read.xlsx("beta_Mosquito.xlsx",5, header=T)
file.upper <- read.xlsx("beta_Mosquito.xlsx",6, header=T)

# Species names

species <-  as.character(file.mean[,1])

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

model <- "Ovaskainen 2016"

row.rep <- nrow(file.mean)

# Create single data.frame

for(i in colnames(file.mean)){
  name <- i
  dfr <- cbind(c(rownames(file.mean)), file.mean[,i], file.lower[,i], file.upper[,i],
               rep(model, row.rep), rep(name, row.rep))
  colnames(dfr) <- c("coefficient", "posterior.mean", "lower", "upper", "model", "species")
  dfr <- as.data.frame(dfr)
  dfr$coefficient <- file.mean[, 1]
  df <- rbind(df, dfr)
}
df$coefficient <- rep(rownames(file.mean, unique(species)))
rownames(df) <- NULL

################################
########## Write csv ###########
################################

write.csv(df,"Beta_Mosquito_Ovaskainen2016.csv")
