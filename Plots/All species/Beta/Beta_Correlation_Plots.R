#####################
### Load Packages ###
#####################

library(corrplot)
library(abind)
library(ggplot2)
library(RColorBrewer)

#########################
### Defined Functions ###
#########################

# autoColourScheme() ----

autoColourScheme <- function(dataframe, colname){
  
  if("model" %in% colnames(dataframe)){
    
    model <- unique(dataframe$model)
    
    col_palette <- brewer.pal(6, "Dark2")
    
    colour <- c()
    
    if("MPR" %in% model){
      colour <- c(colour, col_palette[6])
    }
    if("HPR" %in% model){
      colour <- c(colour, col_palette[5])
    }
    if("LPR" %in% model){
      colour <- c(colour, col_palette[4])
    }
    if("DPR" %in% model){
      colour <- c(colour, col_palette[3])
    }
    if("HLR-S" %in% model){
      colour <- c(colour, col_palette[2])
    }
    if("HLR-NS" %in% model){
      colour <- c(colour, col_palette[1])
    }
  }
  
  if("Model" %in% colnames(dataframe)){
    
    model <- unique(dataframe$Model)
    
    col_palette <- brewer.pal(6, "Dark2")
    
    colour <- c()
    
    if("MPR" %in% model){
      colour <- c(colour, col_palette[6])
    }
    if("HPR" %in% model){
      colour <- c(colour, col_palette[5])
    }
    if("LPR" %in% model){
      colour <- c(colour, col_palette[4])
    }
    if("DPR" %in% model){
      colour <- c(colour, col_palette[3])
    }
    if("HLR-S" %in% model){
      colour <- c(colour, col_palette[2])
    }
    if("HLR-NS" %in% model){
      colour <- c(colour, col_palette[1])
    }
  }
  
  return(colour)
}

# ----

###################
### Load Script ###
###################

source("modelNameChange.R")

#################
### Load Data ###
#################

### BayesComm (6 datasets) ----

BC_Bird <- read.csv("Beta_df_Birds_BayesComm.csv")
BC_Bird <- modelNameChange(BC_Bird)
BC_But <- read.csv("Beta_df_Butterfly_BayesComm.csv")
BC_But <- modelNameChange(BC_But)
BC_Euc <- read.csv("Beta_df_Eucalypt_BayesComm.csv")
BC_Euc <- modelNameChange(BC_Euc)
BC_Frog <- read.csv("Beta_df_Frog_BayesComm.csv")
BC_Frog <- modelNameChange(BC_Frog)
BC_Fun <- read.csv("Beta_df_Fungi_BayesComm.csv")
BC_Fun <- modelNameChange(BC_Fun)
BC_Mosq <- read.csv("Beta_df_Mosquito_BayesComm.csv")
BC_Mosq <- modelNameChange(BC_Mosq)

### boral (6 datasets) ----

boral_Bird <- read.csv("Beta_Bird_boral.csv")
boral_Bird <- modelNameChange(boral_Bird)
boral_But <- read.csv("Beta_Butterfly_boral.csv")
boral_But <- modelNameChange(boral_But)
boral_Euc <- read.csv("Beta_Eucalypt_boral.csv")
boral_Euc <- modelNameChange(boral_Euc)
boral_Frog <- read.csv("Beta_Frog_boral.csv")
boral_Frog <- modelNameChange(boral_Frog)
boral_Fun <- read.csv("Beta_Fungi_boral.csv")
boral_Fun <- modelNameChange(boral_Fun)
boral_Mosq <- read.csv("Beta_Mosquito_boral.csv")
boral_Mosq <- modelNameChange(boral_Mosq)

### Clark (6 datasets) ----

Clark_Bird <- read.csv("Beta_Bird_Clark.csv")
Clark_Bird <- modelNameChange(Clark_Bird)
Clark_But <- read.csv("Beta_Butterflies_Clark.csv")
Clark_But <- modelNameChange(Clark_But)
Clark_Euc <- read.csv("Beta_Eucalypts_Clark.csv")
Clark_Euc <- modelNameChange(Clark_Euc)
Clark_Frog <- read.csv("Beta_Frog_Clark.csv")
Clark_Frog <- modelNameChange(Clark_Frog)
Clark_Fun <- read.csv("Beta_Fungi_Clark.csv")
Clark_Fun <- modelNameChange(Clark_Fun)
Clark_Mosq <- read.csv("Beta_Mosquito_Clark.csv")
Clark_Mosq <- modelNameChange(Clark_Mosq)

### Ovaskainen 2016 (3 datasets) ----

Ov_Bird <- BC_Bird
Ov_Bird[, 3:10] <- NA
Ov_Bird$model <- "HLR-S"
Ov_But <- BC_But
Ov_But[, 3:10] <- NA
Ov_But$model <- "HLR-S"
Ov_Euc <- read.csv("Beta_Eucalypt_Ovaskainen2016.csv")
Ov_Euc <- modelNameChange(Ov_Euc)
Ov_Frog <- read.csv("Beta_Frog_Ovaskainen2016.csv")
Ov_Frog <- modelNameChange(Ov_Frog)
Ov_Fun <- BC_Fun
Ov_Fun[, 3:10] <- NA
Ov_Fun$model <- "HLR-S"
Ov_Mosq <- read.csv("Beta_Mosquito_Ovaskainen2016.csv")
Ov_Mosq <- modelNameChange(Ov_Mosq)

### Ovaskainen 2016 NS (6 datasets) ----

OvNS_Bird <- read.csv("Beta_Bird_Ovaskainen2016NS.csv")
OvNS_Bird <- modelNameChange(OvNS_Bird)
OvNS_But <- read.csv("Beta_Butterfly_Ovaskainen2016NS.csv")
OvNS_But <- modelNameChange(OvNS_But)
OvNS_Euc <- read.csv("Beta_Eucalypt_Ovaskainen2016NS.csv")
OvNS_Euc <- modelNameChange(OvNS_Euc)
OvNS_Frog <- read.csv("Beta_Frog_Ovaskainen2016NS.csv")
OvNS_Frog <- modelNameChange(OvNS_Frog)
OvNS_Fun <- read.csv("Beta_Fungi_Ovaskainen2016NS.csv")
OvNS_Fun <- modelNameChange(OvNS_Fun)
OvNS_Mosq <- read.csv("Beta_Mosquito_Ovaskainen2016NS.csv")
OvNS_Mosq <- modelNameChange(OvNS_Mosq)

### Pollock (4 datasets) ----

Pol_Bird <- BC_Bird
Pol_Bird[, 3:10] <- NA
Pol_Bird$model <- "HPR"
Pol_But <- BC_But
Pol_But[,3:10] <- NA
Pol_But$model <- "HPR"
Pol_Euc <- read.csv("Beta_Eucalypt_Pollock.csv")
Pol_Euc <- modelNameChange(Pol_Euc)
Pol_Frog <- read.csv("Beta_Frog_Pollock.csv")
Pol_Frog <- modelNameChange(Pol_Frog)
Pol_Fun <- read.csv("Beta_Fungi_Pollock.csv")
Pol_Fun <- modelNameChange(Pol_Fun)
Pol_Mosq <- read.csv("Beta_Mosquito_Pollock.csv")
Pol_Mosq <- modelNameChange(Pol_Mosq)

# ----

model_names <- c("MPR", "HPR", "LPR", "DPR", "HLR-S", "HLR-NS")

##########################################
### Correlation Between Beta Estimates ###
##########################################

### Birds ----

Bird_list <- list(BC_Bird$posterior.mean, Pol_Bird$posterior.mean,
                  boral_Bird$posterior.mean, Clark_Bird$posterior.mean,
                  Ov_Bird$posterior.mean, OvNS_Bird$posterior.mean)

corr_Bird_vector <- c()

for(i in 1:(length(Bird_list)-1)){
  for(j in (i+1):length(Bird_list)){
    tmp <- cor(Bird_list[[i]],Bird_list[[j]], use = "na.or.complete")
    corr_Bird_vector <- c(corr_Bird_vector, tmp)
  } 
}

corr_Bird_matrix <- diag(length(Bird_list))
corr_Bird_matrix[lower.tri(corr_Bird_matrix, diag = FALSE)] <- corr_Bird_vector
corr_Bird_matrix <- corr_Bird_matrix + t(corr_Bird_matrix)
diag(corr_Bird_matrix) <- 1
colnames(corr_Bird_matrix) <- rownames(corr_Bird_matrix) <- model_names

pdf(file = "Beta_corr_Birds.pdf", width = 7, height = 7)
corrplot(corr_Bird_matrix,
         type = "upper",
         method = "color",
         diag = FALSE,
         outline = TRUE,
         na.label = "X",
         addCoef.col = "white",
         number.cex = 1.3,
         tl.col = "black",
         tl.srt = 0,
         tl.offset = 0.6,
         addgrid.col = "black")
dev.off()

### Butterflies ----

But_list <- list(BC_But$posterior.mean, Pol_But$posterior.mean,
                 boral_But$posterior.mean, Clark_But$posterior.mean,
                 Ov_But$posterior.mean, OvNS_But$posterior.mean)

corr_But_vector <- c()

for(i in 1:(length(But_list)-1)){
  for(j in (i+1):length(But_list)){
    tmp <- cor(But_list[[i]],But_list[[j]], use = "na.or.complete")
    corr_But_vector <- c(corr_But_vector, tmp)
  } 
}

corr_But_matrix <- diag(length(But_list))
corr_But_matrix[lower.tri(corr_But_matrix, diag = FALSE)] <- corr_But_vector
corr_But_matrix <- corr_But_matrix + t(corr_But_matrix)
diag(corr_But_matrix) <- 1
colnames(corr_But_matrix) <- rownames(corr_But_matrix) <- model_names

pdf(file = "Beta_corr_Butterflies.pdf", height = 7, width = 7)
corrplot(corr_But_matrix,
         type = "upper",
         method = "color",
         diag = FALSE,
         outline = TRUE,
         na.label = "X",
         addCoef.col = "white",
         number.cex = 1.3,
         addgrid.col = "black",
         tl.col = "black",
         tl.offset = 0.6,
         tl.srt = 0)
dev.off()

### Eucalypts ----

Euc_list <- list(BC_Euc$posterior.mean, Pol_Euc$posterior.mean,
                 boral_Euc$posterior.mean, Clark_Euc$posterior.mean,
                 Ov_Euc$posterior.mean, OvNS_Euc$posterior.mean)

corr_Euc_vector <- c()

for(i in 1:(length(Euc_list)-1)){
  for(j in (i+1):length(Euc_list)){
    tmp <- cor(Euc_list[[i]],Euc_list[[j]], use = "na.or.complete")
    corr_Euc_vector <- c(corr_Euc_vector, tmp)
  } 
}

corr_Euc_matrix <- diag(length(Euc_list))
corr_Euc_matrix[lower.tri(corr_Euc_matrix, diag = FALSE)] <- corr_Euc_vector
corr_Euc_matrix <- corr_Euc_matrix + t(corr_Euc_matrix)
diag(corr_Euc_matrix) <- 1
colnames(corr_Euc_matrix) <- rownames(corr_Euc_matrix) <- model_names

pdf(file = "Beta_corr_Eucalypts.pdf", height = 7, width = 7)
corrplot(corr_Euc_matrix,
         type = "upper",
         method = "color",
         diag = FALSE,
         outline = TRUE,
         na.label = "X",
         addCoef.col = "white",
         number.cex = 1.3,
         addgrid.col = "black",
         tl.col = "black",
         tl.offset = 0.6,
         tl.srt = 0)
dev.off()

### Frogs ----

Frog_list <- list(BC_Frog$posterior.mean, Pol_Frog$posterior.mean,
                  boral_Frog$posterior.mean, Clark_Frog$posterior.mean,
                  Ov_Frog$posterior.mean, OvNS_Frog$posterior.mean)

corr_Frog_vector <- c()

for(i in 1:(length(Frog_list)-1)){
  for(j in (i+1):length(Frog_list)){
    tmp <- cor(Frog_list[[i]],Frog_list[[j]])
    corr_Frog_vector <- c(corr_Frog_vector, tmp)
  } 
}

corr_Frog_matrix <- diag(length(Frog_list))
corr_Frog_matrix[lower.tri(corr_Frog_matrix, diag = FALSE)] <- corr_Frog_vector
corr_Frog_matrix <- t(corr_Frog_matrix)
colnames(corr_Frog_matrix) <- rownames(corr_Frog_matrix) <- model_names

pdf(file = "Beta_corr_Frogs.pdf", height = 7, width = 7)
corrplot(corr_Frog_matrix,
         type = "upper",
         method = "color",
         diag = FALSE,
         outline = TRUE,
         na.label = "X",
         addCoef.col = "white",
         number.cex = 1.3,
         addgrid.col = "black",
         tl.col = "black",
         tl.offset = 0.6,
         tl.srt = 0)
dev.off()

### Fungi ----

Fun_list <- list(BC_Fun$posterior.mean, Pol_Fun$posterior.mean,
                 boral_Fun$posterior.mean, Clark_Fun$posterior.mean,
                 Ov_Fun$posterior.mean, OvNS_Fun$posterior.mean)

corr_Fun_vector <- c()

for(i in 1:(length(Fun_list)-1)){
  for(j in (i+1):length(Fun_list)){
    tmp <- cor(Fun_list[[i]],Fun_list[[j]])
    corr_Fun_vector <- c(corr_Fun_vector, tmp)
  } 
}

corr_Fun_matrix <- diag(length(Fun_list))
corr_Fun_matrix[lower.tri(corr_Fun_matrix, diag = FALSE)] <- corr_Fun_vector
corr_Fun_matrix <- t(corr_Fun_matrix)
colnames(corr_Fun_matrix) <- rownames(corr_Fun_matrix) <- model_names

pdf(file = "Beta_corr_Fungi.pdf", height = 7, width = 7)
corrplot(corr_Fun_matrix,
         type = "upper",
         method = "color",
         diag = FALSE,
         outline = TRUE,
         na.label = "X",
         addCoef.col = "white",
         number.cex = 1.3,
         addgrid.col = "black",
         tl.col = "black",
         tl.offset = 0.6,
         tl.srt = 0)
dev.off()

### Mosquito ----

Mosq_list <- list(BC_Mosq$posterior.mean, Pol_Mosq$posterior.mean,
                  boral_Mosq$posterior.mean, Clark_Mosq$posterior.mean,
                  Ov_Mosq$posterior.mean, OvNS_Mosq$posterior.mean)

corr_Mosq_vector <- c()

for(i in 1:(length(Mosq_list)-1)){
  for(j in (i+1):length(Mosq_list)){
    tmp <- cor(Mosq_list[[i]],Mosq_list[[j]])
    corr_Mosq_vector <- c(corr_Mosq_vector, tmp)
  } 
}

corr_Mosq_matrix <- diag(length(Mosq_list))
corr_Mosq_matrix[lower.tri(corr_Mosq_matrix, diag = FALSE)] <- corr_Mosq_vector
corr_Mosq_matrix <- t(corr_Mosq_matrix)
colnames(corr_Mosq_matrix) <- rownames(corr_Mosq_matrix) <- model_names

pdf(file = "Beta_corr_Mosquitos.pdf", height = 7, width = 7)
corrplot(corr_Mosq_matrix,
         type = "upper",
         method = "color",
         diag = FALSE,
         outline = TRUE,
         na.label = "X",
         addCoef.col = "white",
         number.cex = 1.3,
         addgrid.col = "black",
         tl.col = "black",
         tl.offset = 0.6,
         tl.srt = 0)
dev.off()

### Average Over All Datasets ----

corr_list <- list(corr_Bird_matrix, corr_But_matrix, corr_Euc_matrix, corr_Frog_matrix,
                  corr_Fun_matrix, corr_Mosq_matrix)
corr_array <- abind(corr_list, along = 3)
Avg_corr <- apply(corr_array, MARGIN = c(1,2), FUN = mean, na.rm = TRUE)

pdf(file = "Avg_Beta_correlation_all_datasets.pdf", height = 7, width = 7)
corrplot(Avg_corr,
         type = "upper",
         method = "color",
         diag = FALSE,
         outline = TRUE,
         tl.col = "black",
         tl.srt = 0,
         tl.cex = 1.2,
         tl.offset = 0.6,
         cl.cex = 1,
         cl.align.text = "l",
         addCoef.col = "white",
         number.cex = 1.3,
         addgrid.col = "black")
dev.off()

# ----

##############################################
### Comparing 95% Credible Interval Widths ###
##############################################

### BayesComm ----

BC_Bird_Uncertainty <- BC_Bird$upper - BC_Bird$lower
BC_But_Uncertainty <- BC_But$upper - BC_But$lower
BC_Euc_Uncertainty <- BC_Euc$upper - BC_Euc$lower
BC_Frog_Uncertainty <- BC_Frog$upper - BC_Frog$lower
BC_Fun_Uncertainty <- BC_Fun$upper - BC_Fun$lower
BC_Mosq_Uncertainty <- BC_Mosq$upper - BC_Mosq$lower

### boral ----

boral_Bird_Uncertainty <- boral_Bird$upper - boral_Bird$lower
boral_But_Uncertainty <- boral_But$upper - boral_But$lower
boral_Euc_Uncertainty <- boral_Euc$upper - boral_Euc$lower
boral_Frog_Uncertainty <- boral_Frog$upper - boral_Frog$lower
boral_Fun_Uncertainty <- boral_Fun$upper - boral_Fun$lower
boral_Mosq_Uncertainty <- boral_Mosq$upper - boral_Mosq$lower

### Clark ----

Clark_Bird_Uncertainty <- Clark_Bird$upper - Clark_Bird$lower
Clark_But_Uncertainty <- Clark_But$upper - Clark_But$lower
Clark_Euc_Uncertainty <- Clark_Euc$upper - Clark_Euc$lower
Clark_Frog_Uncertainty <- Clark_Frog$upper - Clark_Frog$lower
Clark_Fun_Uncertainty <- Clark_Fun$upper - Clark_Fun$lower
Clark_Mosq_Uncertainty <- Clark_Mosq$upper - Clark_Mosq$lower

### Ovaskainen 2016 ----

Ov_Bird_Uncertainty <- Ov_Bird$upper - Ov_Bird$lower
Ov_But_Uncertainty <- Ov_But$upper - Ov_But$lower
Ov_Euc_Uncertainty <- Ov_Euc$upper - Ov_Euc$lower
Ov_Frog_Uncertainty <- Ov_Frog$upper - Ov_Frog$lower
Ov_Fun_Uncertainty <- Ov_Fun$upper <- Ov_Fun$lower
Ov_Mosq_Uncertainty <- Ov_Mosq$upper - Ov_Mosq$lower

### Ovaskainen 2016 NS ----

OvNS_Bird_Uncertainty <- OvNS_Bird$upper - OvNS_Bird$lower
OvNS_But_Uncertainty <- OvNS_But$upper - OvNS_But$lower
OvNS_Euc_Uncertainty <- OvNS_Euc$upper - OvNS_Euc$lower
OvNS_Frog_Uncertainty <- OvNS_Frog$upper - OvNS_Frog$lower
OvNS_Fun_Uncertainty <- OvNS_Fun$upper - OvNS_Fun$lower
OvNS_Mosq_Uncertainty <- OvNS_Mosq$upper - OvNS_Mosq$lower

### Pollock ----

Pol_Bird_Uncertainty <- Pol_Bird$upper - Pol_Bird$lower
Pol_But_Uncertainty <- Pol_But$upper - Pol_But$lower
Pol_Euc_Uncertainty <- Pol_Euc$upper - Pol_Euc$lower
Pol_Frog_Uncertainty <- Pol_Frog$upper - Pol_Frog$lower
Pol_Fun_Uncertainty <- Pol_Fun$upper - Pol_Fun$lower
Pol_Mosq_Uncertainty <- Pol_Mosq$upper - Pol_Mosq$lower

# ----

### Make data.frames by dataset ----

Bird_Uncertainty <- data.frame(BC_Bird_Uncertainty, Pol_Bird_Uncertainty, boral_Bird_Uncertainty,
                               Clark_Bird_Uncertainty, Ov_Bird_Uncertainty, OvNS_Bird_Uncertainty)
But_Uncertainty <- data.frame(BC_But_Uncertainty, Pol_But_Uncertainty, boral_But_Uncertainty,
                              Clark_But_Uncertainty, Ov_But_Uncertainty, OvNS_But_Uncertainty)
Euc_Uncertainty <- data.frame(BC_Euc_Uncertainty, Pol_Euc_Uncertainty, boral_Euc_Uncertainty,
                              Clark_Euc_Uncertainty, Ov_Euc_Uncertainty, OvNS_Euc_Uncertainty)
Frog_Uncertainty <- data.frame(BC_Frog_Uncertainty, Pol_Frog_Uncertainty, boral_Frog_Uncertainty,
                               Clark_Frog_Uncertainty, Ov_Frog_Uncertainty, OvNS_Frog_Uncertainty)
Fun_Uncertainty <- data.frame(BC_Fun_Uncertainty, Pol_Fun_Uncertainty, boral_Fun_Uncertainty,
                              Clark_Fun_Uncertainty, Ov_Fun_Uncertainty, OvNS_Fun_Uncertainty)
Mosq_Uncertainty <- data.frame(BC_Mosq_Uncertainty, Pol_Mosq_Uncertainty, boral_Mosq_Uncertainty,
                               Clark_Mosq_Uncertainty, Ov_Mosq_Uncertainty, OvNS_Mosq_Uncertainty)

colnames(Bird_Uncertainty) <- colnames(But_Uncertainty) <- colnames(Euc_Uncertainty) <- model_names
colnames(Frog_Uncertainty) <- colnames(Fun_Uncertainty) <- colnames(Mosq_Uncertainty) <- model_names

### Make data.frame ranks by dataset ----

Bird_Uncertainty_Rank <- c()

for(i in seq(nrow(Bird_Uncertainty))){
  tmp <- rank(Bird_Uncertainty[i,], na.last = "keep")
  Bird_Uncertainty_Rank <- rbind(Bird_Uncertainty_Rank, tmp)
}

But_Uncertainty_Rank <- c()

for(i in seq(nrow(But_Uncertainty))){
  tmp <- rank(But_Uncertainty[i,], na.last = "keep")
  But_Uncertainty_Rank <- rbind(But_Uncertainty_Rank, tmp)
}

Euc_Uncertainty_Rank <- c()

for(i in seq(nrow(Euc_Uncertainty))){
  tmp <- rank(Euc_Uncertainty[i,], na.last = "keep")
  Euc_Uncertainty_Rank <- rbind(Euc_Uncertainty_Rank, tmp)
}

Frog_Uncertainty_Rank <- c()

for(i in seq(nrow(Frog_Uncertainty))){
  tmp <- rank(Frog_Uncertainty[i,], na.last = "keep")
  Frog_Uncertainty_Rank <- rbind(Frog_Uncertainty_Rank, tmp)
}

Fun_Uncertainty_Rank <- c()

for(i in seq(nrow(Fun_Uncertainty))){
  tmp <- rank(Fun_Uncertainty[i,], na.last = "keep")
  Fun_Uncertainty_Rank <- rbind(Fun_Uncertainty_Rank, tmp)
}

Mosq_Uncertainty_Rank <- c()

for(i in seq(nrow(Mosq_Uncertainty))){
  tmp <- rank(Mosq_Uncertainty[i,], na.last = "keep")
  Mosq_Uncertainty_Rank <- rbind(Mosq_Uncertainty_Rank, tmp)
}

### All Datasets dataframe ----

All_Datasets_Uncertainty_Rank <- rbind(Bird_Uncertainty_Rank, But_Uncertainty_Rank, Euc_Uncertainty_Rank,
                                       Frog_Uncertainty_Rank, Fun_Uncertainty_Rank, Mosq_Uncertainty_Rank)

Uncertainty_Dataframe <- c()
Uncertainty_Dataframe <- rbind(Uncertainty_Dataframe, apply(Bird_Uncertainty_Rank, 2, mean, na.rm = TRUE))
Uncertainty_Dataframe <- rbind(Uncertainty_Dataframe, apply(But_Uncertainty_Rank, 2, mean, na.rm = TRUE))
Uncertainty_Dataframe <- rbind(Uncertainty_Dataframe, apply(Euc_Uncertainty_Rank, 2, mean, na.rm = TRUE))
Uncertainty_Dataframe <- rbind(Uncertainty_Dataframe, apply(Frog_Uncertainty_Rank, 2, mean, na.rm = TRUE))
Uncertainty_Dataframe <- rbind(Uncertainty_Dataframe, apply(Fun_Uncertainty_Rank, 2, mean, na.rm = TRUE))
Uncertainty_Dataframe <- rbind(Uncertainty_Dataframe, apply(Mosq_Uncertainty_Rank, 2, mean, na.rm = TRUE))
Uncertainty_Dataframe <- rbind(Uncertainty_Dataframe, apply(Uncertainty_Dataframe, 2, mean, na.rm = TRUE))
rownames(Uncertainty_Dataframe) <- c("Bird", "Butterfly", "Eucalypts", "Frogs", "Fungi",
                                     "Mosquito", "Average")

# ----

#################################
### Make Aggregated Dataframe ###
#################################

# ----

Birds_df <- rbind(BC_Bird, Pol_Bird, boral_Bird, Clark_Bird, Ov_Bird, OvNS_Bird)
Birds_df <- cbind(Birds_df, rep("Birds", nrow(Birds_df)))
Birds_df <- Birds_df[,-1]
colnames(Birds_df)[12] <- "Dataset"
Birds_df$coefficient <- rep(c("Intercept", "Bio2", "Bio3", "Bio5", "Bio8", "Bio9", "Bio15", "Bio16",
                              "Bio18"),nrow(Birds_df)/9)

Butterflies_df <- rbind(BC_But, Pol_But, boral_But, Clark_But, Ov_But, OvNS_But)
Butterflies_df <- cbind(Butterflies_df, rep("Butterflies", nrow(Butterflies_df)))
Butterflies_df <- Butterflies_df[,-1]
colnames(Butterflies_df)[12] <- "Dataset"
Butterflies_df$coefficient <- rep(c("Intercept", "Climate", "Broad-leaved Woodland", "Coniferous Woodland",
                                    "Calcareous Substrates"), nrow(Butterflies_df)/5)

Eucalypts_df <- rbind(BC_Euc, Pol_Euc, boral_Euc, Clark_Euc, Ov_Euc, OvNS_Euc)
Eucalypts_df <- cbind(Eucalypts_df, rep("Eucalypts", nrow(Eucalypts_df)))
Eucalypts_df <- Eucalypts_df[,-1]
colnames(Eucalypts_df)[12] <- "Dataset"
Eucalypts_df$coefficient <- rep(c("Intercept", "Rockiness", "Sandiness", "Valley Bottom Flat", "Annual Precipitation",
                                  "Loaminess", "cvTemp", "T0"), nrow(Eucalypts_df)/8)

Frogs_df <- rbind(BC_Frog, Pol_Frog, boral_Frog, Clark_Frog, Ov_Frog, OvNS_Frog)
Frogs_df <- cbind(Frogs_df, rep("Frogs", nrow(Frogs_df)))
Frogs_df <- Frogs_df[,-1]
colnames(Frogs_df)[12] <- "Dataset"
Frogs_df$coefficient <- rep(c("Intercept", "Pond Surface Area", "Vertical Wall", "Road Density"),nrow(Frogs_df)/4)

Fungi_df <- rbind(BC_Fun, Pol_Fun, boral_Fun, Clark_Fun, Ov_Fun, OvNS_Fun)
Fungi_df <- cbind(Fungi_df, rep("Fungi", nrow(Fungi_df)))
Fungi_df <- Fungi_df[,-1]
colnames(Fungi_df)[12] <- "Dataset"
Fungi_df$coefficient <- rep(c("Intercept", "diam", "dc1", "dc2", "dc3", "dc4", "dc5", "quality3", "quality4", "ground3",
                              "ground4", "epi", "bark"), nrow(Fungi_df)/13)

Mosquitos_df <- rbind(BC_Mosq, Pol_Mosq, boral_Mosq, Clark_Mosq, Ov_Mosq, OvNS_Mosq)
Mosquitos_df <- cbind(Mosquitos_df, rep("Mosquitos", nrow(Mosquitos_df)))
Mosquitos_df <- Mosquitos_df[,-1]
colnames(Mosquitos_df)[12] <- "Dataset"
Mosquitos_df$coefficient <- rep(c("Intercept", "Depth (cm)", "Temperature (Celsius)", "Oxidation Reduction Potential (Mv)",
                                  "Salinity (ppm)", "Water Crowfoot", "Rushes", "Filamentous Algae", "Emergent Grass",
                                  "Ivy Leafed Duckweed", "Bulrushes", "Reeds", "Marestail", "Common Duckweed"),
                                nrow(Mosquitos_df)/14)

All_Datasets_df <- rbind(Birds_df, Butterflies_df, Eucalypts_df, Frogs_df, Fungi_df, Mosquitos_df)

# ----

###########################################
### Beta Coefficient Estimate Strengths ###
###########################################

### Birds ----

## Set coefficient factor levels

Birds_df$coefficient <- factor(Birds_df$coefficient,
                               levels = c("Intercept", "Bio2", "Bio3", "Bio5",
                                          "Bio8", "Bio9", "Bio15", "Bio16",
                                          "Bio18"))
## Set Colour Scale 

colour <- autoColourScheme(Birds_df[Birds_df$model != "HPR" &
                                      Birds_df$model != "HLR-S", ])

## Plots

ggplot(Birds_df[Birds_df$coefficient != "Intercept",],
       aes(coefficient, sd, fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,0.014)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Standard Deviation")
ggsave("Beta_sd_Birds.pdf", units = "in", width = 7, height = 7)

ggplot(Birds_df, aes(coefficient, abs(coefVar), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,4)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Coefficient of variation")
ggsave("Beta_coefVar_Birds.pdf", units = "in", width = 7, height = 7)

ggplot(Birds_df, aes(coefficient, abs(qcd), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,3)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quatile coefficient of dispersion (0.25 - 0.75)")
ggsave("Beta_qcd_Birds.pdf", units = "in", width = 7, height = 7)

ggplot(Birds_df, aes(coefficient, abs(qcd2), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,7)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.025 - 0.975")
ggsave("Beta_qcd2_Birds.pdf", units = "in", width = 7, height = 7)

ggplot(Birds_df, aes(coefficient, abs(gini), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,2)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Gini Coefficient")
ggsave("Beta_gini_Birds.pdf", units = "in", width = 7, height = 7)

### Butterflies ----

## Set coefficient factor levels

Butterflies_df$coefficient <- factor(Butterflies_df$coefficient,
                                     levels = c("Intercept", "Broad-leaved Woodland",
                                                "Calcareous Substrates", "Climate",
                                                "Coniferous Woodland"))
## Set Colour Scale 

colour <- autoColourScheme(Butterflies_df[Butterflies_df$model != "HPR" &
                                            Butterflies_df$model != "HLR-S",])

## Plots

ggplot(Butterflies_df, aes(coefficient, sd, fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 75)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Standard deviation")
ggsave("Beta_sd_Butterflies.pdf", units = "in", width = 7, height = 7)

ggplot(Butterflies_df, aes(coefficient, abs(coefVar), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,3.5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Coefficient of variation")
ggsave("Beta_coefVar_Butterflies.pdf", units = "in", width = 7, height = 7)

ggplot(Butterflies_df, aes(coefficient, abs(qcd), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,2.5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.25 - 0.75)")
ggsave("Beta_qcd_Butterflies.pdf", units = "in", width = 7, height = 7)

ggplot(Butterflies_df, aes(coefficient, abs(qcd2), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,6)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.025 - 0.975)")
ggsave("Beta_qcd2_Butterflies.pdf", units = "in", width = 7, height = 7)

ggplot(Butterflies_df, aes(coefficient, abs(gini), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,2)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Gini coefficient")
ggsave("Beta_gini_Butterflies.pdf", units = "in", width = 7, height = 7)

### Eucalypts ----

## Set factor level for coefficients

Eucalypts_df$coefficient <- factor(Eucalypts_df$coefficient,
                                   levels = c("Intercept", "Annual Precipitation",
                                              "cvTemp", "Loaminess", "Rockiness",
                                              "Sandiness", "T0",
                                              "Valley Bottom Flat"))
## Set Colour Scale 

colour <- autoColourScheme(Eucalypts_df)

## Plots

ggplot(Eucalypts_df, aes(coefficient, sd, fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Standard deviation")
ggsave("Beta_sd_Eucalypts.pdf", units = "in", width = 7, height = 7)

ggplot(Eucalypts_df, aes(coefficient, abs(coefVar), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,6)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Coefficient of variation")
ggsave("Beta_coefVar_Eucalypts.pdf", units = "in", width = 7, height = 7)

ggplot(Eucalypts_df, aes(coefficient, abs(qcd), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.25 - 0.75)")
ggsave("Beta_qcd_Eucalypts.pdf", units = "in", width = 7, height = 7)

ggplot(Eucalypts_df, aes(coefficient, abs(qcd2), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,9)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.025 - 0.975)")
ggsave("Beta_qcd2_Eucalypts.pdf", units = "in", width = 7, height = 7)

ggplot(Eucalypts_df, aes(coefficient, abs(gini), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,3.5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Gini Coefficient")
ggsave("Beta_gini_Eucalypts.pdf", units = "in", width = 7, height = 7)

### Frogs ----

## Set factor levels for coefficients

Frogs_df$coefficient <- factor(Frogs_df$coefficient,
                               levels = c("Intercept", "Pond Surface Area",
                                          "Road Density", "Vertical Wall"))
## Set Colour Scale 

colour <- autoColourScheme(Frogs_df)

## Plots

ggplot(Frogs_df, aes(coefficient, sd, fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,150)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Standard deviation")
ggsave("Beta_sd_Frogs.pdf", units = "in", width = 7, height = 7)

ggplot(Frogs_df, aes(coefficient, abs(coefVar), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,6)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Coefficient of variation")
ggsave("Beta_coefVar_Frogs.pdf", units = "in", width = 7, height = 7)

ggplot(Frogs_df, aes(coefficient, abs(qcd), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,4)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.25 - 0.75)")
ggsave("Beta_qcd_Frogs.pdf", units = "in", width = 7, height = 7)

ggplot(Frogs_df, aes(coefficient, abs(qcd2), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,12)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.025 - 0.975)")
ggsave("Beta_qcd2_Frogs.pdf", units = "in", width = 7, height = 7)

ggplot(Frogs_df, aes(coefficient, abs(gini), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,3.5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Gini Coefficient")
ggsave("Beta_gini_Frogs.pdf", units = "in", width = 7, height = 7)

### Fungi ----

## Set factor levels for coefficients

Fungi_df$coefficient <- factor(Fungi_df$coefficient,
                               levels = c("Intercept", "bark", "dc1", "dc2", "dc3",
                                          "dc4", "dc5", "diam", "epi", "ground3",
                                          "ground4", "quality3", "quality4"))
## Set Colour Scale 

colour <- autoColourScheme(Fungi_df[Fungi_df$model != "HLR-S", ])

## Plots

ggplot(Fungi_df, aes(coefficient, sd, fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,8.5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Standard deviation")
ggsave("Beta_sd_Fungi.pdf", units = "in", width = 7, height = 7)

ggplot(Fungi_df, aes(coefficient, abs(coefVar), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,12)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Coefficient of variation")
ggsave("Beta_coefVar_Fungi.pdf", units = "in", width = 7, height = 7)

ggplot(Fungi_df, aes(coefficient, abs(qcd), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,10)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.25 - 0.75)")
ggsave("Beta_qcd_Fungi.pdf", units = "in", width = 7, height = 7)

ggplot(Fungi_df, aes(coefficient, abs(qcd2), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,12)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.025  0.975)")
ggsave("Beta_qcd2_Fungi.pdf", units = "in", width = 7, height = 7)

ggplot(Fungi_df, aes(coefficient, abs(gini), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,10)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Gini Coefficient")
ggsave("Beta_gini_Fungi.pdf", units = "in", width = 7, height = 7)

### Mosquitos ----

## Set factor levels for coefficients

Mosquitos_df$coefficient <- factor(Mosquitos_df$coefficient,
                                   levels = c("Intercept", "Bulrushes", "Common Duckweed",
                                              "Depth (cm)", "Emergent Grass",
                                              "Filamentous Algae", "Ivy Leafed Duckweed",
                                              "Marestail", "Oxidation Reduction Potential (Mv)",
                                              "Reeds", "Rushes", "Salinity (ppm)",
                                              "Temperature (Celsius)", "Water Crowfoot"))
## Set Colour Scale 

colour <- autoColourScheme(Mosquitos_df)

## Plots

ggplot(Mosquitos_df, aes(coefficient, sd, fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,10)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Standard deviation")
ggsave("Beta_sd_Mosquitos.pdf", units = "in", width = 7, height = 7)

ggplot(Mosquitos_df, aes(coefficient, abs(coefVar), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,14)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Coefficient of variation")
ggsave("Beta_coefVar_Mosquitos.pdf", units = "in", width = 7, height = 7)

ggplot(Mosquitos_df, aes(coefficient, abs(qcd), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,9)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.25 - 0.75)")
ggsave("Beta_qcd_Mosquitos.pdf", units = "in", width = 7, height = 7)

ggplot(Mosquitos_df, aes(coefficient, abs(qcd2), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,30)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Quartile coefficient of dispersion (0.025 - 0.975)")
ggsave("Beta_qcd2_Mosquitos.pdf", units = "in", width = 7, height = 7)

ggplot(Mosquitos_df, aes(coefficient, abs(gini), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,7)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Covariate",
       y = "Gini Coefficient")
ggsave("Beta_gini_Mosquitos.pdf", units = "in", width = 7, height = 7)

### All Datasets ----

## Set factor levels for model

All_Datasets_df$model <- factor(All_Datasets_df$model,
                                levels = c("MPR","HPR","LPR","DPR",
                                           "HLR-S","HLR-NS"))
## Set Colour Scale 

colour <- autoColourScheme(All_Datasets_df)

## Plots

ggplot(All_Datasets_df, aes(model, sd, fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 1.5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Standard deviation")
ggsave("Beta_sd_All.pdf", units = "in", width = 7, height = 7)

ggplot(All_Datasets_df, aes(model, abs(coefVar), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,4)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Coefficient of variation")
ggsave("Beta_coefVar_All.pdf", units = "in", width = 7, height = 7)

ggplot(All_Datasets_df, aes(model, abs(qcd), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,2.5)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Quartile coefficient of dispersion (0.25 - 0.75)")
ggsave("Beta_qcd_All.pdf", units = "in", width = 7, height = 7)

ggplot(All_Datasets_df, aes(model, abs(qcd2), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,7)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Quartile coefficient of dispersion (0.025 - 0.975")
ggsave("Beta_qcd2_All.pdf", units = "in", width = 7, height = 7)

ggplot(All_Datasets_df, aes(model, abs(gini), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,2)) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Gini Coefficient")
ggsave("Beta_gini_All.pdf", units = "in", width = 7, height = 7)

