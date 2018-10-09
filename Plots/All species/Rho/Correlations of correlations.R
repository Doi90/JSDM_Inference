#####################
### Load Packages ###
#####################

library(corrplot)
library(openxlsx)
library(abind)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

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

#################
### Load Data ###
################

## BayesComm

### Birds ----

BayesComm_Bird_Mean <- read.csv("Rho_mean_Bird_BayesComm.csv")
BayesComm_Bird_Mean <- BayesComm_Bird_Mean[,-1]
BayesComm_Bird_Mean <- as.matrix(BayesComm_Bird_Mean)
colnames(BayesComm_Bird_Mean) <- rownames(BayesComm_Bird_Mean) <- 1:ncol(BayesComm_Bird_Mean)

BayesComm_Bird_Lower <- read.csv("Rho_lower_Bird_BayesComm.csv")
BayesComm_Bird_Lower <- BayesComm_Bird_Lower[,-1]
BayesComm_Bird_Lower <- as.matrix(BayesComm_Bird_Lower)
colnames(BayesComm_Bird_Lower) <- rownames(BayesComm_Bird_Lower) <- 1:ncol(BayesComm_Bird_Lower)

BayesComm_Bird_upper <- read.csv("Rho_upper_Bird_BayesComm.csv")
BayesComm_Bird_upper <- BayesComm_Bird_upper[,-1]
BayesComm_Bird_upper <- as.matrix(BayesComm_Bird_upper)
colnames(BayesComm_Bird_upper) <- rownames(BayesComm_Bird_upper) <- 1:ncol(BayesComm_Bird_upper)

Uncertainty_BayesComm_Bird <- BayesComm_Bird_upper - BayesComm_Bird_Lower
colnames(Uncertainty_BayesComm_Bird) <- rownames(Uncertainty_BayesComm_Bird) <- 1:ncol(Uncertainty_BayesComm_Bird)
Uncertainty_BayesComm_Bird <- as.matrix(Uncertainty_BayesComm_Bird)

### Butterflies ----

BayesComm_Butterfly_Mean <- read.csv("Rho_mean_Butterfly_BayesComm.csv")
rownames(BayesComm_Butterfly_Mean) <- BayesComm_Butterfly_Mean[,1]
BayesComm_Butterfly_Mean <- BayesComm_Butterfly_Mean[,-1]
BayesComm_Butterfly_Mean <- as.matrix(BayesComm_Butterfly_Mean)
colnames(BayesComm_Butterfly_Mean) <- rownames(BayesComm_Butterfly_Mean) <- 1:ncol(BayesComm_Butterfly_Mean)

BayesComm_Butterfly_Lower <- read.csv("Rho_lower_Butterfly_BayesComm.csv")
rownames(BayesComm_Butterfly_Lower) <- BayesComm_Butterfly_Lower[,1]
BayesComm_Butterfly_Lower <- BayesComm_Butterfly_Lower[,-1]
BayesComm_Butterfly_Lower <- as.matrix(BayesComm_Butterfly_Lower)
colnames(BayesComm_Butterfly_Lower) <- rownames(BayesComm_Butterfly_Lower) <- 1:ncol(BayesComm_Butterfly_Lower)

BayesComm_Butterfly_upper <- read.csv("Rho_upper_Butterfly_BayesComm.csv")
rownames(BayesComm_Butterfly_upper) <- BayesComm_Butterfly_upper[,1]
BayesComm_Butterfly_upper <- BayesComm_Butterfly_upper[,-1]
BayesComm_Butterfly_upper <- as.matrix(BayesComm_Butterfly_upper)
colnames(BayesComm_Butterfly_upper) <- rownames(BayesComm_Butterfly_upper) <- 1:ncol(BayesComm_Butterfly_upper)

Uncertainty_BayesComm_Butterfly <- BayesComm_Butterfly_upper - BayesComm_Butterfly_Lower
colnames(Uncertainty_BayesComm_Butterfly) <- rownames(Uncertainty_BayesComm_Butterfly) <- 1:ncol(Uncertainty_BayesComm_Butterfly)
Uncertainty_BayesComm_Butterfly <- as.matrix(Uncertainty_BayesComm_Butterfly)


### Eucalypts ----

BayesComm_Eucalypts_Mean <- read.csv("Rho_mean_Eucalypt_BayesComm.csv")
BayesComm_Eucalypts_Mean <- BayesComm_Eucalypts_Mean[,-1]
BayesComm_Eucalypts_Mean <- as.matrix(BayesComm_Eucalypts_Mean)
colnames(BayesComm_Eucalypts_Mean) <- rownames(BayesComm_Eucalypts_Mean) <- 1:ncol(BayesComm_Eucalypts_Mean)

BayesComm_Eucalypts_Lower <- read.csv("Rho_lower_Eucalypt_BayesComm.csv")
BayesComm_Eucalypts_Lower <- BayesComm_Eucalypts_Lower[,-1]
BayesComm_Eucalypts_Lower <- as.matrix(BayesComm_Eucalypts_Lower)
colnames(BayesComm_Eucalypts_Lower) <- rownames(BayesComm_Eucalypts_Lower) <- 1:ncol(BayesComm_Eucalypts_Lower)

BayesComm_Eucalypts_upper <- read.csv("Rho_upper_Eucalypt_BayesComm.csv")
BayesComm_Eucalypts_upper <- BayesComm_Eucalypts_upper[,-1]
BayesComm_Eucalypts_upper <- as.matrix(BayesComm_Eucalypts_upper)
colnames(BayesComm_Eucalypts_upper) <- rownames(BayesComm_Eucalypts_upper) <- 1:ncol(BayesComm_Eucalypts_upper)

Uncertainty_BayesComm_Eucalypts <- BayesComm_Eucalypts_upper - BayesComm_Eucalypts_Lower
colnames(Uncertainty_BayesComm_Eucalypts) <- rownames(Uncertainty_BayesComm_Eucalypts) <- 1:ncol(Uncertainty_BayesComm_Eucalypts)
Uncertainty_BayesComm_Eucalypts <- as.matrix(Uncertainty_BayesComm_Eucalypts)

### Frogs ----

BayesComm_Frog_Mean <- read.csv("Rho_mean_Frog_BayesComm.csv")
BayesComm_Frog_Mean <- BayesComm_Frog_Mean[,-1]
BayesComm_Frog_Mean <- as.matrix(BayesComm_Frog_Mean)
colnames(BayesComm_Frog_Mean) <- rownames(BayesComm_Frog_Mean) <- 1:ncol(BayesComm_Frog_Mean)

BayesComm_Frog_Lower <- read.csv("Rho_lower_Frog_BayesComm.csv")
BayesComm_Frog_Lower <- BayesComm_Frog_Lower[,-1]
BayesComm_Frog_Lower <- as.matrix(BayesComm_Frog_Lower)
colnames(BayesComm_Frog_Lower) <- rownames(BayesComm_Frog_Lower) <- 1:ncol(BayesComm_Frog_Lower)

BayesComm_Frog_upper <- read.csv("Rho_upper_Frog_BayesComm.csv")
BayesComm_Frog_upper <- BayesComm_Frog_upper[,-1]
BayesComm_Frog_upper <- as.matrix(BayesComm_Frog_upper)
colnames(BayesComm_Frog_upper) <- rownames(BayesComm_Frog_upper) <- 1:ncol(BayesComm_Frog_upper)

Uncertainty_BayesComm_Frog <- BayesComm_Frog_upper - BayesComm_Frog_Lower
colnames(Uncertainty_BayesComm_Frog) <- rownames(Uncertainty_BayesComm_Frog) <- 1:ncol(Uncertainty_BayesComm_Frog)
Uncertainty_BayesComm_Frog <- as.matrix(Uncertainty_BayesComm_Frog)

### Fungi ----

BayesComm_Fungi_Mean <- read.csv("Rho_mean_Fungi_BayesComm.csv")
BayesComm_Fungi_Mean <- BayesComm_Fungi_Mean[,-1]
BayesComm_Fungi_Mean <- as.matrix(BayesComm_Fungi_Mean)
colnames(BayesComm_Fungi_Mean) <- rownames(BayesComm_Fungi_Mean) <- 1:ncol(BayesComm_Fungi_Mean)

BayesComm_Fungi_Lower <- read.csv("Rho_lower_Fungi_BayesComm.csv")
BayesComm_Fungi_Lower <- BayesComm_Fungi_Lower[,-1]
BayesComm_Fungi_Lower <- as.matrix(BayesComm_Fungi_Lower)
colnames(BayesComm_Fungi_Lower) <- rownames(BayesComm_Fungi_Lower) <- 1:ncol(BayesComm_Fungi_Lower)

BayesComm_Fungi_upper <- read.csv("Rho_upper_Fungi_BayesComm.csv")
BayesComm_Fungi_upper <- BayesComm_Fungi_upper[,-1]
BayesComm_Fungi_upper <- as.matrix(BayesComm_Fungi_upper)
colnames(BayesComm_Fungi_upper) <- rownames(BayesComm_Fungi_upper) <- 1:ncol(BayesComm_Fungi_upper)

Uncertainty_BayesComm_Fungi <- BayesComm_Fungi_upper - BayesComm_Fungi_Lower
colnames(Uncertainty_BayesComm_Fungi) <- rownames(Uncertainty_BayesComm_Fungi) <- 1:ncol(Uncertainty_BayesComm_Fungi)
Uncertainty_BayesComm_Fungi <- as.matrix(Uncertainty_BayesComm_Fungi)

### Mosquitos ----

BayesComm_Mosquito_Mean <- read.csv("Rho_mean_Mosquito_BayesComm.csv")
BayesComm_Mosquito_Mean <- BayesComm_Mosquito_Mean[,-1]
BayesComm_Mosquito_Mean <- as.matrix(BayesComm_Mosquito_Mean)
colnames(BayesComm_Mosquito_Mean) <- rownames(BayesComm_Mosquito_Mean) <- 1:ncol(BayesComm_Mosquito_Mean)

BayesComm_Mosquito_Lower <- read.csv("Rho_lower_Mosquito_BayesComm.csv")
BayesComm_Mosquito_Lower <- BayesComm_Mosquito_Lower[,-1]
BayesComm_Mosquito_Lower <- as.matrix(BayesComm_Mosquito_Lower)
colnames(BayesComm_Mosquito_Lower) <- rownames(BayesComm_Mosquito_Lower) <- 1:ncol(BayesComm_Mosquito_Lower)

BayesComm_Mosquito_upper <- read.csv("Rho_upper_Mosquito_BayesComm.csv")
BayesComm_Mosquito_upper <- BayesComm_Mosquito_upper[,-1]
BayesComm_Mosquito_upper <- as.matrix(BayesComm_Mosquito_upper)
colnames(BayesComm_Mosquito_upper) <- rownames(BayesComm_Mosquito_upper) <- 1:ncol(BayesComm_Mosquito_upper)

Uncertainty_BayesComm_Mosquito <- BayesComm_Mosquito_upper - BayesComm_Mosquito_Lower
colnames(Uncertainty_BayesComm_Mosquito) <- rownames(Uncertainty_BayesComm_Mosquito) <- 1:ncol(Uncertainty_BayesComm_Mosquito)
Uncertainty_BayesComm_Mosquito <- as.matrix(Uncertainty_BayesComm_Mosquito)

# ----

## boral

### Birds ----

boral_Bird_Mean <- read.csv("Rho_mean_Bird_boral.csv")
boral_Bird_Mean <- boral_Bird_Mean[,-1]
boral_Bird_Mean <- as.matrix(boral_Bird_Mean)
colnames(boral_Bird_Mean) <- rownames(boral_Bird_Mean) <- 1:ncol(boral_Bird_Mean)

boral_Bird_Lower <- read.csv("Rho_lower_Bird_boral.csv")
boral_Bird_Lower <- boral_Bird_Lower[,-1]
boral_Bird_Lower <- as.matrix(boral_Bird_Lower)
colnames(boral_Bird_Lower) <- rownames(boral_Bird_Lower) <- 1:ncol(boral_Bird_Lower)

boral_Bird_upper <- read.csv("Rho_upper_Bird_boral.csv")
boral_Bird_upper <- boral_Bird_upper[,-1]
boral_Bird_upper <- as.matrix(boral_Bird_upper)
colnames(boral_Bird_upper) <- rownames(boral_Bird_upper) <- 1:ncol(boral_Bird_upper)

Uncertainty_boral_Bird <- boral_Bird_upper - boral_Bird_Lower
colnames(Uncertainty_boral_Bird) <- rownames(Uncertainty_boral_Bird) <- 1:ncol(Uncertainty_boral_Bird)
Uncertainty_boral_Bird <- as.matrix(Uncertainty_boral_Bird)

### Butterflies ----

boral_Butterfly_Mean <- read.csv("Rho_mean_Butterfly_boral.csv")
boral_Butterfly_Mean <- boral_Butterfly_Mean[,-1]
boral_Butterfly_Mean <- as.matrix(boral_Butterfly_Mean)
colnames(boral_Butterfly_Mean) <- rownames(boral_Butterfly_Mean) <- 1:ncol(boral_Butterfly_Mean)

boral_Butterfly_Lower <- read.csv("Rho_lower_Butterfly_boral.csv")
boral_Butterfly_Lower <- boral_Butterfly_Lower[,-1]
boral_Butterfly_Lower <- as.matrix(boral_Butterfly_Lower)
colnames(boral_Butterfly_Lower) <- rownames(boral_Butterfly_Lower) <- 1:ncol(boral_Butterfly_Lower)

boral_Butterfly_upper <- read.csv("Rho_upper_Butterfly_boral.csv")
boral_Butterfly_upper <- boral_Butterfly_upper[,-1]
boral_Butterfly_upper <- as.matrix(boral_Butterfly_upper)
colnames(boral_Butterfly_upper) <- rownames(boral_Butterfly_upper) <- 1:ncol(boral_Butterfly_upper)

Uncertainty_boral_Butterfly <- boral_Butterfly_upper - boral_Butterfly_Lower
colnames(Uncertainty_boral_Butterfly) <- rownames(Uncertainty_boral_Butterfly) <- 1:ncol(Uncertainty_boral_Butterfly)
Uncertainty_boral_Butterfly <- as.matrix(Uncertainty_boral_Butterfly)

### Eucalypts ----

boral_Eucalypts_Mean <- read.csv("Rho_mean_Eucalypt_boral.csv")
boral_Eucalypts_Mean <- boral_Eucalypts_Mean[,-1]
boral_Eucalypts_Mean <- as.matrix(boral_Eucalypts_Mean)
colnames(boral_Eucalypts_Mean) <- rownames(boral_Eucalypts_Mean) <- 1:ncol(boral_Eucalypts_Mean)

boral_Eucalypts_Lower <- read.csv("Rho_lower_Eucalypt_boral.csv")
boral_Eucalypts_Lower <- boral_Eucalypts_Lower[,-1]
boral_Eucalypts_Lower <- as.matrix(boral_Eucalypts_Lower)
colnames(boral_Eucalypts_Lower) <- rownames(boral_Eucalypts_Lower) <- 1:ncol(boral_Eucalypts_Lower)

boral_Eucalypts_upper <- read.csv("Rho_upper_Eucalypt_boral.csv")
boral_Eucalypts_upper <- boral_Eucalypts_upper[,-1]
boral_Eucalypts_upper <- as.matrix(boral_Eucalypts_upper)
colnames(boral_Eucalypts_upper) <- rownames(boral_Eucalypts_upper) <- 1:ncol(boral_Eucalypts_upper)

Uncertainty_boral_Eucalypts <- boral_Eucalypts_upper - boral_Eucalypts_Lower
colnames(Uncertainty_boral_Eucalypts) <- rownames(Uncertainty_boral_Eucalypts) <- 1:ncol(Uncertainty_boral_Eucalypts)
Uncertainty_boral_Eucalypts <- as.matrix(Uncertainty_boral_Eucalypts)

### Frogs ----

boral_Frog_Mean <- read.csv("Rho_mean_Frog_boral.csv")
boral_Frog_Mean <- boral_Frog_Mean[,-1]
boral_Frog_Mean <- as.matrix(boral_Frog_Mean)
colnames(boral_Frog_Mean) <- rownames(boral_Frog_Mean) <- 1:ncol(boral_Frog_Mean)

boral_Frog_Lower <- read.csv("Rho_lower_Frog_boral.csv")
boral_Frog_Lower <- boral_Frog_Lower[,-1]
boral_Frog_Lower <- as.matrix(boral_Frog_Lower)
colnames(boral_Frog_Lower) <- rownames(boral_Frog_Lower) <- 1:ncol(boral_Frog_Lower)

boral_Frog_upper <- read.csv("Rho_upper_Frog_boral.csv")
boral_Frog_upper <- boral_Frog_upper[,-1]
boral_Frog_upper <- as.matrix(boral_Frog_upper)
colnames(boral_Frog_upper) <- rownames(boral_Frog_upper) <- 1:ncol(boral_Frog_upper)

Uncertainty_boral_Frog <- boral_Frog_upper - boral_Frog_Lower
colnames(Uncertainty_boral_Frog) <- rownames(Uncertainty_boral_Frog) <- 1:ncol(Uncertainty_boral_Frog)
Uncertainty_boral_Frog <- as.matrix(Uncertainty_boral_Frog)

### Fungi ----

boral_Fungi_Mean <- read.csv("Rho_mean_Fungi_boral.csv")
boral_Fungi_Mean <- boral_Fungi_Mean[,-1]
boral_Fungi_Mean <- as.matrix(boral_Fungi_Mean)
colnames(boral_Fungi_Mean) <- rownames(boral_Fungi_Mean) <- 1:ncol(boral_Fungi_Mean)

boral_Fungi_Lower <- read.csv("Rho_lower_Fungi_boral.csv")
boral_Fungi_Lower <- boral_Fungi_Lower[,-1]
boral_Fungi_Lower <- as.matrix(boral_Fungi_Lower)
colnames(boral_Fungi_Lower) <- rownames(boral_Fungi_Lower) <- 1:ncol(boral_Fungi_Lower)

boral_Fungi_upper <- read.csv("Rho_upper_Fungi_boral.csv")
boral_Fungi_upper <- boral_Fungi_upper[,-1]
boral_Fungi_upper <- as.matrix(boral_Fungi_upper)
colnames(boral_Fungi_upper) <- rownames(boral_Fungi_upper) <- 1:ncol(boral_Fungi_upper)

Uncertainty_boral_Fungi <- boral_Fungi_upper - boral_Fungi_Lower
colnames(Uncertainty_boral_Fungi) <- rownames(Uncertainty_boral_Fungi) <- 1:ncol(Uncertainty_boral_Fungi)
Uncertainty_boral_Fungi <- as.matrix(Uncertainty_boral_Fungi)

### Mosquitos ----

boral_Mosquito_Mean <- read.csv("Rho_mean_Mosquito_boral.csv")
boral_Mosquito_Mean <- boral_Mosquito_Mean[,-1]
boral_Mosquito_Mean <- as.matrix(boral_Mosquito_Mean)
colnames(boral_Mosquito_Mean) <- rownames(boral_Mosquito_Mean) <- 1:ncol(boral_Mosquito_Mean)

boral_Mosquito_Lower <- read.csv("Rho_lower_Mosquito_boral.csv")
boral_Mosquito_Lower <- boral_Mosquito_Lower[,-1]
boral_Mosquito_Lower <- as.matrix(boral_Mosquito_Lower)
colnames(boral_Mosquito_Lower) <- rownames(boral_Mosquito_Lower) <- 1:ncol(boral_Mosquito_Lower)

boral_Mosquito_upper <- read.csv("Rho_upper_Mosquito_boral.csv")
boral_Mosquito_upper <- boral_Mosquito_upper[,-1]
boral_Mosquito_upper <- as.matrix(boral_Mosquito_upper)
colnames(boral_Mosquito_upper) <- rownames(boral_Mosquito_upper) <- 1:ncol(boral_Mosquito_upper)

Uncertainty_boral_Mosquito <- boral_Mosquito_upper - boral_Mosquito_Lower
colnames(Uncertainty_boral_Mosquito) <- rownames(Uncertainty_boral_Mosquito) <- 1:ncol(Uncertainty_boral_Mosquito)
Uncertainty_boral_Mosquito <- as.matrix(Uncertainty_boral_Mosquito)

# ----

## Clark

### Birds ----

Clark_Bird_Mean <- read.csv("Rho_mean_Bird_Clark.csv")
Clark_Bird_Mean <- Clark_Bird_Mean[,-1]
Clark_Bird_Mean <- as.matrix(Clark_Bird_Mean)
colnames(Clark_Bird_Mean) <- rownames(Clark_Bird_Mean) <- 1:ncol(Clark_Bird_Mean)

Clark_Bird_Lower <- read.csv("Rho_lower_Bird_Clark.csv")
Clark_Bird_Lower <- Clark_Bird_Lower[,-1]
Clark_Bird_Lower <- as.matrix(Clark_Bird_Lower)
colnames(Clark_Bird_Lower) <- rownames(Clark_Bird_Lower) <- 1:ncol(Clark_Bird_Lower)

Clark_Bird_upper <- read.csv("Rho_upper_Bird_Clark.csv")
Clark_Bird_upper <- Clark_Bird_upper[,-1]
Clark_Bird_upper <- as.matrix(Clark_Bird_upper)
colnames(Clark_Bird_upper) <- rownames(Clark_Bird_upper) <- 1:ncol(Clark_Bird_upper)

Uncertainty_Clark_Bird <- Clark_Bird_upper - Clark_Bird_Lower
colnames(Uncertainty_Clark_Bird) <- rownames(Uncertainty_Clark_Bird) <- 1:ncol(Uncertainty_Clark_Bird)
Uncertainty_Clark_Bird <- as.matrix(Uncertainty_Clark_Bird)

### Butterflies ----

Clark_Butterfly_Mean <- read.csv("Rho_mean_Butterflies_Clark.csv")
Clark_Butterfly_Mean <- Clark_Butterfly_Mean[,-1]
Clark_Butterfly_Mean <- as.matrix(Clark_Butterfly_Mean)
colnames(Clark_Butterfly_Mean) <- rownames(Clark_Butterfly_Mean) <- 1:ncol(Clark_Butterfly_Mean)

Clark_Butterfly_Lower <- read.csv("Rho_lower_Butterflies_Clark.csv")
Clark_Butterfly_Lower <- Clark_Butterfly_Lower[,-1]
Clark_Butterfly_Lower <- as.matrix(Clark_Butterfly_Lower)
colnames(Clark_Butterfly_Lower) <- rownames(Clark_Butterfly_Lower) <- 1:ncol(Clark_Butterfly_Lower)

Clark_Butterfly_upper <- read.csv("Rho_upper_Butterflies_Clark.csv")
Clark_Butterfly_upper <- Clark_Butterfly_upper[,-1]
Clark_Butterfly_upper <- as.matrix(Clark_Butterfly_upper)
colnames(Clark_Butterfly_upper) <- rownames(Clark_Butterfly_upper) <- 1:ncol(Clark_Butterfly_upper)

Uncertainty_Clark_Butterfly <- Clark_Butterfly_upper - Clark_Butterfly_Lower
colnames(Uncertainty_Clark_Butterfly) <- rownames(Uncertainty_Clark_Butterfly) <- 1:ncol(Uncertainty_Clark_Butterfly)
Uncertainty_Clark_Butterfly <- as.matrix(Uncertainty_Clark_Butterfly)

### Eucalypts ----

Clark_Eucalypts_Mean <- read.csv("Rho_mean_Eucalypts_Clark.csv")
Clark_Eucalypts_Mean <- Clark_Eucalypts_Mean[,-1]
Clark_Eucalypts_Mean <- as.matrix(Clark_Eucalypts_Mean)
colnames(Clark_Eucalypts_Mean) <- rownames(Clark_Eucalypts_Mean) <- 1:ncol(Clark_Eucalypts_Mean)

Clark_Eucalypts_Lower <- read.csv("Rho_lower_Eucalypts_Clark.csv")
Clark_Eucalypts_Lower <- Clark_Eucalypts_Lower[,-1]
Clark_Eucalypts_Lower <- as.matrix(Clark_Eucalypts_Lower)
colnames(Clark_Eucalypts_Lower) <- rownames(Clark_Eucalypts_Lower) <- 1:ncol(Clark_Eucalypts_Lower)

Clark_Eucalypts_upper <- read.csv("Rho_upper_Eucalypts_Clark.csv")
Clark_Eucalypts_upper <- Clark_Eucalypts_upper[,-1]
Clark_Eucalypts_upper <- as.matrix(Clark_Eucalypts_upper)
colnames(Clark_Eucalypts_upper) <- rownames(Clark_Eucalypts_upper) <- 1:ncol(Clark_Eucalypts_upper)

Uncertainty_Clark_Eucalypts <- Clark_Eucalypts_upper - Clark_Eucalypts_Lower
colnames(Uncertainty_Clark_Eucalypts) <- rownames(Uncertainty_Clark_Eucalypts) <- 1:ncol(Uncertainty_Clark_Eucalypts)
Uncertainty_Clark_Eucalypts <- as.matrix(Uncertainty_Clark_Eucalypts)

### Frogs ----

Clark_Frog_Mean <- read.csv("Rho_mean_Frog_Clark.csv")
Clark_Frog_Mean <- Clark_Frog_Mean[,-1]
Clark_Frog_Mean <- as.matrix(Clark_Frog_Mean)
colnames(Clark_Frog_Mean) <- rownames(Clark_Frog_Mean) <- 1:ncol(Clark_Frog_Mean)

Clark_Frog_Lower <- read.csv("Rho_lower_Frog_Clark.csv")
Clark_Frog_Lower <- Clark_Frog_Lower[,-1]
Clark_Frog_Lower <- as.matrix(Clark_Frog_Lower)
colnames(Clark_Frog_Lower) <- rownames(Clark_Frog_Lower) <- 1:ncol(Clark_Frog_Lower)

Clark_Frog_upper <- read.csv("Rho_upper_Frog_Clark.csv")
Clark_Frog_upper <- Clark_Frog_upper[,-1]
Clark_Frog_upper <- as.matrix(Clark_Frog_upper)
colnames(Clark_Frog_upper) <- rownames(Clark_Frog_upper) <- 1:ncol(Clark_Frog_upper)

Uncertainty_Clark_Frog <- Clark_Frog_upper - Clark_Frog_Lower
colnames(Uncertainty_Clark_Frog) <- rownames(Uncertainty_Clark_Frog) <- 1:ncol(Uncertainty_Clark_Frog)
Uncertainty_Clark_Frog <- as.matrix(Uncertainty_Clark_Frog)

### Fungi ----

Clark_Fungi_Mean <- read.csv("Rho_mean_Fungi_Clark.csv")
Clark_Fungi_Mean <- Clark_Fungi_Mean[,-1]
Clark_Fungi_Mean <- as.matrix(Clark_Fungi_Mean)
colnames(Clark_Fungi_Mean) <- rownames(Clark_Fungi_Mean) <- 1:ncol(Clark_Fungi_Mean)

Clark_Fungi_Lower <- read.csv("Rho_lower_Fungi_Clark.csv")
Clark_Fungi_Lower <- Clark_Fungi_Lower[,-1]
Clark_Fungi_Lower <- as.matrix(Clark_Fungi_Lower)
colnames(Clark_Fungi_Lower) <- rownames(Clark_Fungi_Lower) <- 1:ncol(Clark_Fungi_Lower)

Clark_Fungi_upper <- read.csv("Rho_upper_Fungi_Clark.csv")
Clark_Fungi_upper <- Clark_Fungi_upper[,-1]
Clark_Fungi_upper <- as.matrix(Clark_Fungi_upper)
colnames(Clark_Fungi_upper) <- rownames(Clark_Fungi_upper) <- 1:ncol(Clark_Fungi_upper)

Uncertainty_Clark_Fungi <- Clark_Fungi_upper - Clark_Fungi_Lower
colnames(Uncertainty_Clark_Fungi) <- rownames(Uncertainty_Clark_Fungi) <- 1:ncol(Uncertainty_Clark_Fungi)
Uncertainty_Clark_Fungi <- as.matrix(Uncertainty_Clark_Fungi)

### Mosquitos ----

Clark_Mosquito_Mean <- read.csv("Rho_mean_Mosquito_Clark.csv")
Clark_Mosquito_Mean <- Clark_Mosquito_Mean[,-1]
Clark_Mosquito_Mean <- as.matrix(Clark_Mosquito_Mean)
colnames(Clark_Mosquito_Mean) <- rownames(Clark_Mosquito_Mean) <- 1:ncol(Clark_Mosquito_Mean)

Clark_Mosquito_Lower <- read.csv("Rho_lower_Mosquito_Clark.csv")
Clark_Mosquito_Lower <- Clark_Mosquito_Lower[,-1]
Clark_Mosquito_Lower <- as.matrix(Clark_Mosquito_Lower)
colnames(Clark_Mosquito_Lower) <- rownames(Clark_Mosquito_Lower) <- 1:ncol(Clark_Mosquito_Lower)

Clark_Mosquito_upper <- read.csv("Rho_upper_Mosquito_Clark.csv")
Clark_Mosquito_upper <- Clark_Mosquito_upper[,-1]
Clark_Mosquito_upper <- as.matrix(Clark_Mosquito_upper)
colnames(Clark_Mosquito_upper) <- rownames(Clark_Mosquito_upper) <- 1:ncol(Clark_Mosquito_upper)

Uncertainty_Clark_Mosquito <- Clark_Mosquito_upper - Clark_Mosquito_Lower
colnames(Uncertainty_Clark_Mosquito) <- rownames(Uncertainty_Clark_Mosquito) <- 1:ncol(Uncertainty_Clark_Mosquito)
Uncertainty_Clark_Mosquito <- as.matrix(Uncertainty_Clark_Mosquito)

# ----

## Ovaskainen

### Birds ----

Ovaskainen_Bird_Mean <- matrix(rep(NA, length(BayesComm_Bird_Mean)),
                               ncol = ncol(BayesComm_Bird_Mean),
                               nrow = nrow(BayesComm_Bird_Mean))

Uncertainty_Ovaskainen_Bird <- Ovaskainen_Bird_Mean

### Butterflies ----

Ovaskainen_Butterfly_Mean <- matrix(rep(NA, length(BayesComm_Butterfly_Mean)),
                               ncol = ncol(BayesComm_Butterfly_Mean),
                               nrow = nrow(BayesComm_Butterfly_Mean))

Uncertainty_Ovaskainen_Butterfly <- Ovaskainen_Butterfly_Mean

### Eucalypts ----

Ovaskainen_Eucalypts_Mean <- openxlsx::read.xlsx("R1_Eucalypt_Ovaskainen.xlsx",sheet = 1, colNames = T)
Ovaskainen_Eucalypts_Mean <- Ovaskainen_Eucalypts_Mean[,-1]
Ovaskainen_Eucalypts_Mean <- as.matrix(Ovaskainen_Eucalypts_Mean)
colnames(Ovaskainen_Eucalypts_Mean) <- rownames(Ovaskainen_Eucalypts_Mean) <- 1:ncol(Ovaskainen_Eucalypts_Mean)

Ovaskainen_Eucalypts_Lower <- openxlsx::read.xlsx("R1_Eucalypt_Ovaskainen.xlsx",sheet = 5, colNames = T)
Ovaskainen_Eucalypts_Lower <- Ovaskainen_Eucalypts_Lower[,-1]
Ovaskainen_Eucalypts_Lower <- as.matrix(Ovaskainen_Eucalypts_Lower)
colnames(Ovaskainen_Eucalypts_Lower) <- rownames(Ovaskainen_Eucalypts_Lower) <- 1:ncol(Ovaskainen_Eucalypts_Lower)

Ovaskainen_Eucalypts_Upper <- openxlsx::read.xlsx("R1_Eucalypt_Ovaskainen.xlsx",sheet = 6, colNames = T)
Ovaskainen_Eucalypts_Upper <- Ovaskainen_Eucalypts_Upper[,-1]
Ovaskainen_Eucalypts_Upper <- as.matrix(Ovaskainen_Eucalypts_Upper)
colnames(Ovaskainen_Eucalypts_Upper) <- rownames(Ovaskainen_Eucalypts_Upper) <- 1:ncol(Ovaskainen_Eucalypts_Upper)

Uncertainty_Ovaskainen_Eucalypts <- Ovaskainen_Eucalypts_Upper - Ovaskainen_Eucalypts_Lower
colnames(Uncertainty_Ovaskainen_Eucalypts) <- rownames(Uncertainty_Ovaskainen_Eucalypts) <- 1:ncol(Uncertainty_Ovaskainen_Eucalypts)
Uncertainty_Ovaskainen_Eucalypts <- as.matrix(Uncertainty_Ovaskainen_Eucalypts)

### Frogs ----

Ovaskainen_Frog_Mean <- openxlsx::read.xlsx("R1_Frog_Ovaskainen.xlsx",sheet = 1, colNames = T)
Ovaskainen_Frog_Mean <- Ovaskainen_Frog_Mean[,-1]
Ovaskainen_Frog_Mean <- as.matrix(Ovaskainen_Frog_Mean)
colnames(Ovaskainen_Frog_Mean) <- rownames(Ovaskainen_Frog_Mean) <- 1:ncol(Ovaskainen_Frog_Mean)

Ovaskainen_Frog_Lower <- openxlsx::read.xlsx("R1_Frog_Ovaskainen.xlsx",sheet = 5, colNames = T)
Ovaskainen_Frog_Lower <- Ovaskainen_Frog_Lower[,-1]
Ovaskainen_Frog_Lower <- as.matrix(Ovaskainen_Frog_Lower)
colnames(Ovaskainen_Frog_Lower) <- rownames(Ovaskainen_Frog_Lower) <- 1:ncol(Ovaskainen_Frog_Lower)

Ovaskainen_Frog_Upper <- openxlsx::read.xlsx("R1_Frog_Ovaskainen.xlsx",sheet = 6, colNames = T)
Ovaskainen_Frog_Upper <- Ovaskainen_Frog_Upper[,-1]
Ovaskainen_Frog_Upper <- as.matrix(Ovaskainen_Frog_Upper)
colnames(Ovaskainen_Frog_Upper) <- rownames(Ovaskainen_Frog_Upper) <- 1:ncol(Ovaskainen_Frog_Upper)

Uncertainty_Ovaskainen_Frog <- Ovaskainen_Frog_Upper - Ovaskainen_Frog_Lower
colnames(Uncertainty_Ovaskainen_Frog) <- rownames(Uncertainty_Ovaskainen_Frog) <- 1:ncol(Uncertainty_Ovaskainen_Frog)
Uncertainty_Ovaskainen_Frog <- as.matrix(Uncertainty_Ovaskainen_Frog)

### Fungi ----

Ovaskainen_Fungi_Mean <- matrix(rep(NA, length(BayesComm_Fungi_Mean)),
                                ncol = ncol(BayesComm_Fungi_Mean),
                                nrow = nrow(BayesComm_Fungi_Mean))

Uncertainty_Ovaskainen_Fungi <- Ovaskainen_Fungi_Mean

### Mosquitos ----

Ovaskainen_Mosquito_Mean <- openxlsx::read.xlsx("R1_Mosquito_Ovaskainen.xlsx",sheet = 1, colNames = T)
Ovaskainen_Mosquito_Mean <- Ovaskainen_Mosquito_Mean[,-1]
Ovaskainen_Mosquito_Mean <- as.matrix(Ovaskainen_Mosquito_Mean)
colnames(Ovaskainen_Mosquito_Mean) <- rownames(Ovaskainen_Mosquito_Mean) <- 1:ncol(Ovaskainen_Mosquito_Mean)

Ovaskainen_Mosquito_Lower <- openxlsx::read.xlsx("R1_Mosquito_Ovaskainen.xlsx",sheet = 5, colNames = T)
Ovaskainen_Mosquito_Lower <- Ovaskainen_Mosquito_Lower[,-1]
Ovaskainen_Mosquito_Lower <- as.matrix(Ovaskainen_Mosquito_Lower)
colnames(Ovaskainen_Mosquito_Lower) <- rownames(Ovaskainen_Mosquito_Lower) <- 1:ncol(Ovaskainen_Mosquito_Lower)

Ovaskainen_Mosquito_Upper <- openxlsx::read.xlsx("R1_Mosquito_Ovaskainen.xlsx",sheet = 6, colNames = T)
Ovaskainen_Mosquito_Upper <- Ovaskainen_Mosquito_Upper[,-1]
Ovaskainen_Mosquito_Upper <- as.matrix(Ovaskainen_Mosquito_Upper)
colnames(Ovaskainen_Mosquito_Upper) <- rownames(Ovaskainen_Mosquito_Upper) <- 1:ncol(Ovaskainen_Mosquito_Upper)

Uncertainty_Ovaskainen_Mosquito <- Ovaskainen_Mosquito_Upper - Ovaskainen_Mosquito_Lower
colnames(Uncertainty_Ovaskainen_Mosquito) <- rownames(Uncertainty_Ovaskainen_Mosquito) <- 1:ncol(Uncertainty_Ovaskainen_Mosquito)
Uncertainty_Ovaskainen_Mosquito <- as.matrix(Uncertainty_Ovaskainen_Mosquito)

# ----

## Ovaskainen NS

### Birds ----

OvaskainenNS_Bird_Mean <- openxlsx::read.xlsx("R1_Bird_OvaskainenNS.xlsx", sheet = 1, colNames = T)
OvaskainenNS_Bird_Mean <- OvaskainenNS_Bird_Mean[,-1]
OvaskainenNS_Bird_Mean <- as.matrix(OvaskainenNS_Bird_Mean)
colnames(OvaskainenNS_Bird_Mean) <- rownames(OvaskainenNS_Bird_Mean) <- 1:ncol(OvaskainenNS_Bird_Mean)

OvaskainenNS_Bird_Lower <- openxlsx::read.xlsx("R1_Bird_OvaskainenNS.xlsx", sheet = 5, colNames = T)
OvaskainenNS_Bird_Lower <- OvaskainenNS_Bird_Lower[,-1]
OvaskainenNS_Bird_Lower <- as.matrix(OvaskainenNS_Bird_Lower)
colnames(OvaskainenNS_Bird_Lower) <- rownames(OvaskainenNS_Bird_Lower) <- 1:ncol(OvaskainenNS_Bird_Lower)

OvaskainenNS_Bird_Upper <- openxlsx::read.xlsx("R1_Bird_OvaskainenNS.xlsx", sheet = 6, colNames = T)
OvaskainenNS_Bird_Upper <- OvaskainenNS_Bird_Upper[,-1]
OvaskainenNS_Bird_Upper <- as.matrix(OvaskainenNS_Bird_Upper)
colnames(OvaskainenNS_Bird_Upper) <- rownames(OvaskainenNS_Bird_Upper) <- 1:ncol(OvaskainenNS_Bird_Upper)

Uncertainty_OvaskainenNS_Bird <- OvaskainenNS_Bird_Upper - OvaskainenNS_Bird_Lower
colnames(Uncertainty_OvaskainenNS_Bird) <- rownames(Uncertainty_OvaskainenNS_Bird) <- 1:ncol(Uncertainty_OvaskainenNS_Bird)
Uncertainty_OvaskainenNS_Bird <- as.matrix(Uncertainty_OvaskainenNS_Bird)

### Butterflies ----

OvaskainenNS_Butterfly_Mean <- openxlsx::read.xlsx("R1_Butterfly_OvaskainenNS.xlsx",sheet = 1, colNames = T)
OvaskainenNS_Butterfly_Mean <- OvaskainenNS_Butterfly_Mean[,-1]
OvaskainenNS_Butterfly_Mean <- as.matrix(OvaskainenNS_Butterfly_Mean)
colnames(OvaskainenNS_Butterfly_Mean) <- rownames(OvaskainenNS_Butterfly_Mean) <- 1:ncol(OvaskainenNS_Butterfly_Mean)

OvaskainenNS_Butterfly_Lower <- openxlsx::read.xlsx("R1_Butterfly_OvaskainenNS.xlsx",sheet = 5, colNames = T)
OvaskainenNS_Butterfly_Lower <- OvaskainenNS_Butterfly_Lower[,-1]
OvaskainenNS_Butterfly_Lower <- as.matrix(OvaskainenNS_Butterfly_Lower)
colnames(OvaskainenNS_Butterfly_Lower) <- rownames(OvaskainenNS_Butterfly_Lower) <- 1:ncol(OvaskainenNS_Butterfly_Lower)

OvaskainenNS_Butterfly_Upper <- openxlsx::read.xlsx("R1_Butterfly_OvaskainenNS.xlsx",sheet = 6, colNames = T)
OvaskainenNS_Butterfly_Upper <- OvaskainenNS_Butterfly_Upper[,-1]
OvaskainenNS_Butterfly_Upper <- as.matrix(OvaskainenNS_Butterfly_Upper)
colnames(OvaskainenNS_Butterfly_Upper) <- rownames(OvaskainenNS_Butterfly_Upper) <- 1:ncol(OvaskainenNS_Butterfly_Upper)

Uncertainty_OvaskainenNS_Butterfly <- OvaskainenNS_Butterfly_Upper - OvaskainenNS_Butterfly_Lower
colnames(Uncertainty_OvaskainenNS_Butterfly) <- rownames(Uncertainty_OvaskainenNS_Butterfly) <- 1:ncol(Uncertainty_OvaskainenNS_Butterfly)
Uncertainty_OvaskainenNS_Butterfly <- as.matrix(Uncertainty_OvaskainenNS_Butterfly)

### Eucalypts ----

OvaskainenNS_Eucalypts_Mean <- openxlsx::read.xlsx("R1_Eucalypt_OvaskainenNS.xlsx",sheet = 1, colNames = T)
OvaskainenNS_Eucalypts_Mean <- OvaskainenNS_Eucalypts_Mean[,-1]
OvaskainenNS_Eucalypts_Mean <- as.matrix(OvaskainenNS_Eucalypts_Mean)
colnames(OvaskainenNS_Eucalypts_Mean) <- rownames(OvaskainenNS_Eucalypts_Mean) <- 1:ncol(OvaskainenNS_Eucalypts_Mean)

OvaskainenNS_Eucalypts_Lower <- openxlsx::read.xlsx("R1_Eucalypt_OvaskainenNS.xlsx",sheet = 5, colNames = T)
OvaskainenNS_Eucalypts_Lower <- OvaskainenNS_Eucalypts_Lower[,-1]
OvaskainenNS_Eucalypts_Lower <- as.matrix(OvaskainenNS_Eucalypts_Lower)
colnames(OvaskainenNS_Eucalypts_Lower) <- rownames(OvaskainenNS_Eucalypts_Lower) <- 1:ncol(OvaskainenNS_Eucalypts_Lower)

OvaskainenNS_Eucalypts_Upper <- openxlsx::read.xlsx("R1_Eucalypt_OvaskainenNS.xlsx",sheet = 6, colNames = T)
OvaskainenNS_Eucalypts_Upper <- OvaskainenNS_Eucalypts_Upper[,-1]
OvaskainenNS_Eucalypts_Upper <- as.matrix(OvaskainenNS_Eucalypts_Upper)
colnames(OvaskainenNS_Eucalypts_Upper) <- rownames(OvaskainenNS_Eucalypts_Upper) <- 1:ncol(OvaskainenNS_Eucalypts_Upper)

Uncertainty_OvaskainenNS_Eucalypts <- OvaskainenNS_Eucalypts_Upper - OvaskainenNS_Eucalypts_Lower
colnames(Uncertainty_OvaskainenNS_Eucalypts) <- rownames(Uncertainty_OvaskainenNS_Eucalypts) <- 1:ncol(Uncertainty_OvaskainenNS_Eucalypts)
Uncertainty_OvaskainenNS_Eucalypts <- as.matrix(Uncertainty_OvaskainenNS_Eucalypts)

### Frogs ----

OvaskainenNS_Frog_Mean <- openxlsx::read.xlsx("R1_Frog_OvaskainenNS.xlsx",sheet = 1, colNames = T)
OvaskainenNS_Frog_Mean <- OvaskainenNS_Frog_Mean[,-1]
OvaskainenNS_Frog_Mean <- as.matrix(OvaskainenNS_Frog_Mean)
colnames(OvaskainenNS_Frog_Mean) <- rownames(OvaskainenNS_Frog_Mean) <- 1:ncol(OvaskainenNS_Frog_Mean)

OvaskainenNS_Frog_Lower <- openxlsx::read.xlsx("R1_Frog_OvaskainenNS.xlsx",sheet = 5, colNames = T)
OvaskainenNS_Frog_Lower <- OvaskainenNS_Frog_Lower[,-1]
OvaskainenNS_Frog_Lower <- as.matrix(OvaskainenNS_Frog_Lower)
colnames(OvaskainenNS_Frog_Lower) <- rownames(OvaskainenNS_Frog_Lower) <- 1:ncol(OvaskainenNS_Frog_Lower)

OvaskainenNS_Frog_Upper <- openxlsx::read.xlsx("R1_Frog_OvaskainenNS.xlsx",sheet = 6, colNames = T)
OvaskainenNS_Frog_Upper <- OvaskainenNS_Frog_Upper[,-1]
OvaskainenNS_Frog_Upper <- as.matrix(OvaskainenNS_Frog_Upper)
colnames(OvaskainenNS_Frog_Upper) <- rownames(OvaskainenNS_Frog_Upper) <- 1:ncol(OvaskainenNS_Frog_Upper)

Uncertainty_OvaskainenNS_Frog <- OvaskainenNS_Frog_Upper - OvaskainenNS_Frog_Lower
colnames(Uncertainty_OvaskainenNS_Frog) <- rownames(Uncertainty_OvaskainenNS_Frog) <- 1:ncol(Uncertainty_OvaskainenNS_Frog)
Uncertainty_OvaskainenNS_Frog <- as.matrix(Uncertainty_OvaskainenNS_Frog)

### Fungi ----

OvaskainenNS_Fungi_Mean <- openxlsx::read.xlsx("R1_Fungi_OvaskainenNS.xlsx",sheet = 1, colNames = T)
OvaskainenNS_Fungi_Mean <- OvaskainenNS_Fungi_Mean[,-1]
OvaskainenNS_Fungi_Mean <- as.matrix(OvaskainenNS_Fungi_Mean)
colnames(OvaskainenNS_Fungi_Mean) <- rownames(OvaskainenNS_Fungi_Mean) <- 1:ncol(OvaskainenNS_Fungi_Mean)

OvaskainenNS_Fungi_Lower <- openxlsx::read.xlsx("R1_Fungi_OvaskainenNS.xlsx",sheet = 5, colNames = T)
OvaskainenNS_Fungi_Lower <- OvaskainenNS_Fungi_Lower[,-1]
OvaskainenNS_Fungi_Lower <- as.matrix(OvaskainenNS_Fungi_Lower)
colnames(OvaskainenNS_Fungi_Lower) <- rownames(OvaskainenNS_Fungi_Lower) <- 1:ncol(OvaskainenNS_Fungi_Lower)

OvaskainenNS_Fungi_Upper <- openxlsx::read.xlsx("R1_Fungi_OvaskainenNS.xlsx",sheet = 6, colNames = T)
OvaskainenNS_Fungi_Upper <- OvaskainenNS_Fungi_Upper[,-1]
OvaskainenNS_Fungi_Upper <- as.matrix(OvaskainenNS_Fungi_Upper)
colnames(OvaskainenNS_Fungi_Upper) <- rownames(OvaskainenNS_Fungi_Upper) <- 1:ncol(OvaskainenNS_Fungi_Upper)

Uncertainty_OvaskainenNS_Fungi <- OvaskainenNS_Fungi_Upper - OvaskainenNS_Fungi_Lower
colnames(Uncertainty_OvaskainenNS_Fungi) <- rownames(Uncertainty_OvaskainenNS_Fungi) <- 1:ncol(Uncertainty_OvaskainenNS_Fungi)
Uncertainty_OvaskainenNS_Fungi <- as.matrix(Uncertainty_OvaskainenNS_Fungi)

### Mosquitos ----

OvaskainenNS_Mosquito_Mean <- openxlsx::read.xlsx("R1_Mosquito_OvaskainenNS.xlsx",sheet = 1, colNames = T)
OvaskainenNS_Mosquito_Mean <- OvaskainenNS_Mosquito_Mean[,-1]
OvaskainenNS_Mosquito_Mean <- as.matrix(OvaskainenNS_Mosquito_Mean)
colnames(OvaskainenNS_Mosquito_Mean) <- rownames(OvaskainenNS_Mosquito_Mean) <- 1:ncol(OvaskainenNS_Mosquito_Mean)

OvaskainenNS_Mosquito_Lower <- openxlsx::read.xlsx("R1_Mosquito_OvaskainenNS.xlsx",sheet = 5, colNames = T)
OvaskainenNS_Mosquito_Lower <- OvaskainenNS_Mosquito_Lower[,-1]
OvaskainenNS_Mosquito_Lower <- as.matrix(OvaskainenNS_Mosquito_Lower)
colnames(OvaskainenNS_Mosquito_Lower) <- rownames(OvaskainenNS_Mosquito_Lower) <- 1:ncol(OvaskainenNS_Mosquito_Lower)

OvaskainenNS_Mosquito_Upper <- openxlsx::read.xlsx("R1_Mosquito_OvaskainenNS.xlsx",sheet = 6, colNames = T)
OvaskainenNS_Mosquito_Upper <- OvaskainenNS_Mosquito_Upper[,-1]
OvaskainenNS_Mosquito_Upper <- as.matrix(OvaskainenNS_Mosquito_Upper)
colnames(OvaskainenNS_Mosquito_Upper) <- rownames(OvaskainenNS_Mosquito_Upper) <- 1:ncol(OvaskainenNS_Mosquito_Upper)

Uncertainty_OvaskainenNS_Mosquito <- OvaskainenNS_Mosquito_Upper - OvaskainenNS_Mosquito_Lower
colnames(Uncertainty_OvaskainenNS_Mosquito) <- rownames(Uncertainty_OvaskainenNS_Mosquito) <- 1:ncol(Uncertainty_OvaskainenNS_Mosquito)
Uncertainty_OvaskainenNS_Mosquito <- as.matrix(Uncertainty_OvaskainenNS_Mosquito)

# ----

## Pollock

### Birds ----

Pollock_Bird_Mean <- matrix(rep(NA, length(BayesComm_Bird_Mean)),
                            ncol = ncol(BayesComm_Bird_Mean),
                            nrow = nrow(BayesComm_Bird_Mean))

Uncertainty_Pollock_Bird <- Pollock_Bird_Mean

### Butterflies ----

Pollock_Butterfly_Mean <- matrix(rep(NA, length(BayesComm_Butterfly_Mean)),
                                 ncol = ncol(BayesComm_Butterfly_Mean),
                                 nrow = nrow(BayesComm_Butterfly_Mean))

Uncertainty_Pollock_Butterfly <- Pollock_Butterfly_Mean

### Eucalypts ----

Pollock_Eucalypts_Mean <- read.csv("Rho_mean_Eucalypt_Pollock.csv")
Pollock_Eucalypts_Mean <- Pollock_Eucalypts_Mean[,-1]
Pollock_Eucalypts_Mean <- as.matrix(Pollock_Eucalypts_Mean)
colnames(Pollock_Eucalypts_Mean) <- rownames(Pollock_Eucalypts_Mean) <- 1:ncol(Pollock_Eucalypts_Mean)

Pollock_Eucalypts_Lower <- read.csv("Rho_lower_Eucalypt_Pollock.csv")
Pollock_Eucalypts_Lower <- Pollock_Eucalypts_Lower[,-1]
Pollock_Eucalypts_Lower <- as.matrix(Pollock_Eucalypts_Lower)
colnames(Pollock_Eucalypts_Lower) <- rownames(Pollock_Eucalypts_Lower) <- 1:ncol(Pollock_Eucalypts_Lower)

Pollock_Eucalypts_Upper <- read.csv("Rho_upper_Eucalypt_Pollock.csv")
Pollock_Eucalypts_Upper <- Pollock_Eucalypts_Upper[,-1]
Pollock_Eucalypts_Upper <- as.matrix(Pollock_Eucalypts_Upper)
colnames(Pollock_Eucalypts_Upper) <- rownames(Pollock_Eucalypts_Upper) <- 1:ncol(Pollock_Eucalypts_Upper)

Uncertainty_Pollock_Eucalypts <- Pollock_Eucalypts_Upper - Pollock_Eucalypts_Lower
colnames(Uncertainty_Pollock_Eucalypts) <- rownames(Uncertainty_Pollock_Eucalypts) <- 1:ncol(Uncertainty_Pollock_Eucalypts)
Uncertainty_Pollock_Eucalypts <- as.matrix(Uncertainty_Pollock_Eucalypts)

### Frogs ----

Pollock_Frog_Mean <- read.csv("Rho_mean_Frog_Pollock.csv")
Pollock_Frog_Mean <- Pollock_Frog_Mean[,-1]
Pollock_Frog_Mean <- as.matrix(Pollock_Frog_Mean)
colnames(Pollock_Frog_Mean) <- rownames(Pollock_Frog_Mean) <- 1:ncol(Pollock_Frog_Mean)

Pollock_Frog_Lower <- read.csv("Rho_lower_Frog_Pollock.csv")
Pollock_Frog_Lower <- Pollock_Frog_Lower[,-1]
Pollock_Frog_Lower <- as.matrix(Pollock_Frog_Lower)
colnames(Pollock_Frog_Lower) <- rownames(Pollock_Frog_Lower) <- 1:ncol(Pollock_Frog_Lower)

Pollock_Frog_Upper <- read.csv("Rho_upper_Frog_Pollock.csv")
Pollock_Frog_Upper <- Pollock_Frog_Upper[,-1]
Pollock_Frog_Upper <- as.matrix(Pollock_Frog_Upper)
colnames(Pollock_Frog_Upper) <- rownames(Pollock_Frog_Upper) <- 1:ncol(Pollock_Frog_Upper)

Uncertainty_Pollock_Frog <- Pollock_Frog_Upper - Pollock_Frog_Lower
colnames(Uncertainty_Pollock_Frog) <- rownames(Uncertainty_Pollock_Frog) <- 1:ncol(Uncertainty_Pollock_Frog)
Uncertainty_Pollock_Frog <- as.matrix(Uncertainty_Pollock_Frog)

### Fungi ----

Pollock_Fungi_Mean <- read.csv("Rho_mean_Fungi_Pollock.csv")
Pollock_Fungi_Mean <- Pollock_Fungi_Mean[,-1]
Pollock_Fungi_Mean <- as.matrix(Pollock_Fungi_Mean)
colnames(Pollock_Fungi_Mean) <- rownames(Pollock_Fungi_Mean) <- 1:ncol(Pollock_Fungi_Mean)

Pollock_Fungi_Lower <- read.csv("Rho_lower_Fungi_Pollock.csv")
Pollock_Fungi_Lower <- Pollock_Fungi_Lower[,-1]
Pollock_Fungi_Lower <- as.matrix(Pollock_Fungi_Lower)
colnames(Pollock_Fungi_Lower) <- rownames(Pollock_Fungi_Lower) <- 1:ncol(Pollock_Fungi_Lower)

Pollock_Fungi_Upper <- read.csv("Rho_upper_Fungi_Pollock.csv")
Pollock_Fungi_Upper <- Pollock_Fungi_Upper[,-1]
Pollock_Fungi_Upper <- as.matrix(Pollock_Fungi_Upper)
colnames(Pollock_Fungi_Upper) <- rownames(Pollock_Fungi_Upper) <- 1:ncol(Pollock_Fungi_Upper)

Uncertainty_Pollock_Fungi <- Pollock_Fungi_Upper - Pollock_Fungi_Lower
colnames(Uncertainty_Pollock_Fungi) <- rownames(Uncertainty_Pollock_Fungi) <- 1:ncol(Uncertainty_Pollock_Fungi)
Uncertainty_Pollock_Fungi <- as.matrix(Uncertainty_Pollock_Fungi)

### Mosquitos ----

Pollock_Mosquito_Mean <- read.csv("Rho_mean_Mosquito_Pollock.csv")
Pollock_Mosquito_Mean <- Pollock_Mosquito_Mean[,-1]
Pollock_Mosquito_Mean <- as.matrix(Pollock_Mosquito_Mean)
colnames(Pollock_Mosquito_Mean) <- rownames(Pollock_Mosquito_Mean) <- 1:ncol(Pollock_Mosquito_Mean)

Pollock_Mosquito_Lower <- read.csv("Rho_lower_Mosquito_Pollock.csv")
Pollock_Mosquito_Lower <- Pollock_Mosquito_Lower[,-1]
Pollock_Mosquito_Lower <- as.matrix(Pollock_Mosquito_Lower)
colnames(Pollock_Mosquito_Lower) <- rownames(Pollock_Mosquito_Lower) <- 1:ncol(Pollock_Mosquito_Lower)

Pollock_Mosquito_Upper <- read.csv("Rho_upper_Mosquito_Pollock.csv")
Pollock_Mosquito_Upper <- Pollock_Mosquito_Upper[,-1]
Pollock_Mosquito_Upper <- as.matrix(Pollock_Mosquito_Upper)
colnames(Pollock_Mosquito_Upper) <- rownames(Pollock_Mosquito_Upper) <- 1:ncol(Pollock_Mosquito_Upper)

Uncertainty_Pollock_Mosquito <- Pollock_Mosquito_Upper - Pollock_Mosquito_Lower
colnames(Uncertainty_Pollock_Mosquito) <- rownames(Uncertainty_Pollock_Mosquito) <- 1:ncol(Uncertainty_Pollock_Mosquito)
Uncertainty_Pollock_Mosquito <- as.matrix(Uncertainty_Pollock_Mosquito)

# ----

####################
### Set Up Lists ###
####################

## Model Names ----

model_names <- c("MPR", "HPR", "LPR", "DPR", "HLR-S", "HLR-NS")

## Rho Lists ----

Bird_Rho_List <- list(BayesComm_Bird_Mean, Pollock_Bird_Mean, boral_Bird_Mean,
                      Clark_Bird_Mean, Ovaskainen_Bird_Mean, OvaskainenNS_Bird_Mean)

Butterfly_Rho_List <- list(BayesComm_Butterfly_Mean, Pollock_Butterfly_Mean,
                           boral_Butterfly_Mean, Clark_Butterfly_Mean,
                           Ovaskainen_Butterfly_Mean, OvaskainenNS_Butterfly_Mean)

Eucalypt_Rho_List <- list(BayesComm_Eucalypts_Mean, Pollock_Eucalypts_Mean,
                          boral_Eucalypts_Mean, Clark_Eucalypts_Mean,
                          Ovaskainen_Eucalypts_Mean, OvaskainenNS_Eucalypts_Mean)

Frog_Rho_List <- list(BayesComm_Frog_Mean, Pollock_Frog_Mean, boral_Frog_Mean,
                      Clark_Frog_Mean, Ovaskainen_Frog_Mean, OvaskainenNS_Frog_Mean)

Fungi_Rho_List <- list(BayesComm_Fungi_Mean, Pollock_Fungi_Mean,
                       boral_Fungi_Mean, Clark_Fungi_Mean,
                       Ovaskainen_Fungi_Mean, OvaskainenNS_Fungi_Mean)

Mosquito_Rho_List <- list(BayesComm_Mosquito_Mean, Pollock_Mosquito_Mean,
                          boral_Mosquito_Mean, Clark_Mosquito_Mean,
                          Ovaskainen_Mosquito_Mean, OvaskainenNS_Mosquito_Mean)

## Uncertainty Lists ----

Bird_Uncertainty_List <- list(Uncertainty_BayesComm_Bird, Uncertainty_Pollock_Bird,
                              Uncertainty_boral_Bird, Uncertainty_Clark_Bird,
                              Uncertainty_Ovaskainen_Bird, Uncertainty_OvaskainenNS_Bird)

Butterfly_Uncertainty_List <- list(Uncertainty_BayesComm_Butterfly, Uncertainty_Pollock_Butterfly,
                                   Uncertainty_boral_Butterfly, Uncertainty_Clark_Butterfly,
                                   Uncertainty_Ovaskainen_Butterfly, Uncertainty_OvaskainenNS_Butterfly)

Eucalypt_Uncertainty_List <- list(Uncertainty_BayesComm_Eucalypts, Uncertainty_Pollock_Eucalypts,
                                  Uncertainty_boral_Eucalypts, Uncertainty_Clark_Eucalypts,
                                  Uncertainty_Ovaskainen_Eucalypts, Uncertainty_OvaskainenNS_Eucalypts)

Frog_Uncertainty_List <- list(Uncertainty_BayesComm_Frog, Uncertainty_Pollock_Frog,
                              Uncertainty_boral_Frog, Uncertainty_Clark_Frog,
                              Uncertainty_Ovaskainen_Frog, Uncertainty_OvaskainenNS_Frog)

Fungi_Uncertainty_List <- list(Uncertainty_BayesComm_Fungi, Uncertainty_Pollock_Fungi,
                               Uncertainty_boral_Fungi, Uncertainty_Clark_Fungi,
                               Uncertainty_Ovaskainen_Fungi, Uncertainty_OvaskainenNS_Fungi)

Mosquito_Uncertainty_List <- list(Uncertainty_BayesComm_Mosquito, Uncertainty_Pollock_Mosquito,
                                  Uncertainty_boral_Mosquito, Uncertainty_Clark_Mosquito,
                                  Uncertainty_Ovaskainen_Mosquito, Uncertainty_OvaskainenNS_Mosquito)

# ----

########################
### Rho Correlations ###
########################

## Birds ----

Bird_Rho_Corr_Vector <- c()

for(i in 1:(length(Bird_Rho_List)-1)){
  for(j in (i+1):length(Bird_Rho_List)){
    tmp <- cor(Bird_Rho_List[[i]][upper.tri(Bird_Rho_List[[i]], diag = FALSE)],
               Bird_Rho_List[[j]][upper.tri(Bird_Rho_List[[j]], diag = FALSE)])
    Bird_Rho_Corr_Vector <- c(Bird_Rho_Corr_Vector, tmp)
  }
}

corr_Bird_matrix <- diag(length(Bird_Rho_List))
corr_Bird_matrix[lower.tri(corr_Bird_matrix, diag = FALSE)] <- Bird_Rho_Corr_Vector
corr_Bird_matrix <- t(corr_Bird_matrix)
colnames(corr_Bird_matrix) <- rownames(corr_Bird_matrix) <- model_names

## Butterfly ----

Butterfly_Rho_Corr_Vector <- c()

for(i in 1:(length(Butterfly_Rho_List)-1)){
  for(j in (i+1):length(Butterfly_Rho_List)){
    tmp <- cor(Butterfly_Rho_List[[i]][upper.tri(Butterfly_Rho_List[[i]], diag = FALSE)],
               Butterfly_Rho_List[[j]][upper.tri(Butterfly_Rho_List[[j]], diag = FALSE)])
    Butterfly_Rho_Corr_Vector <- c(Butterfly_Rho_Corr_Vector, tmp)
  }
}

corr_Butterfly_matrix <- diag(length(Butterfly_Rho_List))
corr_Butterfly_matrix[lower.tri(corr_Butterfly_matrix, diag = FALSE)] <- Butterfly_Rho_Corr_Vector
corr_Butterfly_matrix <- t(corr_Butterfly_matrix)
colnames(corr_Butterfly_matrix) <- rownames(corr_Butterfly_matrix) <- model_names

## Eucalypts ----

Eucalypt_Rho_Corr_Vector <- c()

for(i in 1:(length(Eucalypt_Rho_List)-1)){
  for(j in (i+1):length(Eucalypt_Rho_List)){
    tmp <- cor(Eucalypt_Rho_List[[i]][upper.tri(Eucalypt_Rho_List[[i]], diag = FALSE)],
               Eucalypt_Rho_List[[j]][upper.tri(Eucalypt_Rho_List[[j]], diag = FALSE)])
    Eucalypt_Rho_Corr_Vector <- c(Eucalypt_Rho_Corr_Vector, tmp)
  }
}

corr_Eucalypt_matrix <- diag(length(Eucalypt_Rho_List))
corr_Eucalypt_matrix[lower.tri(corr_Eucalypt_matrix, diag = FALSE)] <- Eucalypt_Rho_Corr_Vector
corr_Eucalypt_matrix <- t(corr_Eucalypt_matrix)
colnames(corr_Eucalypt_matrix) <- rownames(corr_Eucalypt_matrix) <- model_names

## Frogs ----

Frog_Rho_Corr_Vector <- c()

for(i in 1:(length(Frog_Rho_List)-1)){
  for(j in (i+1):length(Frog_Rho_List)){
    tmp <- cor(Frog_Rho_List[[i]][upper.tri(Frog_Rho_List[[i]], diag = FALSE)],
               Frog_Rho_List[[j]][upper.tri(Frog_Rho_List[[j]], diag = FALSE)])
    Frog_Rho_Corr_Vector <- c(Frog_Rho_Corr_Vector, tmp)
  }
}

corr_Frog_matrix <- diag(length(Frog_Rho_List))
corr_Frog_matrix[lower.tri(corr_Frog_matrix, diag = FALSE)] <- Frog_Rho_Corr_Vector
corr_Frog_matrix <- t(corr_Frog_matrix)
colnames(corr_Frog_matrix) <- rownames(corr_Frog_matrix) <- model_names

## Fungi ----

Fungi_Rho_Corr_Vector <- c()

for(i in 1:(length(Fungi_Rho_List)-1)){
  for(j in (i+1):length(Fungi_Rho_List)){
    tmp <- cor(Fungi_Rho_List[[i]][upper.tri(Fungi_Rho_List[[i]], diag = FALSE)],
               Fungi_Rho_List[[j]][upper.tri(Fungi_Rho_List[[j]], diag = FALSE)])
    Fungi_Rho_Corr_Vector <- c(Fungi_Rho_Corr_Vector, tmp)
  }
}

corr_Fungi_matrix <- diag(length(Fungi_Rho_List))
corr_Fungi_matrix[lower.tri(corr_Fungi_matrix, diag = FALSE)] <- Fungi_Rho_Corr_Vector
corr_Fungi_matrix <- t(corr_Fungi_matrix)
colnames(corr_Fungi_matrix) <- rownames(corr_Fungi_matrix) <- model_names

## Mosquito ----

Mosquito_Rho_Corr_Vector <- c()

for(i in 1:(length(Mosquito_Rho_List)-1)){
  for(j in (i+1):length(Mosquito_Rho_List)){
    tmp <- cor(Mosquito_Rho_List[[i]][upper.tri(Mosquito_Rho_List[[i]], diag = FALSE)],
               Mosquito_Rho_List[[j]][upper.tri(Mosquito_Rho_List[[j]], diag = FALSE)])
    Mosquito_Rho_Corr_Vector <- c(Mosquito_Rho_Corr_Vector, tmp)
  }
}

corr_Mosquito_matrix <- diag(length(Mosquito_Rho_List))
corr_Mosquito_matrix[lower.tri(corr_Mosquito_matrix, diag = FALSE)] <- Mosquito_Rho_Corr_Vector
corr_Mosquito_matrix <- t(corr_Mosquito_matrix)
colnames(corr_Mosquito_matrix) <- rownames(corr_Mosquito_matrix) <- model_names

### All Datasets ----

All_corr_list <- abind(corr_Bird_matrix, corr_Butterfly_matrix, corr_Eucalypt_matrix,
                       corr_Frog_matrix, corr_Fungi_matrix, corr_Mosquito_matrix,
                       along = 3)

Avg_corr_all_datasets <- apply(All_corr_list, MARGIN = c(1,2), mean, na.rm = TRUE)

# ----

####################################
### Rho Uncertainty Correlations ###
####################################

## Birds ----

Bird_Uncertainty_Corr_Vector <- c()

for(i in 1:(length(Bird_Uncertainty_List)-1)){
  for(j in (i+1):length(Bird_Uncertainty_List)){
    tmp <- cor(Bird_Uncertainty_List[[i]][upper.tri(Bird_Uncertainty_List[[i]], diag = FALSE)],
               Bird_Uncertainty_List[[j]][upper.tri(Bird_Uncertainty_List[[j]], diag = FALSE)])
    Bird_Uncertainty_Corr_Vector <- c(Bird_Uncertainty_Corr_Vector, tmp)
  }
}

corr_Bird_Uncertainty_matrix <- diag(length(Bird_Uncertainty_List))
corr_Bird_Uncertainty_matrix[lower.tri(corr_Bird_Uncertainty_matrix, diag = FALSE)] <- Bird_Uncertainty_Corr_Vector
corr_Bird_Uncertainty_matrix <- t(corr_Bird_Uncertainty_matrix)
colnames(corr_Bird_Uncertainty_matrix) <- rownames(corr_Bird_Uncertainty_matrix) <- model_names

## Butterfly ----

Butterfly_Uncertainty_Corr_Vector <- c()

for(i in 1:(length(Butterfly_Uncertainty_List)-1)){
  for(j in (i+1):length(Butterfly_Uncertainty_List)){
    tmp <- cor(Butterfly_Uncertainty_List[[i]][upper.tri(Butterfly_Uncertainty_List[[i]], diag = FALSE)],
               Butterfly_Uncertainty_List[[j]][upper.tri(Butterfly_Uncertainty_List[[j]], diag = FALSE)])
    Butterfly_Uncertainty_Corr_Vector <- c(Butterfly_Uncertainty_Corr_Vector, tmp)
  }
}

corr_Butterfly_Uncertainty_matrix <- diag(length(Butterfly_Uncertainty_List))
corr_Butterfly_Uncertainty_matrix[lower.tri(corr_Butterfly_Uncertainty_matrix, diag = FALSE)] <- Butterfly_Uncertainty_Corr_Vector
corr_Butterfly_Uncertainty_matrix <- t(corr_Butterfly_Uncertainty_matrix)
colnames(corr_Butterfly_Uncertainty_matrix) <- rownames(corr_Butterfly_Uncertainty_matrix) <- model_names

## Eucalypts ----

Eucalypt_Uncertainty_Corr_Vector <- c()

for(i in 1:(length(Eucalypt_Uncertainty_List)-1)){
  for(j in (i+1):length(Eucalypt_Uncertainty_List)){
    tmp <- cor(Eucalypt_Uncertainty_List[[i]][upper.tri(Eucalypt_Uncertainty_List[[i]], diag = FALSE)],
               Eucalypt_Uncertainty_List[[j]][upper.tri(Eucalypt_Uncertainty_List[[j]], diag = FALSE)])
    Eucalypt_Uncertainty_Corr_Vector <- c(Eucalypt_Uncertainty_Corr_Vector, tmp)
  }
}

corr_Eucalypt_Uncertainty_matrix <- diag(length(Eucalypt_Uncertainty_List))
corr_Eucalypt_Uncertainty_matrix[lower.tri(corr_Eucalypt_Uncertainty_matrix, diag = FALSE)] <- Eucalypt_Uncertainty_Corr_Vector
corr_Eucalypt_Uncertainty_matrix <- t(corr_Eucalypt_Uncertainty_matrix)
colnames(corr_Eucalypt_Uncertainty_matrix) <- rownames(corr_Eucalypt_Uncertainty_matrix) <- model_names

## Frogs ----

Frog_Uncertainty_Corr_Vector <- c()

for(i in 1:(length(Frog_Uncertainty_List)-1)){
  for(j in (i+1):length(Frog_Uncertainty_List)){
    tmp <- cor(Frog_Uncertainty_List[[i]][upper.tri(Frog_Uncertainty_List[[i]], diag = FALSE)],
               Frog_Uncertainty_List[[j]][upper.tri(Frog_Uncertainty_List[[j]], diag = FALSE)])
    Frog_Uncertainty_Corr_Vector <- c(Frog_Uncertainty_Corr_Vector, tmp)
  }
}

corr_Frog_Uncertainty_matrix <- diag(length(Frog_Uncertainty_List))
corr_Frog_Uncertainty_matrix[lower.tri(corr_Frog_Uncertainty_matrix, diag = FALSE)] <- Frog_Uncertainty_Corr_Vector
corr_Frog_Uncertainty_matrix <- t(corr_Frog_Uncertainty_matrix)
colnames(corr_Frog_Uncertainty_matrix) <- rownames(corr_Frog_Uncertainty_matrix) <- model_names

## Fungi ----

Fungi_Uncertainty_Corr_Vector <- c()

for(i in 1:(length(Fungi_Uncertainty_List)-1)){
  for(j in (i+1):length(Fungi_Uncertainty_List)){
    tmp <- cor(Fungi_Uncertainty_List[[i]][upper.tri(Fungi_Uncertainty_List[[i]], diag = FALSE)],
               Fungi_Uncertainty_List[[j]][upper.tri(Fungi_Uncertainty_List[[j]], diag = FALSE)])
    Fungi_Uncertainty_Corr_Vector <- c(Fungi_Uncertainty_Corr_Vector, tmp)
  }
}

corr_Fungi_Uncertainty_matrix <- diag(length(Fungi_Uncertainty_List))
corr_Fungi_Uncertainty_matrix[lower.tri(corr_Fungi_Uncertainty_matrix, diag = FALSE)] <- Fungi_Uncertainty_Corr_Vector
corr_Fungi_Uncertainty_matrix <- t(corr_Fungi_Uncertainty_matrix)
colnames(corr_Fungi_Uncertainty_matrix) <- rownames(corr_Fungi_Uncertainty_matrix) <- model_names

## Mosquito ----

Mosquito_Uncertainty_Corr_Vector <- c()

for(i in 1:(length(Mosquito_Uncertainty_List)-1)){
  for(j in (i+1):length(Mosquito_Uncertainty_List)){
    tmp <- cor(Mosquito_Uncertainty_List[[i]][upper.tri(Mosquito_Uncertainty_List[[i]], diag = FALSE)],
               Mosquito_Uncertainty_List[[j]][upper.tri(Mosquito_Uncertainty_List[[j]], diag = FALSE)])
    Mosquito_Uncertainty_Corr_Vector <- c(Mosquito_Uncertainty_Corr_Vector, tmp)
  }
}

corr_Mosquito_Uncertainty_matrix <- diag(length(Mosquito_Uncertainty_List))
corr_Mosquito_Uncertainty_matrix[lower.tri(corr_Mosquito_Uncertainty_matrix, diag = FALSE)] <- Mosquito_Uncertainty_Corr_Vector
corr_Mosquito_Uncertainty_matrix <- t(corr_Mosquito_Uncertainty_matrix)
colnames(corr_Mosquito_Uncertainty_matrix) <- rownames(corr_Mosquito_Uncertainty_matrix) <- model_names

### All Datasets ----

All_corr_Uncertainty_list <- abind(corr_Bird_Uncertainty_matrix, corr_Butterfly_Uncertainty_matrix, corr_Eucalypt_Uncertainty_matrix,
                       corr_Frog_Uncertainty_matrix, corr_Fungi_Uncertainty_matrix, corr_Mosquito_Uncertainty_matrix,
                       along = 3)

Avg_corr_Uncertainty_all_datasets <- apply(All_corr_Uncertainty_list, MARGIN = c(1,2), mean, na.rm = TRUE)

# ----

#########################
### Correlation Plots ###
#########################

## Model Correlation By Dataset

### Birds ----

pdf(file = "Corrplot_Birds.pdf")
Bird_corrplot <- corrplot(corr_Bird_matrix,
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
                          addgrid.col = "black",
                          na.label = "X")
dev.off()

### Butterflies ----

pdf(file = "Corrplot_Butterflies.pdf")
Butterfly_corrplot <- corrplot(corr_Butterfly_matrix,
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
                               addgrid.col = "black",
                               na.label = "X")
dev.off()

### Eucalypts ----

pdf(file = "Corrplot_Eucalypts.pdf")
Eucalypt_corrplot <- corrplot(corr_Eucalypt_matrix,
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
                              addgrid.col = "black",
                              na.label = "X")
dev.off()

### Frogs ----

pdf(file = "Corrplot_Frogs.pdf")
Frog_corrplot <- corrplot(corr_Frog_matrix,
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
                          addgrid.col = "black",
                          na.label = "X")
dev.off()

### Fungi ----

pdf(file = "Corrplot_Fungi.pdf")
Fungi_corrplot <- corrplot(corr_Fungi_matrix,
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
                           addgrid.col = "black",
                           na.label = "X")
dev.off()

### Mosquitos ----

pdf(file = "Corrplot_Mosquitos.pdf")
Mosquito_corrplot <- corrplot(corr_Mosquito_matrix,
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
                              addgrid.col = "black",
                              na.label = "X")
dev.off()

### All Datasets ----

pdf(file = "Corrplot_All_Datasets.pdf")
Avg_corrplot <- corrplot(Avg_corr_all_datasets,
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

#####################################
### Correlation Uncertainty Plots ###
#####################################

## Model Correlation By Dataset

### Birds ----

pdf(file = "Corrplot_Uncertainty_Birds.pdf")
Bird_Uncertainty_corrplot <- corrplot(corr_Bird_Uncertainty_matrix,
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
                          addCoef.col = "black",
                          na.label = "X")
dev.off()

### Butterflies ----

pdf(file = "Corrplot_Uncertainty_Butterflies.pdf")
Butterfly_Uncertainty_corrplot <- corrplot(corr_Butterfly_Uncertainty_matrix,
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
                               addCoef.col = "black",
                               na.label = "X")
dev.off()

### Eucalypts ----

pdf(file = "Corrplot_Uncertainty_Eucalypts.pdf")
Eucalypt_Uncertainty_corrplot <- corrplot(corr_Eucalypt_Uncertainty_matrix,
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
                              addCoef.col = "black",
                              na.label = "X")
dev.off()

### Frogs ----

pdf(file = "Corrplot_Uncertainty_Frogs.pdf")
Frog_Uncertainty_corrplot <- corrplot(corr_Frog_Uncertainty_matrix,
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
                          addCoef.col = "black",
                          na.label = "X")
dev.off()

### Fungi ----

pdf(file = "Corrplot_Uncertainty_Fungi.pdf")
Fungi_Uncertainty_corrplot <- corrplot(corr_Fungi_Uncertainty_matrix,
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
                           addCoef.col = "black",
                           na.label = "X")
dev.off()

### Mosquitos ----

pdf(file = "Corrplot_Uncertainty_Mosquitos.pdf")
Mosquito_Uncertainty_corrplot <- corrplot(corr_Mosquito_Uncertainty_matrix,
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
                              addCoef.col = "black",
                              na.label = "X")
dev.off()

### All Datasets ----

pdf(file = "Corrplot_Uncertainty_All_Datasets.pdf")
Avg_Uncertainty_corrplot <- corrplot(Avg_corr_Uncertainty_all_datasets,
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
                         addCoef.col = "black")
dev.off()

# ----

##################################
### Correlation Strength Plots ###
##################################

## Strength Metric Levels

Strength_Metric_Levels <- seq(0, 1, by = 0.1)

### Birds ----

Bird_Strength_Metric <- matrix(nrow = length(model_names),
                               ncol = length(Strength_Metric_Levels),
                               dimnames = list(model_names,
                                               seq(0,1,0.1)))

for(i in seq(length(Bird_Rho_List))){  # for each model in list
  
  for(j in 1:(length(Strength_Metric_Levels) - 1)){ # for each metric range
    
    Bird_Strength_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Bird_Rho_List[[i]][upper.tri(Bird_Rho_List[[i]])] > Strength_Metric_Levels[j] &  # lower
          Bird_Rho_List[[i]][upper.tri(Bird_Rho_List[[i]])] < Strength_Metric_Levels[j+1] # upper
        
      ) / length(Bird_Rho_List[[i]][upper.tri(Bird_Rho_List[[i]])]) # make proportion
    ) 
  }
}

### Butterflies ----

Butterfly_Strength_Metric <- matrix(nrow = length(model_names),
                               ncol = length(Strength_Metric_Levels),
                               dimnames = list(model_names,
                                               seq(0,1,0.1)))

for(i in seq(length(Butterfly_Rho_List))){  # for each model in list
  
  for(j in 1:(length(Strength_Metric_Levels) - 1)){ # for each metric range
    
    Butterfly_Strength_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Butterfly_Rho_List[[i]][upper.tri(Butterfly_Rho_List[[i]])] > Strength_Metric_Levels[j] &  # lower
        Butterfly_Rho_List[[i]][upper.tri(Butterfly_Rho_List[[i]])] < Strength_Metric_Levels[j+1] # upper
        
      ) / length(Butterfly_Rho_List[[i]][upper.tri(Butterfly_Rho_List[[i]])]) # make proportion
    ) 
  }
}

### Eucalypts ----

Eucalypts_Strength_Metric <- matrix(nrow = length(model_names),
                                    ncol = length(Strength_Metric_Levels),
                                    dimnames = list(model_names,
                                                    seq(0,1,0.1)))

for(i in seq(length(Eucalypt_Rho_List))){  # for each model in list
  
  for(j in 1:(length(Strength_Metric_Levels) - 1)){ # for each metric range
    
    Eucalypts_Strength_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Eucalypt_Rho_List[[i]][upper.tri(Eucalypt_Rho_List[[i]])] > Strength_Metric_Levels[j] &  # lower
        Eucalypt_Rho_List[[i]][upper.tri(Eucalypt_Rho_List[[i]])] < Strength_Metric_Levels[j+1] # upper
        
      ) / length(Eucalypt_Rho_List[[i]][upper.tri(Eucalypt_Rho_List[[i]])]) # make proportion
    ) 
  }
}

### Frogs ----

Frogs_Strength_Metric <- matrix(nrow = length(model_names),
                                ncol = length(Strength_Metric_Levels),
                                dimnames = list(model_names,
                                                seq(0,1,0.1)))

for(i in seq(length(Frog_Rho_List))){  # for each model in list
  
  for(j in 1:(length(Strength_Metric_Levels) - 1)){ # for each metric range
    
    Frogs_Strength_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Frog_Rho_List[[i]][upper.tri(Frog_Rho_List[[i]])] > Strength_Metric_Levels[j] &  # lower
        Frog_Rho_List[[i]][upper.tri(Frog_Rho_List[[i]])] < Strength_Metric_Levels[j+1] # upper
        
      ) / length(Frog_Rho_List[[i]][upper.tri(Frog_Rho_List[[i]])]) # make proportion
    ) 
  }
}

### Fungi ----

Fungi_Strength_Metric <- matrix(nrow = length(model_names),
                                ncol = length(Strength_Metric_Levels),
                                dimnames = list(model_names,
                                                seq(0,1,0.1)))

for(i in seq(length(Fungi_Rho_List))){  # for each model in list
  
  for(j in 1:(length(Strength_Metric_Levels) - 1)){ # for each metric range
    
    Fungi_Strength_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Fungi_Rho_List[[i]][upper.tri(Fungi_Rho_List[[i]])] > Strength_Metric_Levels[j] &  # lower
        Fungi_Rho_List[[i]][upper.tri(Fungi_Rho_List[[i]])] < Strength_Metric_Levels[j+1] # upper
        
      ) / length(Fungi_Rho_List[[i]][upper.tri(Fungi_Rho_List[[i]])]) # make proportion
    ) 
  }
}

### Mosquitos ----

Mosquito_Strength_Metric <- matrix(nrow = length(model_names),
                                   ncol = length(Strength_Metric_Levels),
                                   dimnames = list(model_names,
                                                   seq(0,1,0.1)))

for(i in seq(length(Mosquito_Rho_List))){  # for each model in list
  
  for(j in 1:(length(Strength_Metric_Levels) - 1)){ # for each metric range
    
    Mosquito_Strength_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Mosquito_Rho_List[[i]][upper.tri(Mosquito_Rho_List[[i]])] > Strength_Metric_Levels[j] &  # lower
        Mosquito_Rho_List[[i]][upper.tri(Mosquito_Rho_List[[i]])] < Strength_Metric_Levels[j+1] # upper
        
      ) / length(Mosquito_Rho_List[[i]][upper.tri(Mosquito_Rho_List[[i]])]) # make proportion
    ) 
  }
}

### All Datasets ----

All_Datasets_Strength_Array <- abind(Bird_Strength_Metric, Butterfly_Strength_Metric,
                                     Eucalypts_Strength_Metric, Frogs_Strength_Metric,
                                     Fungi_Strength_Metric, Mosquito_Strength_Metric,
                                     along = 3)

All_Datasets_Strength_Metric <- apply(All_Datasets_Strength_Array,
                                      c(1,2),
                                      mean, na.rm = TRUE)

# ----

## Strength Dataframes

### Birds ----

# Normal

reps <- (nrow(Bird_Rho_List[[1]])*(nrow(Bird_Rho_List[[1]])-1))/2

Strength_Rho_Birds <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Bird_Rho_List[[1]])[upper.tri(unlist(Bird_Rho_List[[1]]))],
                          unlist(Bird_Rho_List[[2]])[upper.tri(unlist(Bird_Rho_List[[2]]))],
                          unlist(Bird_Rho_List[[3]])[upper.tri(unlist(Bird_Rho_List[[3]]))],
                          unlist(Bird_Rho_List[[4]])[upper.tri(unlist(Bird_Rho_List[[4]]))],
                          unlist(Bird_Rho_List[[5]])[upper.tri(unlist(Bird_Rho_List[[5]]))],
                          unlist(Bird_Rho_List[[6]])[upper.tri(unlist(Bird_Rho_List[[6]]))]),
             dataset = rep("Birds", reps*6))

#Strength_Rho_Birds <- Strength_Rho_Birds[!is.na(Strength_Rho_Birds$strength),]
Strength_Rho_Birds$model <- factor(Strength_Rho_Birds$model,
                                   levels = c("MPR","HPR","LPR",
                                              "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Bird_Rho_List_Adj <- list(
  abs(Bird_Rho_List[[1]])/abs(Bird_Rho_List[[1]]),
  abs(Bird_Rho_List[[2]])/abs(Bird_Rho_List[[1]]),
  abs(Bird_Rho_List[[3]])/abs(Bird_Rho_List[[1]]),
  abs(Bird_Rho_List[[4]])/abs(Bird_Rho_List[[1]]),
  abs(Bird_Rho_List[[5]])/abs(Bird_Rho_List[[1]]),
  abs(Bird_Rho_List[[6]])/abs(Bird_Rho_List[[1]])
)

reps <- (nrow(Bird_Rho_List[[1]])*(nrow(Bird_Rho_List[[1]])-1))/2

Strength_Rho_Birds_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Bird_Rho_List_Adj[[1]])[upper.tri(unlist(Bird_Rho_List_Adj[[1]]))],
                          unlist(Bird_Rho_List_Adj[[2]])[upper.tri(unlist(Bird_Rho_List_Adj[[2]]))],
                          unlist(Bird_Rho_List_Adj[[3]])[upper.tri(unlist(Bird_Rho_List_Adj[[3]]))],
                          unlist(Bird_Rho_List_Adj[[4]])[upper.tri(unlist(Bird_Rho_List_Adj[[4]]))],
                          unlist(Bird_Rho_List_Adj[[5]])[upper.tri(unlist(Bird_Rho_List_Adj[[5]]))],
                          unlist(Bird_Rho_List_Adj[[6]])[upper.tri(unlist(Bird_Rho_List_Adj[[6]]))]),
             dataset = rep("Birds", reps*6))

#Strength_Rho_Birds_Adj <- Strength_Rho_Birds_Adj[!is.na(Strength_Rho_Birds_Adj$strength),]
Strength_Rho_Birds_Adj$model <- factor(Strength_Rho_Birds_Adj$model,
                                       levels = c("MPR","HPR","LPR",
                                                  "DPR","HLR-S","HLR-NS"))

### Butterflies ----

# Normal

reps <- (nrow(Butterfly_Rho_List[[1]])*(nrow(Butterfly_Rho_List[[1]])-1))/2

Strength_Rho_Butterfly <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Butterfly_Rho_List[[1]])[upper.tri(unlist(Butterfly_Rho_List[[1]]))],
                          unlist(Butterfly_Rho_List[[2]])[upper.tri(unlist(Butterfly_Rho_List[[2]]))],
                          unlist(Butterfly_Rho_List[[3]])[upper.tri(unlist(Butterfly_Rho_List[[3]]))],
                          unlist(Butterfly_Rho_List[[4]])[upper.tri(unlist(Butterfly_Rho_List[[4]]))],
                          unlist(Butterfly_Rho_List[[5]])[upper.tri(unlist(Butterfly_Rho_List[[5]]))],
                          unlist(Butterfly_Rho_List[[6]])[upper.tri(unlist(Butterfly_Rho_List[[6]]))]),
             dataset = rep("Butterflies", reps*6))

#Strength_Rho_Butterfly <- Strength_Rho_Butterfly[!is.na(Strength_Rho_Butterfly$strength),]
Strength_Rho_Butterfly$model <- factor(Strength_Rho_Butterfly$model,
                                       levels = c("MPR","HPR","LPR",
                                                  "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Butterfly_Rho_List_Adj <- list(
  abs(Butterfly_Rho_List[[1]])/abs(Butterfly_Rho_List[[1]]),
  abs(Butterfly_Rho_List[[2]])/abs(Butterfly_Rho_List[[1]]),
  abs(Butterfly_Rho_List[[3]])/abs(Butterfly_Rho_List[[1]]),
  abs(Butterfly_Rho_List[[4]])/abs(Butterfly_Rho_List[[1]]),
  abs(Butterfly_Rho_List[[5]])/abs(Butterfly_Rho_List[[1]]),
  abs(Butterfly_Rho_List[[6]])/abs(Butterfly_Rho_List[[1]])
)

reps <- (nrow(Butterfly_Rho_List[[1]])*(nrow(Butterfly_Rho_List[[1]])-1))/2

Strength_Rho_Butterfly_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Butterfly_Rho_List_Adj[[1]])[upper.tri(unlist(Butterfly_Rho_List_Adj[[1]]))],
                          unlist(Butterfly_Rho_List_Adj[[2]])[upper.tri(unlist(Butterfly_Rho_List_Adj[[2]]))],
                          unlist(Butterfly_Rho_List_Adj[[3]])[upper.tri(unlist(Butterfly_Rho_List_Adj[[3]]))],
                          unlist(Butterfly_Rho_List_Adj[[4]])[upper.tri(unlist(Butterfly_Rho_List_Adj[[4]]))],
                          unlist(Butterfly_Rho_List_Adj[[5]])[upper.tri(unlist(Butterfly_Rho_List_Adj[[5]]))],
                          unlist(Butterfly_Rho_List_Adj[[6]])[upper.tri(unlist(Butterfly_Rho_List_Adj[[6]]))]),
             dataset = rep("Butterflies", reps*6))

#Strength_Rho_Butterfly_Adj <- Strength_Rho_Butterfly_Adj[!is.na(Strength_Rho_Butterfly_Adj$strength),]
Strength_Rho_Butterfly_Adj$model <- factor(Strength_Rho_Butterfly_Adj$model,
                                           levels = c("MPR","HPR","LPR",
                                                      "DPR","HLR-S","HLR-NS"))

### Eucalypts ----

# Normal

reps <- (nrow(Eucalypt_Rho_List[[1]])*(nrow(Eucalypt_Rho_List[[1]])-1))/2

Strength_Rho_Eucalypt <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Eucalypt_Rho_List[[1]])[upper.tri(unlist(Eucalypt_Rho_List[[1]]))],
                          unlist(Eucalypt_Rho_List[[2]])[upper.tri(unlist(Eucalypt_Rho_List[[2]]))],
                          unlist(Eucalypt_Rho_List[[3]])[upper.tri(unlist(Eucalypt_Rho_List[[3]]))],
                          unlist(Eucalypt_Rho_List[[4]])[upper.tri(unlist(Eucalypt_Rho_List[[4]]))],
                          unlist(Eucalypt_Rho_List[[5]])[upper.tri(unlist(Eucalypt_Rho_List[[5]]))],
                          unlist(Eucalypt_Rho_List[[6]])[upper.tri(unlist(Eucalypt_Rho_List[[6]]))]),
             dataset = rep("Eucalypts", reps*6))

#Strength_Rho_Eucalypt <- Strength_Rho_Eucalypt[!is.na(Strength_Rho_Eucalypt$strength),]
Strength_Rho_Eucalypt$model <- factor(Strength_Rho_Eucalypt$model,
                                       levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Eucalypt_Rho_List_Adj <- list(
  abs(Eucalypt_Rho_List[[1]])/abs(Eucalypt_Rho_List[[1]]),
  abs(Eucalypt_Rho_List[[2]])/abs(Eucalypt_Rho_List[[1]]),
  abs(Eucalypt_Rho_List[[3]])/abs(Eucalypt_Rho_List[[1]]),
  abs(Eucalypt_Rho_List[[4]])/abs(Eucalypt_Rho_List[[1]]),
  abs(Eucalypt_Rho_List[[5]])/abs(Eucalypt_Rho_List[[1]]),
  abs(Eucalypt_Rho_List[[6]])/abs(Eucalypt_Rho_List[[1]])
)

reps <- (nrow(Eucalypt_Rho_List[[1]])*(nrow(Eucalypt_Rho_List[[1]])-1))/2

Strength_Rho_Eucalypt_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Eucalypt_Rho_List_Adj[[1]])[upper.tri(unlist(Eucalypt_Rho_List_Adj[[1]]))],
                          unlist(Eucalypt_Rho_List_Adj[[2]])[upper.tri(unlist(Eucalypt_Rho_List_Adj[[2]]))],
                          unlist(Eucalypt_Rho_List_Adj[[3]])[upper.tri(unlist(Eucalypt_Rho_List_Adj[[3]]))],
                          unlist(Eucalypt_Rho_List_Adj[[4]])[upper.tri(unlist(Eucalypt_Rho_List_Adj[[4]]))],
                          unlist(Eucalypt_Rho_List_Adj[[5]])[upper.tri(unlist(Eucalypt_Rho_List_Adj[[5]]))],
                          unlist(Eucalypt_Rho_List_Adj[[6]])[upper.tri(unlist(Eucalypt_Rho_List_Adj[[6]]))]),
             dataset = rep("Eucalypts", reps*6))

#Strength_Rho_Eucalypt_Adj <- Strength_Rho_Eucalypt_Adj[!is.na(Strength_Rho_Eucalypt_Adj$strength),]
Strength_Rho_Eucalypt_Adj$model <- factor(Strength_Rho_Eucalypt_Adj$model,
                                      levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

### Frogs ----

# Normal

reps <- (nrow(Frog_Rho_List[[1]])*(nrow(Frog_Rho_List[[1]])-1))/2

Strength_Rho_Frog <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Frog_Rho_List[[1]])[upper.tri(unlist(Frog_Rho_List[[1]]))],
                          unlist(Frog_Rho_List[[2]])[upper.tri(unlist(Frog_Rho_List[[2]]))],
                          unlist(Frog_Rho_List[[3]])[upper.tri(unlist(Frog_Rho_List[[3]]))],
                          unlist(Frog_Rho_List[[4]])[upper.tri(unlist(Frog_Rho_List[[4]]))],
                          unlist(Frog_Rho_List[[5]])[upper.tri(unlist(Frog_Rho_List[[5]]))],
                          unlist(Frog_Rho_List[[6]])[upper.tri(unlist(Frog_Rho_List[[6]]))]),
             dataset = rep("Frogs", reps*6))

#Strength_Rho_Frog <- Strength_Rho_Frog[!is.na(Strength_Rho_Frog$strength),]
Strength_Rho_Frog$model <- factor(Strength_Rho_Frog$model,
                                  levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Frog_Rho_List_Adj <- list(
  abs(Frog_Rho_List[[1]])/abs(Frog_Rho_List[[1]]),
  abs(Frog_Rho_List[[2]])/abs(Frog_Rho_List[[1]]),
  abs(Frog_Rho_List[[3]])/abs(Frog_Rho_List[[1]]),
  abs(Frog_Rho_List[[4]])/abs(Frog_Rho_List[[1]]),
  abs(Frog_Rho_List[[5]])/abs(Frog_Rho_List[[1]]),
  abs(Frog_Rho_List[[6]])/abs(Frog_Rho_List[[1]])
)

reps <- (nrow(Frog_Rho_List[[1]])*(nrow(Frog_Rho_List[[1]])-1))/2

Strength_Rho_Frog_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Frog_Rho_List_Adj[[1]])[upper.tri(unlist(Frog_Rho_List_Adj[[1]]))],
                          unlist(Frog_Rho_List_Adj[[2]])[upper.tri(unlist(Frog_Rho_List_Adj[[2]]))],
                          unlist(Frog_Rho_List_Adj[[3]])[upper.tri(unlist(Frog_Rho_List_Adj[[3]]))],
                          unlist(Frog_Rho_List_Adj[[4]])[upper.tri(unlist(Frog_Rho_List_Adj[[4]]))],
                          unlist(Frog_Rho_List_Adj[[5]])[upper.tri(unlist(Frog_Rho_List_Adj[[5]]))],
                          unlist(Frog_Rho_List_Adj[[6]])[upper.tri(unlist(Frog_Rho_List_Adj[[6]]))]),
             dataset = rep("Frogs", reps*6))

#Strength_Rho_Frog_Adj <- Strength_Rho_Frog_Adj[!is.na(Strength_Rho_Frog_Adj$strength),]
Strength_Rho_Frog_Adj$model <- factor(Strength_Rho_Frog_Adj$model,
                                  levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

### Fungi ----

# Normal

reps <- (nrow(Fungi_Rho_List[[1]])*(nrow(Fungi_Rho_List[[1]])-1))/2

Strength_Rho_Fungi <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Fungi_Rho_List[[1]])[upper.tri(unlist(Fungi_Rho_List[[1]]))],
                          unlist(Fungi_Rho_List[[2]])[upper.tri(unlist(Fungi_Rho_List[[2]]))],
                          unlist(Fungi_Rho_List[[3]])[upper.tri(unlist(Fungi_Rho_List[[3]]))],
                          unlist(Fungi_Rho_List[[4]])[upper.tri(unlist(Fungi_Rho_List[[4]]))],
                          unlist(Fungi_Rho_List[[5]])[upper.tri(unlist(Fungi_Rho_List[[5]]))],
                          unlist(Fungi_Rho_List[[6]])[upper.tri(unlist(Fungi_Rho_List[[6]]))]),
             dataset = rep("Fungi", reps*6))

#Strength_Rho_Fungi <- Strength_Rho_Fungi[!is.na(Strength_Rho_Fungi$strength),]
Strength_Rho_Fungi$model <- factor(Strength_Rho_Fungi$model,
                                   levels = c("MPR","HPR","LPR",
                                              "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Fungi_Rho_List_Adj <- list(
  abs(Fungi_Rho_List[[1]])/abs(Fungi_Rho_List[[1]]),
  abs(Fungi_Rho_List[[2]])/abs(Fungi_Rho_List[[1]]),
  abs(Fungi_Rho_List[[3]])/abs(Fungi_Rho_List[[1]]),
  abs(Fungi_Rho_List[[4]])/abs(Fungi_Rho_List[[1]]),
  abs(Fungi_Rho_List[[5]])/abs(Fungi_Rho_List[[1]]),
  abs(Fungi_Rho_List[[6]])/abs(Fungi_Rho_List[[1]])
)

reps <- (nrow(Fungi_Rho_List[[1]])*(nrow(Fungi_Rho_List[[1]])-1))/2

Strength_Rho_Fungi_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Fungi_Rho_List_Adj[[1]])[upper.tri(unlist(Fungi_Rho_List_Adj[[1]]))],
                          unlist(Fungi_Rho_List_Adj[[2]])[upper.tri(unlist(Fungi_Rho_List_Adj[[2]]))],
                          unlist(Fungi_Rho_List_Adj[[3]])[upper.tri(unlist(Fungi_Rho_List_Adj[[3]]))],
                          unlist(Fungi_Rho_List_Adj[[4]])[upper.tri(unlist(Fungi_Rho_List_Adj[[4]]))],
                          unlist(Fungi_Rho_List_Adj[[5]])[upper.tri(unlist(Fungi_Rho_List_Adj[[5]]))],
                          unlist(Fungi_Rho_List_Adj[[6]])[upper.tri(unlist(Fungi_Rho_List_Adj[[6]]))]),
             dataset = rep("Fungi", reps*6))

#Strength_Rho_Fungi_Adj <- Strength_Rho_Fungi_Adj[!is.na(Strength_Rho_Fungi_Adj$strength),]
Strength_Rho_Fungi_Adj$model <- factor(Strength_Rho_Fungi_Adj$model,
                                       levels = c("MPR","HPR","LPR",
                                                  "DPR","HLR-S","HLR-NS"))

### Mosquitos ----

# Normal

reps <- (nrow(Mosquito_Rho_List[[1]])*(nrow(Mosquito_Rho_List[[1]])-1))/2

Strength_Rho_Mosquito <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Mosquito_Rho_List[[1]])[upper.tri(unlist(Mosquito_Rho_List[[1]]))],
                          unlist(Mosquito_Rho_List[[2]])[upper.tri(unlist(Mosquito_Rho_List[[2]]))],
                          unlist(Mosquito_Rho_List[[3]])[upper.tri(unlist(Mosquito_Rho_List[[3]]))],
                          unlist(Mosquito_Rho_List[[4]])[upper.tri(unlist(Mosquito_Rho_List[[4]]))],
                          unlist(Mosquito_Rho_List[[5]])[upper.tri(unlist(Mosquito_Rho_List[[5]]))],
                          unlist(Mosquito_Rho_List[[6]])[upper.tri(unlist(Mosquito_Rho_List[[6]]))]),
             dataset = rep("Mosquitos", reps*6))

#Strength_Rho_Mosquito <- Strength_Rho_Mosquito[!is.na(Strength_Rho_Mosquito$strength),]
Strength_Rho_Mosquito$model <- factor(Strength_Rho_Mosquito$model,
                                      levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Mosquito_Rho_List_Adj <- list(
  abs(Mosquito_Rho_List[[1]])/abs(Mosquito_Rho_List[[1]]),
  abs(Mosquito_Rho_List[[2]])/abs(Mosquito_Rho_List[[1]]),
  abs(Mosquito_Rho_List[[3]])/abs(Mosquito_Rho_List[[1]]),
  abs(Mosquito_Rho_List[[4]])/abs(Mosquito_Rho_List[[1]]),
  abs(Mosquito_Rho_List[[5]])/abs(Mosquito_Rho_List[[1]]),
  abs(Mosquito_Rho_List[[6]])/abs(Mosquito_Rho_List[[1]])
)

reps <- (nrow(Mosquito_Rho_List[[1]])*(nrow(Mosquito_Rho_List[[1]])-1))/2

Strength_Rho_Mosquito_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Mosquito_Rho_List_Adj[[1]])[upper.tri(unlist(Mosquito_Rho_List_Adj[[1]]))],
                          unlist(Mosquito_Rho_List_Adj[[2]])[upper.tri(unlist(Mosquito_Rho_List_Adj[[2]]))],
                          unlist(Mosquito_Rho_List_Adj[[3]])[upper.tri(unlist(Mosquito_Rho_List_Adj[[3]]))],
                          unlist(Mosquito_Rho_List_Adj[[4]])[upper.tri(unlist(Mosquito_Rho_List_Adj[[4]]))],
                          unlist(Mosquito_Rho_List_Adj[[5]])[upper.tri(unlist(Mosquito_Rho_List_Adj[[5]]))],
                          unlist(Mosquito_Rho_List_Adj[[6]])[upper.tri(unlist(Mosquito_Rho_List_Adj[[6]]))]),
             dataset = rep("Mosquitos", reps*6))

#Strength_Rho_Mosquito_Adj <- Strength_Rho_Mosquito_Adj[!is.na(Strength_Rho_Mosquito_Adj$strength),]
Strength_Rho_Mosquito_Adj$model <- factor(Strength_Rho_Mosquito_Adj$model,
                                      levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

### All Datsets ----

# Normal

Strength_Rho_All <- rbind(Strength_Rho_Birds,
                          Strength_Rho_Butterfly,
                          Strength_Rho_Eucalypt,
                          Strength_Rho_Frog,
                          Strength_Rho_Fungi,
                          Strength_Rho_Mosquito)

Strength_Rho_All$model <- factor(Strength_Rho_All$model,
                                 levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Strength_Rho_All_Adj <- rbind(Strength_Rho_Birds_Adj,
                              Strength_Rho_Butterfly_Adj,
                              Strength_Rho_Eucalypt_Adj,
                              Strength_Rho_Frog_Adj,
                              Strength_Rho_Fungi_Adj,
                              Strength_Rho_Mosquito_Adj)

Strength_Rho_All_Adj$model <- factor(Strength_Rho_All_Adj$model,
                                     levels = c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS"))

### ----

## set colour scale

col_palette <- brewer.pal(6, "Dark2")

## need to convert strength metrics to correct dataframe format for ggplot

### All Datasets ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(All_Datasets_Strength_Metric))){
  
  Model_tmp <- rep(rownames(All_Datasets_Strength_Metric)[i],
                   ncol(All_Datasets_Strength_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(All_Datasets_Strength_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- All_Datasets_Strength_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

All_Datasets_Strength_ggplot_df <- data.frame(Model = Model,
                                              Metric = Metric,
                                              Value = Value)

All_Datasets_Strength_ggplot_df$Model <- factor(All_Datasets_Strength_ggplot_df$Model,
                                                levels = c("MPR","HPR","LPR","DPR",
                                                           "HLR-S","HLR-NS"))

### Birds ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Bird_Strength_Metric))){
  
  Model_tmp <- rep(rownames(Bird_Strength_Metric)[i],
                   ncol(Bird_Strength_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Bird_Strength_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Bird_Strength_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Birds_Strength_ggplot_df <- data.frame(Model = Model,
                                       Metric = Metric,
                                       Value = Value)

Birds_Strength_ggplot_df$Model <- factor(Birds_Strength_ggplot_df$Model,
                                         levels = c("MPR","HPR","LPR","DPR",
                                                    "HLR-S","HLR-NS"))

### Butterflies ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Butterfly_Strength_Metric))){
  
  Model_tmp <- rep(rownames(Butterfly_Strength_Metric)[i],
                   ncol(Butterfly_Strength_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Butterfly_Strength_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Butterfly_Strength_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Butterflies_Strength_ggplot_df <- data.frame(Model = Model,
                                             Metric = Metric,
                                             Value = Value)

Butterflies_Strength_ggplot_df$Model <- factor(Butterflies_Strength_ggplot_df$Model,
                                               levels = c("MPR","HPR","LPR","DPR",
                                                          "HLR-S","HLR-NS"))

### Eucalypts ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Eucalypts_Strength_Metric))){
  
  Model_tmp <- rep(rownames(Eucalypts_Strength_Metric)[i],
                   ncol(Eucalypts_Strength_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Eucalypts_Strength_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Eucalypts_Strength_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Eucalypts_Strength_ggplot_df <- data.frame(Model = Model,
                                           Metric = Metric,
                                           Value = Value)

Eucalypts_Strength_ggplot_df$Model <- factor(Eucalypts_Strength_ggplot_df$Model,
                                             levels = c("MPR","HPR","LPR","DPR",
                                                        "HLR-S","HLR-NS"))

### Frogs ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Frogs_Strength_Metric))){
  
  Model_tmp <- rep(rownames(Frogs_Strength_Metric)[i],
                   ncol(Frogs_Strength_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Frogs_Strength_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Frogs_Strength_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Frogs_Strength_ggplot_df <- data.frame(Model = Model,
                                       Metric = Metric,
                                       Value = Value)

Frogs_Strength_ggplot_df$Model <- factor(Frogs_Strength_ggplot_df$Model,
                                         levels = c("MPR","HPR","LPR","DPR",
                                                    "HLR-S","HLR-NS"))

### Fungi ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Fungi_Strength_Metric))){
  
  Model_tmp <- rep(rownames(Fungi_Strength_Metric)[i],
                   ncol(Fungi_Strength_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Fungi_Strength_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Fungi_Strength_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Fungi_Strength_ggplot_df <- data.frame(Model = Model,
                                       Metric = Metric,
                                       Value = Value)

Fungi_Strength_ggplot_df$Model <- factor(Fungi_Strength_ggplot_df$Model,
                                         levels = c("MPR","HPR","LPR","DPR",
                                                    "HLR-S","HLR-NS"))

### Mosquitos ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Mosquito_Strength_Metric))){
  
  Model_tmp <- rep(rownames(Mosquito_Strength_Metric)[i],
                   ncol(Mosquito_Strength_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Mosquito_Strength_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Mosquito_Strength_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Mosquitos_Strength_ggplot_df <- data.frame(Model = Model,
                                           Metric = Metric,
                                           Value = Value)

Mosquitos_Strength_ggplot_df$Model <- factor(Mosquitos_Strength_ggplot_df$Model,
                                             levels = c("MPR","HPR","LPR","DPR",
                                                        "HLR-S","HLR-NS"))

# ----

## ggplot barplots

### All Datasets ----

colour <- autoColourScheme(All_Datasets_Strength_ggplot_df)

ggplot(All_Datasets_Strength_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Strength") +
  facet_wrap(~ Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Strength_Barplot_All_Datasets.pdf", 
       width = 8, height = 5, units = "in")

### Birds ----

colour <- autoColourScheme(Birds_Strength_ggplot_df)

ggplot(Birds_Strength_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Strength") +
  facet_wrap(~ Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Strength_Barplot_Birds.pdf", 
       width = 8, height = 5, units = "in")

### Butterflies ----

colour <- autoColourScheme(Butterflies_Strength_ggplot_df)

ggplot(Butterflies_Strength_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Strength") +
  facet_wrap(~ Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Strength_Barplot_Butterflies.pdf", 
       width = 8, height = 5, units = "in")

### Eucalypts ----

colour <- autoColourScheme(Eucalypts_Strength_ggplot_df)

ggplot(Eucalypts_Strength_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Strength") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Strength_Barplot_Eucalypts.pdf", 
       width = 8, height = 5, units = "in")

### Frogs ----

colour <- autoColourScheme(Frogs_Strength_ggplot_df)

ggplot(Frogs_Strength_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Strength") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Strength_Barplot_Frogs.pdf", 
       width = 8, height = 5, units = "in")

### Fungi ----

colour <- autoColourScheme(Fungi_Strength_ggplot_df)

ggplot(Fungi_Strength_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Strength") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Strength_Barplot_Fungi.pdf", 
       width = 8, height = 5, units = "in")

### Mosquitos ----

colour <- autoColourScheme(Mosquitos_Strength_ggplot_df)

ggplot(Mosquitos_Strength_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Strength") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Strength_Barplot_Mosquitos.pdf", 
       width = 8, height = 5, units = "in")

# ----

## ggplot boxplots

### Birds ----

colour <- autoColourScheme(Strength_Rho_Birds[Strength_Rho_Birds$model != "HPR" &
                                                Strength_Rho_Birds$model != "HLR-S",])

ggplot(Strength_Rho_Birds, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,1))
ggsave("Strength_Rho_Birds_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Strength_Rho_Birds_Adj[Strength_Rho_Birds_Adj$model != "MPR",],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative correlation strength") +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  coord_cartesian(ylim = c(-1,7))
ggsave("Strength_Adj_Rho_Birds_boxplot.pdf", units = "in", width = 7, height = 7)

### Butterflies ----

colour <- autoColourScheme(Strength_Rho_Butterfly[Strength_Rho_Butterfly$model != "HPR" &
                                                    Strength_Rho_Butterfly$model != "HLR-S",])

ggplot(Strength_Rho_Butterfly, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,1))
ggsave("Strength_Rho_Butterflies_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Strength_Rho_Butterfly_Adj[Strength_Rho_Butterfly_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,2.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Strength_Adj_Rho_Butterflies_boxplot.pdf", units = "in", width = 7, height = 7)

### Eucalypts ----

colour <- autoColourScheme(Strength_Rho_Eucalypt)

ggplot(Strength_Rho_Eucalypt, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,0.75))
ggsave("Strength_Rho_Eucalypts_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Strength_Rho_Eucalypt_Adj[Strength_Rho_Eucalypt_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,8.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Strength_Adj_Rho_Eucalypts_boxplot.pdf", units = "in", width = 7, height = 7)

### Frogs ----

colour <- autoColourScheme(Strength_Rho_Frog)

ggplot(Strength_Rho_Frog, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,1))
ggsave("Strength_Rho_Frogs_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Strength_Rho_Frog_Adj[Strength_Rho_Frog_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Strength_Adj_Rho_Frogs_boxplot.pdf", units = "in", width = 7, height = 7)

### Fungi ----

colour <- autoColourScheme(Strength_Rho_Fungi[Strength_Rho_Fungi$model != "HLR-S",])

ggplot(Strength_Rho_Fungi, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,0.6))
ggsave("Strength_Rho_Fungi_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Strength_Rho_Fungi_Adj[Strength_Rho_Fungi_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,4)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Strength_Adj_Rho_Fungi_boxplot.pdf", units = "in", width = 7, height = 7)

### Mosquitos ----

colour <- autoColourScheme(Strength_Rho_Mosquito)

ggplot(Strength_Rho_Mosquito[!is.na(Strength_Rho_Mosquito$strength), ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,1))
ggsave("Strength_Rho_Mosquitos_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Strength_Rho_Mosquito_Adj[!is.na(Strength_Rho_Mosquito_Adj$strength) &
                                   Strength_Rho_Mosquito_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,10)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Strength_Adj_Rho_Mosquitos_boxplot.pdf", units = "in", width = 7, height = 7)

### All Datasets ----

# Actually all

colour <- autoColourScheme(Strength_Rho_All)

ggplot(Strength_Rho_All[!is.na(Strength_Rho_All$strength), ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,1))
ggsave("Strength_Rho_All_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Strength_Rho_All_Adj[!is.na(Strength_Rho_All_Adj$strength) &
                              Strength_Rho_All_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,6.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Strength_Rho_Adj_All_boxplot.pdf", units = "in", width = 7, height = 7)

# Smallest four

colour <- autoColourScheme(Strength_Rho_All)

ggplot(Strength_Rho_All[!is.na(Strength_Rho_All$strength) &
                          Strength_Rho_All$dataset != "Birds" &
                          Strength_Rho_All$dataset != "Butterflies", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,1))
ggsave("Strength_Rho_Small4_boxplot.pdf", units = "in", width = 7, height = 7)

small4RelStr <- ggplot(Strength_Rho_All_Adj[!is.na(Strength_Rho_All_Adj$strength) &
                              Strength_Rho_All_Adj$model != "MPR" &
                              Strength_Rho_All_Adj$dataset != "Birds" &
                              Strength_Rho_All_Adj$dataset != "Butterflies", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.text = element_text(colour = "black")) +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,7.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")

small4RelStr

ggsave("Strength_Rho_Adj_Small4_boxplot.pdf", units = "in", width = 7, height = 7)

# Largest two

colour <- autoColourScheme(Strength_Rho_All[Strength_Rho_All$model != "HPR" &
                                              Strength_Rho_All$model != "HLR-S",])

ggplot(Strength_Rho_All[Strength_Rho_All$dataset %in% c("Birds", "Butterflies"), ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",) +
  labs(x = "Model",
       y = "Correlation strength") +
  coord_cartesian(ylim = c(0,1))
ggsave("Strength_Rho_Large2_boxplot.pdf", units = "in", width = 7, height = 7)

big2RelStr <- ggplot(Strength_Rho_All_Adj[Strength_Rho_All_Adj$model != "MPR" &
                              Strength_Rho_All_Adj$dataset %in% c("Birds","Butterflies"), ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.text = element_text(colour = "black")) +
  labs(x = "Model",
       y = "Relative correlation strength") +
  coord_cartesian(ylim = c(0,6.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")

big2RelStr

ggsave("Strength_Rho_Adj_Large2_boxplot.pdf", units = "in", width = 7, height = 7)

## Both side by side

plot_grid(small4RelStr, big2RelStr, align = "h", labels = "auto")

ggsave(filename = "relStrSideBySide.pdf", width = 8, height = 4, units = "in", dpi = 600)
### ----


#############################
### Uncertainty Bar Plots ###
#############################

## Correlation Uncertainty By Dataset

Uncertainty_Metric_Levels <- seq(0, 2, by = 0.1)

### Birds ----

Bird_Uncertainty_Metric <- matrix(nrow = length(model_names),
                                  ncol = (length(Uncertainty_Metric_Levels) - 1),
                                  dimnames = list(model_names,
                                                  seq(0,1.9,0.1)))

for(i in seq(length(Bird_Uncertainty_List))){  # for each model in list
  
  for(j in 1:(length(Uncertainty_Metric_Levels) - 1)){ # for each metric range
  
    Bird_Uncertainty_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
      Bird_Uncertainty_List[[i]][upper.tri(Bird_Uncertainty_List[[i]])] > Uncertainty_Metric_Levels[j] &  # lower
      Bird_Uncertainty_List[[i]][upper.tri(Bird_Uncertainty_List[[i]])] < Uncertainty_Metric_Levels[j+1] # upper
      
      ) / length(Bird_Uncertainty_List[[i]][upper.tri(Bird_Uncertainty_List[[i]])]) # make proportion
    ) 
  }
}

### Butterflies ----

Butterfly_Uncertainty_Metric <- matrix(nrow = length(model_names),
                                       ncol = (length(Uncertainty_Metric_Levels) - 1),
                                       dimnames = list(model_names,
                                                       seq(0,1.9,0.1)))

for(i in seq(length(Butterfly_Uncertainty_List))){  # for each model in list
  
  for(j in 1:(length(Uncertainty_Metric_Levels) - 1)){ # for each metric range
    
    Butterfly_Uncertainty_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Butterfly_Uncertainty_List[[i]][upper.tri(Butterfly_Uncertainty_List[[i]])] > Uncertainty_Metric_Levels[j] &  # lower
        Butterfly_Uncertainty_List[[i]][upper.tri(Butterfly_Uncertainty_List[[i]])] < Uncertainty_Metric_Levels[j+1] # upper
        
      ) / length(Butterfly_Uncertainty_List[[i]][upper.tri(Butterfly_Uncertainty_List[[i]])]) # make proportion
    ) 
  }
}

### Eucalypts ----

Eucalypt_Uncertainty_Metric <- matrix(nrow = length(model_names),
                                      ncol = (length(Uncertainty_Metric_Levels) - 1),
                                      dimnames = list(model_names,
                                                      seq(0,1.9,0.1)))

for(i in seq(length(Eucalypt_Uncertainty_List))){  # for each model in list
  
  for(j in 1:(length(Uncertainty_Metric_Levels) - 1)){ # for each metric range
    
    Eucalypt_Uncertainty_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Eucalypt_Uncertainty_List[[i]][upper.tri(Eucalypt_Uncertainty_List[[i]])] > Uncertainty_Metric_Levels[j] &  # lower
        Eucalypt_Uncertainty_List[[i]][upper.tri(Eucalypt_Uncertainty_List[[i]])] < Uncertainty_Metric_Levels[j+1] # upper
        
      ) / length(Eucalypt_Uncertainty_List[[i]][upper.tri(Eucalypt_Uncertainty_List[[i]])]) # make proportion
    ) 
  }
}

### Frogs ----

Frog_Uncertainty_Metric <- matrix(nrow = length(model_names),
                                  ncol = (length(Uncertainty_Metric_Levels) - 1),
                                  dimnames = list(model_names,
                                                  seq(0,1.9,0.1)))

for(i in seq(length(Frog_Uncertainty_List))){  # for each model in list
  
  for(j in 1:(length(Uncertainty_Metric_Levels) - 1)){ # for each metric range
    
    Frog_Uncertainty_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Frog_Uncertainty_List[[i]][upper.tri(Frog_Uncertainty_List[[i]])] > Uncertainty_Metric_Levels[j] &  # lower
        Frog_Uncertainty_List[[i]][upper.tri(Frog_Uncertainty_List[[i]])] < Uncertainty_Metric_Levels[j+1] # upper
        
      ) / length(Frog_Uncertainty_List[[i]][upper.tri(Frog_Uncertainty_List[[i]])]) # make proportion
    ) 
  }
}

### Fungi ----

Fungi_Uncertainty_Metric <- matrix(nrow = length(model_names),
                                   ncol = (length(Uncertainty_Metric_Levels) - 1),
                                   dimnames = list(model_names,
                                                   seq(0,1.9,0.1)))

for(i in seq(length(Fungi_Uncertainty_List))){  # for each model in list
  
  for(j in 1:(length(Uncertainty_Metric_Levels) - 1)){ # for each metric range
    
    Fungi_Uncertainty_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Fungi_Uncertainty_List[[i]][upper.tri(Fungi_Uncertainty_List[[i]])] > Uncertainty_Metric_Levels[j] &  # lower
        Fungi_Uncertainty_List[[i]][upper.tri(Fungi_Uncertainty_List[[i]])] < Uncertainty_Metric_Levels[j+1] # upper
        
      ) / length(Fungi_Uncertainty_List[[i]][upper.tri(Fungi_Uncertainty_List[[i]])]) # make proportion
    ) 
  }
}

### Mosquito ----

Mosquito_Uncertainty_Metric <- matrix(nrow = length(model_names),
                                      ncol = (length(Uncertainty_Metric_Levels) - 1),
                                      dimnames = list(model_names,
                                                      seq(0,1.9,0.1)))

for(i in seq(length(Mosquito_Uncertainty_List))){  # for each model in list
  
  for(j in 1:(length(Uncertainty_Metric_Levels) - 1)){ # for each metric range
    
    Mosquito_Uncertainty_Metric[i,j] <- ( #fill matrix with test value
      
      sum(      # convert logical to integer
        
        Mosquito_Uncertainty_List[[i]][upper.tri(Mosquito_Uncertainty_List[[i]])] > Uncertainty_Metric_Levels[j] &  # lower
        Mosquito_Uncertainty_List[[i]][upper.tri(Mosquito_Uncertainty_List[[i]])] < Uncertainty_Metric_Levels[j+1] # upper
        
      ) / length(Mosquito_Uncertainty_List[[i]][upper.tri(Mosquito_Uncertainty_List[[i]])]) # make proportion
    ) 
  }
}

### All datasets ----

All_Datasets_Uncertainty_Array <- abind(Bird_Uncertainty_Metric, Butterfly_Uncertainty_Metric,
                                        Eucalypt_Uncertainty_Metric, Frog_Uncertainty_Metric,
                                        Fungi_Uncertainty_Metric, Mosquito_Uncertainty_Metric,
                                        along = 3)

All_Datasets_Uncertainty_Metric <- apply(All_Datasets_Uncertainty_Array,
                                         MARGIN = c(1,2),
                                         mean, na.rm = TRUE)

# ----

## Uncertainty Dataframes

### Birds ----

# Normal

reps <- (nrow(Bird_Uncertainty_List[[1]])*(nrow(Bird_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Birds <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Bird_Uncertainty_List[[1]])[upper.tri(unlist(Bird_Uncertainty_List[[1]]))],
                          unlist(Bird_Uncertainty_List[[2]])[upper.tri(unlist(Bird_Uncertainty_List[[2]]))],
                          unlist(Bird_Uncertainty_List[[3]])[upper.tri(unlist(Bird_Uncertainty_List[[3]]))],
                          unlist(Bird_Uncertainty_List[[4]])[upper.tri(unlist(Bird_Uncertainty_List[[4]]))],
                          unlist(Bird_Uncertainty_List[[5]])[upper.tri(unlist(Bird_Uncertainty_List[[5]]))],
                          unlist(Bird_Uncertainty_List[[6]])[upper.tri(unlist(Bird_Uncertainty_List[[6]]))]),
             dataset = rep("Birds", reps*6))

#Uncertainty_Rho_Birds <- Uncertainty_Rho_Birds[!is.na(Uncertainty_Rho_Birds$strength),]
Uncertainty_Rho_Birds$model <- factor(Uncertainty_Rho_Birds$model,
                                      levels = c("MPR","HPR","LPR",
                                                 "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Bird_Uncertainty_List_Adj <- list(
  Bird_Uncertainty_List[[1]]/Bird_Uncertainty_List[[1]],
  Bird_Uncertainty_List[[2]]/Bird_Uncertainty_List[[1]],
  Bird_Uncertainty_List[[3]]/Bird_Uncertainty_List[[1]],
  Bird_Uncertainty_List[[4]]/Bird_Uncertainty_List[[1]],
  Bird_Uncertainty_List[[5]]/Bird_Uncertainty_List[[1]],
  Bird_Uncertainty_List[[6]]/Bird_Uncertainty_List[[1]]
)

reps <- (nrow(Bird_Uncertainty_List[[1]])*(nrow(Bird_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Birds_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Bird_Uncertainty_List_Adj[[1]])[upper.tri(unlist(Bird_Uncertainty_List_Adj[[1]]))],
                          unlist(Bird_Uncertainty_List_Adj[[2]])[upper.tri(unlist(Bird_Uncertainty_List_Adj[[2]]))],
                          unlist(Bird_Uncertainty_List_Adj[[3]])[upper.tri(unlist(Bird_Uncertainty_List_Adj[[3]]))],
                          unlist(Bird_Uncertainty_List_Adj[[4]])[upper.tri(unlist(Bird_Uncertainty_List_Adj[[4]]))],
                          unlist(Bird_Uncertainty_List_Adj[[5]])[upper.tri(unlist(Bird_Uncertainty_List_Adj[[5]]))],
                          unlist(Bird_Uncertainty_List_Adj[[6]])[upper.tri(unlist(Bird_Uncertainty_List_Adj[[6]]))]),
             dataset = rep("Birds", reps*6))

#Uncertainty_Rho_Birds_Adj <- Uncertainty_Rho_Birds_Adj[!is.na(Uncertainty_Rho_Birds_Adj$strength),]
Uncertainty_Rho_Birds_Adj$model <- factor(Uncertainty_Rho_Birds_Adj$model,
                                          levels = c("MPR","HPR","LPR",
                                                     "DPR","HLR-S","HLR-NS"))


### Butterflies ----

# Normal

reps <- (nrow(Butterfly_Uncertainty_List[[1]])*(nrow(Butterfly_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Butterfly <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Butterfly_Uncertainty_List[[1]])[upper.tri(unlist(Butterfly_Uncertainty_List[[1]]))],
                          unlist(Butterfly_Uncertainty_List[[2]])[upper.tri(unlist(Butterfly_Uncertainty_List[[2]]))],
                          unlist(Butterfly_Uncertainty_List[[3]])[upper.tri(unlist(Butterfly_Uncertainty_List[[3]]))],
                          unlist(Butterfly_Uncertainty_List[[4]])[upper.tri(unlist(Butterfly_Uncertainty_List[[4]]))],
                          unlist(Butterfly_Uncertainty_List[[5]])[upper.tri(unlist(Butterfly_Uncertainty_List[[5]]))],
                          unlist(Butterfly_Uncertainty_List[[6]])[upper.tri(unlist(Butterfly_Uncertainty_List[[6]]))]),
             dataset = rep("Butterflies", reps*6))

#Uncertainty_Rho_Butterfly <- Uncertainty_Rho_Butterfly[!is.na(Uncertainty_Rho_Butterfly$strength),]
Uncertainty_Rho_Butterfly$model <- factor(Uncertainty_Rho_Butterfly$model,
                                          levels = c("MPR","HPR","LPR",
                                                     "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Butterfly_Uncertainty_List_Adj <- list(
  Butterfly_Uncertainty_List[[1]]/Butterfly_Uncertainty_List[[1]],
  Butterfly_Uncertainty_List[[2]]/Butterfly_Uncertainty_List[[1]],
  Butterfly_Uncertainty_List[[3]]/Butterfly_Uncertainty_List[[1]],
  Butterfly_Uncertainty_List[[4]]/Butterfly_Uncertainty_List[[1]],
  Butterfly_Uncertainty_List[[5]]/Butterfly_Uncertainty_List[[1]],
  Butterfly_Uncertainty_List[[6]]/Butterfly_Uncertainty_List[[1]]
)

reps <- (nrow(Butterfly_Uncertainty_List[[1]])*(nrow(Butterfly_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Butterfly_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Butterfly_Uncertainty_List_Adj[[1]])[upper.tri(unlist(Butterfly_Uncertainty_List_Adj[[1]]))],
                          unlist(Butterfly_Uncertainty_List_Adj[[2]])[upper.tri(unlist(Butterfly_Uncertainty_List_Adj[[2]]))],
                          unlist(Butterfly_Uncertainty_List_Adj[[3]])[upper.tri(unlist(Butterfly_Uncertainty_List_Adj[[3]]))],
                          unlist(Butterfly_Uncertainty_List_Adj[[4]])[upper.tri(unlist(Butterfly_Uncertainty_List_Adj[[4]]))],
                          unlist(Butterfly_Uncertainty_List_Adj[[5]])[upper.tri(unlist(Butterfly_Uncertainty_List_Adj[[5]]))],
                          unlist(Butterfly_Uncertainty_List_Adj[[6]])[upper.tri(unlist(Butterfly_Uncertainty_List_Adj[[6]]))]),
             dataset = rep("Butterflies", reps*6))

#Uncertainty_Rho_Butterfly_Adj <- Uncertainty_Rho_Butterfly_Adj[!is.na(Uncertainty_Rho_Butterfly_Adj$strength),]
Uncertainty_Rho_Butterfly_Adj$model <- factor(Uncertainty_Rho_Butterfly_Adj$model,
                                              levels = c("MPR","HPR","LPR",
                                                         "DPR","HLR-S","HLR-NS"))

### Eucalypts ----

# Normal

reps <- (nrow(Eucalypt_Uncertainty_List[[1]])*(nrow(Eucalypt_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Eucalypt <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Eucalypt_Uncertainty_List[[1]])[upper.tri(unlist(Eucalypt_Uncertainty_List[[1]]))],
                          unlist(Eucalypt_Uncertainty_List[[2]])[upper.tri(unlist(Eucalypt_Uncertainty_List[[2]]))],
                          unlist(Eucalypt_Uncertainty_List[[3]])[upper.tri(unlist(Eucalypt_Uncertainty_List[[3]]))],
                          unlist(Eucalypt_Uncertainty_List[[4]])[upper.tri(unlist(Eucalypt_Uncertainty_List[[4]]))],
                          unlist(Eucalypt_Uncertainty_List[[5]])[upper.tri(unlist(Eucalypt_Uncertainty_List[[5]]))],
                          unlist(Eucalypt_Uncertainty_List[[6]])[upper.tri(unlist(Eucalypt_Uncertainty_List[[6]]))]),
             dataset = rep("Eucalypts", reps*6))

#Uncertainty_Rho_Eucalypt <- Uncertainty_Rho_Eucalypt[!is.na(Uncertainty_Rho_Eucalypt$strength),]
Uncertainty_Rho_Eucalypt$model <- factor(Uncertainty_Rho_Eucalypt$model,
                                         levels = c("MPR","HPR","LPR",
                                                    "DPR","HLR-S","HLR-NS"))
# Adjust to relative values

Eucalypt_Uncertainty_List_Adj <- list(
  Eucalypt_Uncertainty_List[[1]]/Eucalypt_Uncertainty_List[[1]],
  Eucalypt_Uncertainty_List[[2]]/Eucalypt_Uncertainty_List[[1]],
  Eucalypt_Uncertainty_List[[3]]/Eucalypt_Uncertainty_List[[1]],
  Eucalypt_Uncertainty_List[[4]]/Eucalypt_Uncertainty_List[[1]],
  Eucalypt_Uncertainty_List[[5]]/Eucalypt_Uncertainty_List[[1]],
  Eucalypt_Uncertainty_List[[6]]/Eucalypt_Uncertainty_List[[1]]
)

reps <- (nrow(Eucalypt_Uncertainty_List[[1]])*(nrow(Eucalypt_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Eucalypt_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Eucalypt_Uncertainty_List_Adj[[1]])[upper.tri(unlist(Eucalypt_Uncertainty_List_Adj[[1]]))],
                          unlist(Eucalypt_Uncertainty_List_Adj[[2]])[upper.tri(unlist(Eucalypt_Uncertainty_List_Adj[[2]]))],
                          unlist(Eucalypt_Uncertainty_List_Adj[[3]])[upper.tri(unlist(Eucalypt_Uncertainty_List_Adj[[3]]))],
                          unlist(Eucalypt_Uncertainty_List_Adj[[4]])[upper.tri(unlist(Eucalypt_Uncertainty_List_Adj[[4]]))],
                          unlist(Eucalypt_Uncertainty_List_Adj[[5]])[upper.tri(unlist(Eucalypt_Uncertainty_List_Adj[[5]]))],
                          unlist(Eucalypt_Uncertainty_List_Adj[[6]])[upper.tri(unlist(Eucalypt_Uncertainty_List_Adj[[6]]))]),
             dataset = rep("Eucalypts", reps*6))

#Uncertainty_Rho_Eucalypt_Adj <- Uncertainty_Rho_Eucalypt_Adj[!is.na(Uncertainty_Rho_Eucalypt_Adj$strength),]
Uncertainty_Rho_Eucalypt_Adj$model <- factor(Uncertainty_Rho_Eucalypt_Adj$model,
                                             levels = c("MPR","HPR","LPR",
                                                        "DPR","HLR-S","HLR-NS"))

### Frogs ----

# Normal

reps <- (nrow(Frog_Uncertainty_List[[1]])*(nrow(Frog_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Frog <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Frog_Uncertainty_List[[1]])[upper.tri(unlist(Frog_Uncertainty_List[[1]]))],
                          unlist(Frog_Uncertainty_List[[2]])[upper.tri(unlist(Frog_Uncertainty_List[[2]]))],
                          unlist(Frog_Uncertainty_List[[3]])[upper.tri(unlist(Frog_Uncertainty_List[[3]]))],
                          unlist(Frog_Uncertainty_List[[4]])[upper.tri(unlist(Frog_Uncertainty_List[[4]]))],
                          unlist(Frog_Uncertainty_List[[5]])[upper.tri(unlist(Frog_Uncertainty_List[[5]]))],
                          unlist(Frog_Uncertainty_List[[6]])[upper.tri(unlist(Frog_Uncertainty_List[[6]]))]),
             dataset = rep("Frogs", reps*6))

#Uncertainty_Rho_Frog <- Uncertainty_Rho_Frog[!is.na(Uncertainty_Rho_Frog$strength),]
Uncertainty_Rho_Frog$model <- factor(Uncertainty_Rho_Frog$model,
                                     levels = c("MPR","HPR","LPR",
                                                "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Frog_Uncertainty_List_Adj <- list(
  Frog_Uncertainty_List[[1]]/Frog_Uncertainty_List[[1]],
  Frog_Uncertainty_List[[2]]/Frog_Uncertainty_List[[1]],
  Frog_Uncertainty_List[[3]]/Frog_Uncertainty_List[[1]],
  Frog_Uncertainty_List[[4]]/Frog_Uncertainty_List[[1]],
  Frog_Uncertainty_List[[5]]/Frog_Uncertainty_List[[1]],
  Frog_Uncertainty_List[[6]]/Frog_Uncertainty_List[[1]]
)

reps <- (nrow(Frog_Uncertainty_List[[1]])*(nrow(Frog_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Frog_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Frog_Uncertainty_List_Adj[[1]])[upper.tri(unlist(Frog_Uncertainty_List_Adj[[1]]))],
                          unlist(Frog_Uncertainty_List_Adj[[2]])[upper.tri(unlist(Frog_Uncertainty_List_Adj[[2]]))],
                          unlist(Frog_Uncertainty_List_Adj[[3]])[upper.tri(unlist(Frog_Uncertainty_List_Adj[[3]]))],
                          unlist(Frog_Uncertainty_List_Adj[[4]])[upper.tri(unlist(Frog_Uncertainty_List_Adj[[4]]))],
                          unlist(Frog_Uncertainty_List_Adj[[5]])[upper.tri(unlist(Frog_Uncertainty_List_Adj[[5]]))],
                          unlist(Frog_Uncertainty_List_Adj[[6]])[upper.tri(unlist(Frog_Uncertainty_List_Adj[[6]]))]),
             dataset = rep("Frogs", reps*6))

#Uncertainty_Rho_Frog_Adj <- Uncertainty_Rho_Frog_Adj[!is.na(Uncertainty_Rho_Frog_Adj$strength),]
Uncertainty_Rho_Frog_Adj$model <- factor(Uncertainty_Rho_Frog_Adj$model,
                                         levels = c("MPR","HPR","LPR",
                                                    "DPR","HLR-S","HLR-NS"))

### Fungi ----

# Normal

reps <- (nrow(Fungi_Uncertainty_List[[1]])*(nrow(Fungi_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Fungi <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Fungi_Uncertainty_List[[1]])[upper.tri(unlist(Fungi_Uncertainty_List[[1]]))],
                          unlist(Fungi_Uncertainty_List[[2]])[upper.tri(unlist(Fungi_Uncertainty_List[[2]]))],
                          unlist(Fungi_Uncertainty_List[[3]])[upper.tri(unlist(Fungi_Uncertainty_List[[3]]))],
                          unlist(Fungi_Uncertainty_List[[4]])[upper.tri(unlist(Fungi_Uncertainty_List[[4]]))],
                          unlist(Fungi_Uncertainty_List[[5]])[upper.tri(unlist(Fungi_Uncertainty_List[[5]]))],
                          unlist(Fungi_Uncertainty_List[[6]])[upper.tri(unlist(Fungi_Uncertainty_List[[6]]))]),
             dataset = rep("Fungi", reps*6))

#Uncertainty_Rho_Fungi <- Uncertainty_Rho_Fungi[!is.na(Uncertainty_Rho_Fungi$strength),]
Uncertainty_Rho_Fungi$model <- factor(Uncertainty_Rho_Fungi$model,
                                      levels = c("MPR","HPR","LPR",
                                                 "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Fungi_Uncertainty_List_Adj <- list(
  Fungi_Uncertainty_List[[1]]/Fungi_Uncertainty_List[[1]],
  Fungi_Uncertainty_List[[2]]/Fungi_Uncertainty_List[[1]],
  Fungi_Uncertainty_List[[3]]/Fungi_Uncertainty_List[[1]],
  Fungi_Uncertainty_List[[4]]/Fungi_Uncertainty_List[[1]],
  Fungi_Uncertainty_List[[5]]/Fungi_Uncertainty_List[[1]],
  Fungi_Uncertainty_List[[6]]/Fungi_Uncertainty_List[[1]]
)

reps <- (nrow(Fungi_Uncertainty_List[[1]])*(nrow(Fungi_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Fungi_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Fungi_Uncertainty_List_Adj[[1]])[upper.tri(unlist(Fungi_Uncertainty_List_Adj[[1]]))],
                          unlist(Fungi_Uncertainty_List_Adj[[2]])[upper.tri(unlist(Fungi_Uncertainty_List_Adj[[2]]))],
                          unlist(Fungi_Uncertainty_List_Adj[[3]])[upper.tri(unlist(Fungi_Uncertainty_List_Adj[[3]]))],
                          unlist(Fungi_Uncertainty_List_Adj[[4]])[upper.tri(unlist(Fungi_Uncertainty_List_Adj[[4]]))],
                          unlist(Fungi_Uncertainty_List_Adj[[5]])[upper.tri(unlist(Fungi_Uncertainty_List_Adj[[5]]))],
                          unlist(Fungi_Uncertainty_List_Adj[[6]])[upper.tri(unlist(Fungi_Uncertainty_List_Adj[[6]]))]),
             dataset = rep("Fungi", reps*6))

#Uncertainty_Rho_Fungi_Adj <- Uncertainty_Rho_Fungi_Adj[!is.na(Uncertainty_Rho_Fungi_Adj$strength),]
Uncertainty_Rho_Fungi_Adj$model <- factor(Uncertainty_Rho_Fungi_Adj$model,
                                          levels = c("MPR","HPR","LPR",
                                                     "DPR","HLR-S","HLR-NS"))

### Mosquitos ----

# Normal

reps <- (nrow(Mosquito_Uncertainty_List[[1]])*(nrow(Mosquito_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Mosquito <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Mosquito_Uncertainty_List[[1]])[upper.tri(unlist(Mosquito_Uncertainty_List[[1]]))],
                          unlist(Mosquito_Uncertainty_List[[2]])[upper.tri(unlist(Mosquito_Uncertainty_List[[2]]))],
                          unlist(Mosquito_Uncertainty_List[[3]])[upper.tri(unlist(Mosquito_Uncertainty_List[[3]]))],
                          unlist(Mosquito_Uncertainty_List[[4]])[upper.tri(unlist(Mosquito_Uncertainty_List[[4]]))],
                          unlist(Mosquito_Uncertainty_List[[5]])[upper.tri(unlist(Mosquito_Uncertainty_List[[5]]))],
                          unlist(Mosquito_Uncertainty_List[[6]])[upper.tri(unlist(Mosquito_Uncertainty_List[[6]]))]),
             dataset = rep("Mosquitos", reps*6))

#Uncertainty_Rho_Mosquito <- Uncertainty_Rho_Mosquito[!is.na(Uncertainty_Rho_Mosquito$strength),]
Uncertainty_Rho_Mosquito$model <- factor(Uncertainty_Rho_Mosquito$model,
                                         levels = c("MPR","HPR","LPR",
                                                    "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Mosquito_Uncertainty_List_Adj <- list(
  Mosquito_Uncertainty_List[[1]]/Mosquito_Uncertainty_List[[1]],
  Mosquito_Uncertainty_List[[2]]/Mosquito_Uncertainty_List[[1]],
  Mosquito_Uncertainty_List[[3]]/Mosquito_Uncertainty_List[[1]],
  Mosquito_Uncertainty_List[[4]]/Mosquito_Uncertainty_List[[1]],
  Mosquito_Uncertainty_List[[5]]/Mosquito_Uncertainty_List[[1]],
  Mosquito_Uncertainty_List[[6]]/Mosquito_Uncertainty_List[[1]]
)

reps <- (nrow(Mosquito_Uncertainty_List[[1]])*(nrow(Mosquito_Uncertainty_List[[1]])-1))/2

Uncertainty_Rho_Mosquito_Adj <- 
  data.frame(model = c(rep("MPR", reps),
                       rep("HPR", reps),
                       rep("LPR", reps),
                       rep("DPR", reps),
                       rep("HLR-S", reps),
                       rep("HLR-NS", reps)),
             strength = c(unlist(Mosquito_Uncertainty_List_Adj[[1]])[upper.tri(unlist(Mosquito_Uncertainty_List_Adj[[1]]))],
                          unlist(Mosquito_Uncertainty_List_Adj[[2]])[upper.tri(unlist(Mosquito_Uncertainty_List_Adj[[2]]))],
                          unlist(Mosquito_Uncertainty_List_Adj[[3]])[upper.tri(unlist(Mosquito_Uncertainty_List_Adj[[3]]))],
                          unlist(Mosquito_Uncertainty_List_Adj[[4]])[upper.tri(unlist(Mosquito_Uncertainty_List_Adj[[4]]))],
                          unlist(Mosquito_Uncertainty_List_Adj[[5]])[upper.tri(unlist(Mosquito_Uncertainty_List_Adj[[5]]))],
                          unlist(Mosquito_Uncertainty_List_Adj[[6]])[upper.tri(unlist(Mosquito_Uncertainty_List_Adj[[6]]))]),
             dataset = rep("Mosquitos", reps*6))

#Uncertainty_Rho_Mosquito_Adj <- Uncertainty_Rho_Mosquito_Adj[!is.na(Uncertainty_Rho_Mosquito_Adj$strength),]
Uncertainty_Rho_Mosquito_Adj$model <- factor(Uncertainty_Rho_Mosquito_Adj$model,
                                             levels = c("MPR","HPR","LPR",
                                                        "DPR","HLR-S","HLR-NS"))
### All Datsets ----

# Normal

Uncertainty_Rho_All <- rbind(Uncertainty_Rho_Birds,
                             Uncertainty_Rho_Butterfly,
                             Uncertainty_Rho_Eucalypt,
                             Uncertainty_Rho_Frog,
                             Uncertainty_Rho_Fungi,
                             Uncertainty_Rho_Mosquito)

Uncertainty_Rho_All$model <- factor(Uncertainty_Rho_All$model,
                                    levels = c("MPR","HPR","LPR",
                                               "DPR","HLR-S","HLR-NS"))

# Adjust to relative values

Uncertainty_Rho_All_Adj <- rbind(Uncertainty_Rho_Birds_Adj,
                                 Uncertainty_Rho_Butterfly_Adj,
                                 Uncertainty_Rho_Eucalypt_Adj,
                                 Uncertainty_Rho_Frog_Adj,
                                 Uncertainty_Rho_Fungi_Adj,
                                 Uncertainty_Rho_Mosquito_Adj)

Uncertainty_Rho_All_Adj$model <- factor(Uncertainty_Rho_All_Adj$model,
                                        levels = c("MPR","HPR","LPR",
                                                   "DPR","HLR-S","HLR-NS"))

### ----


## set colour scale

col_palette <- brewer.pal(6, "Dark2")

## need to convert to correct dataframe format for ggplot

### All_Datasets ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(All_Datasets_Uncertainty_Metric))){
  
  Model_tmp <- rep(rownames(All_Datasets_Uncertainty_Metric)[i],
                   ncol(All_Datasets_Uncertainty_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(All_Datasets_Uncertainty_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- All_Datasets_Uncertainty_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

All_Datasets_Uncertainty_ggplot_df <- data.frame(Model = Model,
                                                 Metric = Metric,
                                                 Value = Value)

All_Datasets_Uncertainty_ggplot_df$Model <- factor(All_Datasets_Uncertainty_ggplot_df$Model,
                                                   levels = c("MPR","HPR","LPR",
                                                              "DPR","HLR-S","HLR-NS"))
### Birds ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Bird_Uncertainty_Metric))){
  
  Model_tmp <- rep(rownames(Bird_Uncertainty_Metric)[i],
                   ncol(Bird_Uncertainty_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Bird_Uncertainty_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Bird_Uncertainty_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Birds_Uncertainty_ggplot_df <- data.frame(Model = Model,
                                          Metric = Metric,
                                          Value = Value)

Birds_Uncertainty_ggplot_df$Model <- factor(Birds_Uncertainty_ggplot_df$Model,
                                            levels = c("MPR","HPR","LPR","DPR",
                                                       "HLR-S","HLR-NS"))

### Butterflies ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Butterfly_Uncertainty_Metric))){
  
  Model_tmp <- rep(rownames(Butterfly_Uncertainty_Metric)[i],
                   ncol(Butterfly_Uncertainty_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Butterfly_Uncertainty_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Butterfly_Uncertainty_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Butterflies_Uncertainty_ggplot_df <- data.frame(Model = Model,
                                                Metric = Metric,
                                                Value = Value)

Butterflies_Uncertainty_ggplot_df$Model <- factor(Butterflies_Uncertainty_ggplot_df$Model,
                                                  levels = c("MPR","HPR","LPR","DPR",
                                                             "HLR-S","HLR-NS"))

### Eucalypts ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Eucalypt_Uncertainty_Metric))){
  
  Model_tmp <- rep(rownames(Eucalypt_Uncertainty_Metric)[i],
                   ncol(Eucalypt_Uncertainty_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Eucalypt_Uncertainty_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Eucalypt_Uncertainty_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Eucalypts_Uncertainty_ggplot_df <- data.frame(Model = Model,
                                              Metric = Metric,
                                              Value = Value)

Eucalypts_Uncertainty_ggplot_df$Model <- factor(Eucalypts_Uncertainty_ggplot_df$Model,
                                                levels = c("MPR","HPR","LPR",
                                                           "DPR","HLR-S","HLR-NS"))

### Frogs ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Frog_Uncertainty_Metric))){
  
  Model_tmp <- rep(rownames(Frog_Uncertainty_Metric)[i],
                   ncol(Frog_Uncertainty_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Frog_Uncertainty_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Frog_Uncertainty_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Frogs_Uncertainty_ggplot_df <- data.frame(Model = Model,
                                          Metric = Metric,
                                          Value = Value)

Frogs_Uncertainty_ggplot_df$Model <- factor(Frogs_Uncertainty_ggplot_df$Model,
                                            levels = c("MPR","HPR","LPR","DPR",
                                                       "HLR-S","HLR-NS"))

### Fungi ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Fungi_Uncertainty_Metric))){
  
  Model_tmp <- rep(rownames(Fungi_Uncertainty_Metric)[i],
                   ncol(Fungi_Uncertainty_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Fungi_Uncertainty_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Fungi_Uncertainty_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Fungi_Uncertainty_ggplot_df <- data.frame(Model = Model,
                                          Metric = Metric,
                                          Value = Value)

Fungi_Uncertainty_ggplot_df$Model <- factor(Fungi_Uncertainty_ggplot_df$Model,
                                            levels = c("MPR","HPR","LPR",
                                                       "DPR","HLR-S","HLR-NS"))

### Mosquitos ----

Model <- c()
Metric <- c()
Value <- c()

for(i in seq(nrow(Mosquito_Uncertainty_Metric))){
  
  Model_tmp <- rep(rownames(Mosquito_Uncertainty_Metric)[i],
                   ncol(Mosquito_Uncertainty_Metric))
  Model <- c(Model, Model_tmp)
  
  Metric_tmp <- colnames(Mosquito_Uncertainty_Metric)
  Metric <- c(Metric, Metric_tmp)
  
  Value_tmp <- Mosquito_Uncertainty_Metric[i,]
  Value <- c(Value, Value_tmp)
  
}

Mosquito_Uncertainty_ggplot_df <- data.frame(Model = Model,
                                             Metric = Metric,
                                             Value = Value)

Mosquito_Uncertainty_ggplot_df$Model <- factor(Mosquito_Uncertainty_ggplot_df$Model,
                                               levels = c("MPR","HPR","LPR","DPR",
                                                          "HLR-S","HLR-NS"))

# ----

## ggplot barplots

### All_Datasets ----

colour <- autoColourScheme(All_Datasets_Uncertainty_ggplot_df)

ggplot(All_Datasets_Uncertainty_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Uncertainty") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Uncertainty_Barplot_All_Datasets.pdf", 
       width = 8, height = 5, units = "in")

### Birds ----

colour <- autoColourScheme(Birds_Uncertainty_ggplot_df)

ggplot(Birds_Uncertainty_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Uncertainty") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Uncertainty_Barplot_Birds.pdf", 
       width = 8, height = 5, units = "in")

### Butterflies ----

colour <- autoColourScheme(Butterflies_Uncertainty_ggplot_df)

ggplot(Butterflies_Uncertainty_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Uncertainty") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Uncertainty_Barplot_Butterflies.pdf", 
       width = 8, height = 5, units = "in")

### Eucalypts ----

colour <- autoColourScheme(Eucalypts_Uncertainty_ggplot_df)

ggplot(Eucalypts_Uncertainty_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Uncertainty") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Uncertainty_Barplot_Eucalypts.pdf", 
       width = 8, height = 5, units = "in")

### Frogs ----

colour <- autoColourScheme(Frogs_Uncertainty_ggplot_df)

ggplot(Frogs_Uncertainty_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Uncertainty") +
  facet_wrap(~Model) + 
  theme_bw() + 
  theme(legend.position = "none")

ggsave("Uncertainty_Barplot_Frogs.pdf", 
       width = 8, height = 5, units = "in")

### Fungi ----

colour <- autoColourScheme(Fungi_Uncertainty_ggplot_df)

ggplot(Fungi_Uncertainty_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Uncertainty") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Uncertainty_Barplot_Fungi.pdf", 
       width = 8, height = 5, units = "in")

### Mosquitos ----

colour <- autoColourScheme(Mosquito_Uncertainty_ggplot_df)

ggplot(Mosquito_Uncertainty_ggplot_df, aes(Metric, Value)) +
  geom_bar(stat = "identity",
           aes(fill = Model),
           position = "dodge") +
  scale_fill_manual(values = colour) +
  ylab("Proportion") +
  xlab("Correlation Uncertainty") +
  facet_wrap(~Model) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Uncertainty_Barplot_Mosquitos.pdf", 
       width = 8, height = 5, units = "in")

# ----

## ggplot boxplots

### Birds ----

colour <- autoColourScheme(Uncertainty_Rho_Birds[!is.na(Uncertainty_Rho_Birds$strength), ])

ggplot(Uncertainty_Rho_Birds, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,0.75))
ggsave("Uncertainty_Rho_Birds_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Uncertainty_Rho_Birds_Adj[Uncertainty_Rho_Birds_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,2.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Uncertainty_Adj_Rho_Birds_boxplot.pdf", units = "in", width = 7, height = 7)

### Butterflies ----

colour <- autoColourScheme(Uncertainty_Rho_Butterfly[!is.na(Uncertainty_Rho_Butterfly$strength), ])

ggplot(Uncertainty_Rho_Butterfly, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,0.5))
ggsave("Uncertainty_Rho_Butterflies_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Uncertainty_Rho_Butterfly_Adj[Uncertainty_Rho_Butterfly_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,1.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Uncertainty_Adj_Rho_Butterflies_boxplot.pdf", units = "in", width = 7, height = 7)

### Eucalypts ----

colour <- autoColourScheme(Uncertainty_Rho_Eucalypt[!is.na(Uncertainty_Rho_Eucalypt$strength), ])

ggplot(Uncertainty_Rho_Eucalypt, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,1.5))
ggsave("Uncertainty_Rho_Eucalypts_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Uncertainty_Rho_Eucalypt_Adj[Uncertainty_Rho_Eucalypt_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,2)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Uncertainty_Adj_Rho_Eucalypts_boxplot.pdf", units = "in", width = 7, height = 7)

### Frogs ----

colour <- autoColourScheme(Uncertainty_Rho_Frog[!is.na(Uncertainty_Rho_Frog$strength), ])

ggplot(Uncertainty_Rho_Frog, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,1.5))
ggsave("Uncertainty_Rho_Frogs_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Uncertainty_Rho_Frog_Adj[Uncertainty_Rho_Frog_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,2)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Uncertainty_Adj_Rho_Frogs_boxplot.pdf", units = "in", width = 7, height = 7)

### Fungi ----

colour <- autoColourScheme(Uncertainty_Rho_Fungi[!is.na(Uncertainty_Rho_Fungi$strength), ])

ggplot(Uncertainty_Rho_Fungi, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,1))
ggsave("Uncertainty_Rho_Fungi_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Uncertainty_Rho_Fungi_Adj[Uncertainty_Rho_Fungi_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,1.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Uncertainty_Adj_Rho_Fungi_boxplot.pdf", units = "in", width = 7, height = 7)

### Mosquitos ----

colour <- autoColourScheme(Uncertainty_Rho_Mosquito[!is.na(Uncertainty_Rho_Mosquito$strength), ])

ggplot(Uncertainty_Rho_Mosquito, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,1))
ggsave("Uncertainty_Rho_Mosquitos_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Uncertainty_Rho_Mosquito_Adj[Uncertainty_Rho_Mosquito_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,2)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Uncertainty_Adj_Rho_Mosquitos_boxplot.pdf", units = "in", width = 7, height = 7)

### All Datasets ----

# Actually all

colour <- autoColourScheme(Uncertainty_Rho_All[!is.na(Uncertainty_Rho_All$strength), ])

ggplot(Uncertainty_Rho_All, aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,1.25))
ggsave("Uncertainty_Rho_All_boxplot.pdf", units = "in", width = 7, height = 7)

ggplot(Uncertainty_Rho_All_Adj[Uncertainty_Rho_All_Adj$model != "MPR", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,2.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")
ggsave("Uncertainty_Adj_Rho_All_boxplot.pdf", units = "in", width = 7, height = 7)

# Smallest four

colour <- autoColourScheme(Uncertainty_Rho_All[!is.na(Uncertainty_Rho_All$strength), ])

ggplot(Uncertainty_Rho_All[Uncertainty_Rho_All$dataset != "Birds" &
                             Uncertainty_Rho_All$dataset != "Butterflies",],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,1.25))
ggsave("Uncertainty_Rho_Small4_boxplot.pdf", units = "in", width = 7, height = 7)

small4RelUnc <- ggplot(Uncertainty_Rho_All_Adj[Uncertainty_Rho_All_Adj$model != "MPR" &
                                 Uncertainty_Rho_All_Adj$dataset != "Birds" &
                                 Uncertainty_Rho_All_Adj$dataset != "Butterflies", ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.text = element_text(colour = "black")) +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,2.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")

small4RelUnc
ggsave("Uncertainty_Adj_Rho_Small4_boxplot.pdf", units = "in", width = 7, height = 7)

# Largest two

colour <- autoColourScheme(Uncertainty_Rho_All[!is.na(Uncertainty_Rho_All$strength) &
                                                 Uncertainty_Rho_All$dataset %in% c("Birds","Butterflies"), ])

ggplot(Uncertainty_Rho_All[Uncertainty_Rho_All$dataset %in% c("Birds","Butterflies"), ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour,
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  labs(x = "Model",
       y = "Uncertainty (95% credible interval width)") +
  coord_cartesian(ylim = c(0,0.75))
ggsave("Uncertainty_Rho_Large2_boxplot.pdf", units = "in", width = 7, height = 7)

big2RelUnc <- ggplot(Uncertainty_Rho_All_Adj[Uncertainty_Rho_All_Adj$model != "MPR" &
                                 Uncertainty_Rho_All_Adj$dataset %in% c("Birds", "Butterflies"), ],
       aes(model, abs(strength), fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(breaks = model, values = colour[2:length(colour)],
                    name = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",
        axis.text = element_text(colour = "black")) +
  labs(x = "Model",
       y = "Relative uncertainty") +
  coord_cartesian(ylim = c(0,2.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed")

big2RelUnc

ggsave("Uncertainty_Adj_Rho_Large2_boxplot.pdf", units = "in", width = 7, height = 7)

plot_grid(small4RelUnc, big2RelUnc, align = "h", labels = "auto")
ggsave(filename = "RelUncSideBySide.pdf", width = 8, height = 4, units = "in", dpi = 600)
### ----