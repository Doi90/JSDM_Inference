############################
###### Load Packages #######
############################

library(ggplot2)
library(RColorBrewer)
library(R.utils)

############################
####### Load Script ########
############################

source("modelNameChange.R")

############################
####### Read in data #######
############################

BayesComm.df <- read.csv("Beta_df_Eucalypt_BayesComm.csv")
BayesComm.df <- BayesComm.df[,-c(1,6:10)]
Pollock.df <- read.csv("Beta_Eucalypt_Pollock.csv")
Pollock.df <- Pollock.df[,-c(1,6:10)]
Ovaskainen.df <- read.csv("Beta_Eucalypt_Ovaskainen2016.csv")
Ovaskainen.df <- Ovaskainen.df[,-c(1,6:10)]
OvaskainenNS.df <- read.csv("Beta_Eucalypt_Ovaskainen2016NS.csv")
OvaskainenNS.df <- OvaskainenNS.df[,-c(1,6:10)]
boral.df <- read.csv("Beta_Eucalypt_boral.csv")
boral.df <- boral.df[,-c(1,6:10)]
Clark.df <- read.csv("Beta_Eucalypts_Clark.csv")
Clark.df <- Clark.df[,-c(1,6:10)]

############################
##### Merge data frames ####
############################

df <- rbind(BayesComm.df, boral.df, Clark.df, Ovaskainen.df, OvaskainenNS.df, Pollock.df)
df <- modelNameChange(df)
df$species <- gsub("\\_", " ", df$species) # Convert species names to same format [Crinia.sig -> Crinia_sig]
df$species <- gsub("\\.", " ", df$species) # Convert species names to same format [Crinia.sig -> Crinia_sig]
df$species <- gsub("\\-", " ", df$species) # Convert species names to same format [Crinia.sig -> Crinia_sig]
df$species <- gsub(" s ", "'s ", df$species) # Convert species names to same format [Crinia.sig -> Crinia_sig]
df$species <- capitalize(df$species)
species.num <- length(unique(BayesComm.df$species))
df$coefficient <- rep(c("Intercept", "Rockiness", "Sandiness", "Valley Bottom Flat", "Annual Precipitation",
                        "Loaminess", "cvTemp", "T0"),species.num)
colnames(df) <- c("Coefficient", "Posterior.Mean", "Lower", "Upper", "Model", "Species")

#############################
#### Change Factor Levels ###
#############################

df$Model <- factor(df$Model, levels = rev(c("MPR","HPR","LPR","DPR","HLR-S","HLR-NS")))
# levels(df$Model) <- c("MPR","LPR","DPR","HLR-NS")
#df$Model <- factor(df$Model, levels = rev(levels(df$Model)))
df$Coefficient <- factor(df$Coefficient, levels = c("Intercept", "Annual Precipitation",
                                                    "cvTemp", "Loaminess","Rockiness",
                                                    "Sandiness", "T0", "Valley Bottom Flat"))

############################
##### Set Colour Scale #####
############################

model <- unique(df$Model)
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

colour_inverse <- rev(colour)   # because coord_flip() 

############################
######## Make JPEGs ########
############################

dodge <- position_dodge(width=0.5)  

for(i in unique(df$Species)){
  
  # Create blank PNG file
  
  file.name <- paste("Beta_", i, ".pdf", sep = "")
  
  # Create plot
  
  ggplot(df[df$Species == i,],aes(x = Coefficient, y = Posterior.Mean,
                                  colour = Model)) + 
    geom_point(position=dodge) +
    geom_errorbar(aes(ymax=Upper,ymin=Lower),position = dodge) +
    theme(legend.position = "right") +
    ylab("Posterior Mean") + 
    xlab("Variable") +
    coord_flip() +
    ggtitle(i) +
    scale_colour_manual(values = colour_inverse, breaks = rev(levels(df$Model))) +
    scale_x_discrete(name = "", limits = rev(levels(df$Coefficient))) +
    theme_bw()
  
  ggsave(file.name, units = "in", width = 7, height = 7)
}
