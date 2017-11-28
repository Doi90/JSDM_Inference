library(corrplot)
library(abind)
library(ggplot2)

model_names <- c("BayesComm", "boral", "GJAM", "Ovaskainen 2016", "Ovaskainen 2016 NS", "Pollock")

##########################################
### Correlation Between Beta Estimates ###
##########################################

## Frogs

Frog_list <- list(BC_Frog$posterior.mean, boral_Frog$posterior.mean, Clark_Frog$posterior.mean,
                  Ov_Frog$posterior.mean, OvNS_Frog$posterior.mean, Pol_Frog$posterior.mean)

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

corrplot(corr_Frog_matrix, type = "upper", method = "color", diag = FALSE)
corrplot(corr_Frog_matrix, type = "upper", method = "number", diag = FALSE)

## Eucalypts

Eucalypt_list <- list(BC_Euc$posterior.mean, boral_Euc$posterior.mean, Clark_Euc$posterior.mean,
                       Ov_Euc$posterior.mean, OvNS_Euc$posterior.mean, Pol_Euc$posterior.mean)

corr_Eucalypt_vector <- c()

for(i in 1:(length(Eucalypt_list)-1)){
  for(j in (i+1):length(Eucalypt_list)){
    tmp <- cor(Eucalypt_list[[i]],Eucalypt_list[[j]])
    corr_Eucalypt_vector <- c(corr_Eucalypt_vector, tmp)
  } 
}

corr_Eucalypt_matrix <- diag(length(Eucalypt_list))
corr_Eucalypt_matrix[lower.tri(corr_Eucalypt_matrix, diag = FALSE)] <- corr_Eucalypt_vector
corr_Eucalypt_matrix <- t(corr_Eucalypt_matrix)
colnames(corr_Eucalypt_matrix) <- rownames(corr_Eucalypt_matrix) <- model_names

corrplot(corr_Eucalypt_matrix, type = "upper", method = "color", diag = FALSE)
corrplot(corr_Eucalypt_matrix, type = "upper", method = "number", diag = FALSE)

## Mosquito

Mosquito_list <- list(BC_Mosq$posterior.mean, boral_Mosq$posterior.mean, Clark_Mosq$posterior.mean,
                      Ov_Mosq$posterior.mean, OvNS_Mosq$posterior.mean, Pol_Mosq$posterior.mean)

corr_Mosquito_vector <- c()

for(i in 1:(length(Mosquito_list)-1)){
  for(j in (i+1):length(Mosquito_list)){
    tmp <- cor(Mosquito_list[[i]],Mosquito_list[[j]])
    corr_Mosquito_vector <- c(corr_Mosquito_vector, tmp)
  } 
}

corr_Mosquito_matrix <- diag(length(Mosquito_list))
corr_Mosquito_matrix[lower.tri(corr_Mosquito_matrix, diag = FALSE)] <- corr_Mosquito_vector
corr_Mosquito_matrix <- t(corr_Mosquito_matrix)
colnames(corr_Mosquito_matrix) <- rownames(corr_Mosquito_matrix) <- model_names

corrplot(corr_Mosquito_matrix, type = "upper", method = "color", diag = FALSE)
corrplot(corr_Mosquito_matrix, type = "upper", method = "number", diag = FALSE)



## Average Over All Datasets

corr_list <- list(corr_Frog_matrix, corr_Eucalypt_matrix, corr_Mosquito_matrix)
corr_array <- abind(corr_list, along = 3)
Avg_corr <- apply(corr_array, MARGIN = c(1,2), FUN = mean)

jpeg(filename = "Avg_corr_colour.jpg")
corrplot(Avg_corr, type = "upper", method = "color", diag = FALSE, addCoef.col = "black", tl.col="black", tl.srt=45)
dev.off()
jpeg(filename = "Avg_corr_number.jpg")
corrplot(Avg_corr, type = "upper", method = "number", diag = FALSE)
dev.off()

##############################################
### Comparing 95% Credible Interval Widths ###
##############################################

BC_Frog_Uncertainty <- BC_Frog$upper - BC_Frog$lower
mean(BC_Frog_Uncertainty)

Pol_Frog_Uncertainty <- Pol_Frog$upper - Pol_Frog$lower
mean(BC_Frog_Uncertainty)

test <- rbind(BC_Frog[,-6], boral_Frog[,-6], Clark_Frog[,-6],
              Ov_Frog, OvNS_Frog, Pol_Frog[,-6])

ggplot(test, aes(model,(upper-lower))) +
  geom_boxplot() +
  xlab("Model") +
  ylab("95% Credible Interval Width") +
  ggtitle("Regression Coefficient Uncertainty - Frogs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  

test2 <- rbind(BC_Euc[,-6], boral_Euc[,-6], Clark_Euc[,-6],
              Ov_Euc, OvNS_Euc, Pol_Euc[,-6])

ggplot(test2, aes(model,(upper-lower))) +
  geom_boxplot() +
  xlab("Model") +
  ylab("95% Credible Interval Width") +
  ggtitle("Regression Coefficient Uncertainty - Eucalypts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



test3 <- rbind(BC_Mosq[,-6], boral_Mosq[,-6], Clark_Mosq[,-6],
               Ov_Mosq, OvNS_Mosq, Pol_Mosq[,-6])

ggplot(test3, aes(model,(upper-lower))) +
  geom_boxplot() +
  xlab("Model") +
  ylab("95% Credible Interval Width") +
  ggtitle("Regression Coefficient Uncertainty - Mosquitos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
