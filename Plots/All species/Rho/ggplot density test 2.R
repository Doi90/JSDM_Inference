# Column 1 - Birds

x <- data.frame(x = length(unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)])), # BayesComm - Bird
                y = unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
BC.Birds <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "OrRd", guide="none") +
  ylim(0,2) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

Pol.Birds <- ggplot() + # Pollock - Bird
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2) +
  geom_vline(xintercept = 0, lwd = 1) +
  labs(x = NULL, y = NULL)

Ov.Birds <- ggplot() + # OvS - Bird
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2) +
  geom_vline(xintercept = 0, lwd = 1) +
  labs(x = NULL, y = NULL)

x <- data.frame(x = length(unlist(OvNS_Bird_mean[upper.tri(OvNS_Bird_mean)])), # OvNS - Bird
                y = unlist(OvNS_Bird_mean[upper.tri(OvNS_Bird_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
OvNS.Birds <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "RdPu", guide="none") +
  ylim(0,2) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

# Column 2 - Butterflies

x <- data.frame(x = length(unlist(BC_But_mean[upper.tri(BC_But_mean)])), # BayesComm - Butterflies
                y = unlist(BC_But_mean[upper.tri(BC_But_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
BC.Butterflies <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "OrRd", guide="none") +
  ylim(0,2) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

Pol.Butterflies <- ggplot() + # Pollock - Butterflies
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2) +
  geom_vline(xintercept = 0, lwd = 1) +
  labs(x = NULL, y = NULL)

Ov.Butterflies <- ggplot() + # OvS - Butterflies
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2) +
  geom_vline(xintercept = 0, lwd = 1) +
  labs(x = NULL, y = NULL)

x <- data.frame(x = length(unlist(OvNS_But_mean[upper.tri(OvNS_But_mean)])), # OvNS - Butterflies
                y = unlist(OvNS_But_mean[upper.tri(OvNS_But_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
OvNS.Butterflies <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "RdPu", guide="none") +
  ylim(0,2) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

# Column 3 - Eucalypts

x <- data.frame(x = length(unlist(BC_Euc_mean[upper.tri(BC_Euc_mean)])), # BayesComm - Eucalypts
                y = unlist(BC_Euc_mean[upper.tri(BC_Euc_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
BC.Eucalypts <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "OrRd", guide="none") +
  ylim(0,3.5) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(Pol_Euc_mean[upper.tri(Pol_Euc_mean)])), # Pollock - Eucalypts
                y = unlist(Pol_Euc_mean[upper.tri(Pol_Euc_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
Pol.Eucalypts <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "BuGn", guide="none") +
  ylim(0,3.5) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(Ov_Euc_mean[upper.tri(Ov_Euc_mean)])), # OvS - Eucalypts
                y = unlist(Ov_Euc_mean[upper.tri(Ov_Euc_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
Ov.Eucalypts <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "PuRd", guide="none") +
  ylim(0,3.5) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(OvNS_Euc_mean[upper.tri(OvNS_Euc_mean)])), # OvNS - Eucalypts
                y = unlist(OvNS_Euc_mean[upper.tri(OvNS_Euc_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
OvNS.Eucalypts <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "RdPu", guide="none") +
  ylim(0,3.5) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

# Column 4 - Frogs

x <- data.frame(x = length(unlist(BC_Frog_mean[upper.tri(BC_Frog_mean)])), # BayesComm - Frogs
                y = unlist(BC_Frog_mean[upper.tri(BC_Frog_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
BC.Frogs <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "OrRd", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(Pol_Frog_mean[upper.tri(Pol_Frog_mean)])), # Pollock - Frogs
                y = unlist(Pol_Frog_mean[upper.tri(Pol_Frog_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
Pol.Frogs <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "BuGn", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(Ov_Frog_mean[upper.tri(Ov_Frog_mean)])), # OvS - Frogs
                y = unlist(Ov_Frog_mean[upper.tri(Ov_Frog_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
Ov.Frogs <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "PuRd", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(OvNS_Frog_mean[upper.tri(OvNS_Frog_mean)])), # OvNS - Frogs
                y = unlist(OvNS_Frog_mean[upper.tri(OvNS_Frog_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
OvNS.Frogs <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "RdPu", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

# Column 5 - Fungi

x <- data.frame(x = length(unlist(BC_Fun_mean[upper.tri(BC_Fun_mean)])), # BayesComm - Fungi
                y = unlist(BC_Fun_mean[upper.tri(BC_Fun_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
BC.Fungi <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "OrRd", guide="none") +
  ylim(0,3.5) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(Pol_Fun_mean[upper.tri(Pol_Fun_mean)])), # Pollock - Fungi
                y = unlist(Pol_Fun_mean[upper.tri(Pol_Fun_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
Pol.Fungi <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "BuGn", guide="none") +
  ylim(0,3.5) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

Ov.Fungi <- ggplot() +   # OvS - Fungi
  theme_minimal() +
  ylim(0,3.5) +
  xlim(-1,1) +
  geom_vline(xintercept = 0, lwd = 1) +
  labs(x = NULL, y = NULL)

x <- data.frame(x = length(unlist(OvNS_Fun_mean[upper.tri(OvNS_Fun_mean)])), # OvNS - Fungi
                y = unlist(OvNS_Fun_mean[upper.tri(OvNS_Fun_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
OvNS.Fungi <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "RdPu", guide="none") +
  ylim(0,3.5) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

# Column 6 - Mosquito

x <- data.frame(x = length(unlist(BC_Mosq_mean[upper.tri(BC_Mosq_mean)])), # BayesComm - Mosquitos
                y = unlist(BC_Mosq_mean[upper.tri(BC_Mosq_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
BC.Mosquitos <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "OrRd", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(Pol_Mosq_mean[upper.tri(Pol_Mosq_mean)])), # Pollock - Mosquitos
                y = unlist(Pol_Mosq_mean[upper.tri(Pol_Mosq_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
Pol.Mosquitos <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "BuGn", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(Ov_Mosq_mean[upper.tri(Ov_Mosq_mean)])), # OvS - Mosquitos
                y = unlist(Ov_Mosq_mean[upper.tri(Ov_Mosq_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
Ov.Mosquitos <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "PuRd", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)

x <- data.frame(x = length(unlist(OvNS_Mosq_mean[upper.tri(OvNS_Mosq_mean)])), # OvNS - Mosquitos
                y = unlist(OvNS_Mosq_mean[upper.tri(OvNS_Mosq_mean)]))  
dens <- density(x$y, bw = 0.1)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0,0.25, 0.5, 0.75, 1)
quantiles <- quantile(x$y, probs = probs)
df$quant <- factor(findInterval(df$x,quantiles))
OvNS.Mosquitos <- ggplot(df, aes(x,y)) +
  theme_minimal() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  geom_line() +
  # scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(type = seq, palette = "RdPu", guide="none") +
  ylim(0,3) +
  xlim(-1,1) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = 0, lwd = 1)


# Multi-plot

library(gridExtra)

plot.col.model <- grid.arrange(BC.Birds, BC.Butterflies, BC.Eucalypts, BC.Frogs, BC.Fungi, BC.Mosquitos,
                               Pol.Birds, Pol.Butterflies, Pol.Eucalypts, Pol.Frogs, Pol.Fungi, Pol.Mosquitos,
                               Ov.Birds, Ov.Butterflies, Ov.Eucalypts, Ov.Frogs, Ov.Fungi, Ov.Mosquitos,
                               OvNS.Birds, OvNS.Butterflies, OvNS.Eucalypts, OvNS.Frogs, OvNS.Fungi, OvNS.Mosquitos,
                               ncol = 6) 