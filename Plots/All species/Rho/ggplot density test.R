# Column 1 - Birds

x <- data.frame(x = unlist(BC_Bird_mean[upper.tri(BC_Bird_mean)]))  # BayesComm - Bird
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
BC.Bird <- ggplot(x) +
  theme_minimal() + 
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "red4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "red4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

Pol.Bird <- ggplot() + # Pollock - Bird
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2)

Ov.Bird <- ggplot() + # OvS - Bird
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2)

x <- data.frame(x = unlist(OvNS_Bird_mean[upper.tri(OvNS_Bird_mean)]))  # OvNS - Bird
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
OvNS.Bird <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "darkorchid4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "darkorchid4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

# Column 2 - Butterflies

x <- data.frame(x = unlist(BC_But_mean[upper.tri(BC_But_mean)]))  # BayesComm - Butterflies
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
BC.Butterflies <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "red4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "red4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

Pol.Butterflies <- ggplot() + # Pollock - Butterflies
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2)

Ov.Butterflies <- ggplot() + # OvS - Butterflies
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,2)

x <- data.frame(x = unlist(OvNS_But_mean[upper.tri(OvNS_But_mean)]))  # OvNS - Butterflies
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
OvNS.Butterflies <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "darkorchid4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "darkorchid4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

# Column 3 - Eucalypts

x <- data.frame(x = unlist(BC_Euc_mean[upper.tri(BC_Euc_mean)]))  # BayesComm - Eucalypts
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
BC.Eucalypts <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "red4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "red4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(Pol_Euc_mean[upper.tri(Pol_Euc_mean)]))  # Pollock - Eucalypts
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
Pol.Eucalypts <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "lawngreen", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "lawngreen", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(Ov_Euc_mean[upper.tri(Ov_Euc_mean)]))  # Ov - Eucalypts
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
Ov.Eucalypts <- ggplot(x) +
  theme_minimal() +
  geom_area(data = subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "magenta1", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "magenta1", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(OvNS_Euc_mean[upper.tri(OvNS_Euc_mean)]))  # OvNS - Eucalypts
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
OvNS.Eucalypts <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "darkorchid4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "darkorchid4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

# Column 4 - Frogs

x <- data.frame(x = unlist(BC_Frog_mean[upper.tri(BC_Frog_mean)]))  # BayesComm - Frogs
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
BC.Frog <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "red4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "red4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(Pol_Frog_mean[upper.tri(Pol_Frog_mean)]))  # Pollock - Frogs
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
Pol.Frog <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "lawngreen", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "lawngreen", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(Ov_Frog_mean[upper.tri(Ov_Frog_mean)]))  # Ov - Frogs
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
Ov.Frog <- ggplot(x) +
  theme_minimal() +
  geom_area(data = subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "magenta1", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "magenta1", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(OvNS_Frog_mean[upper.tri(OvNS_Frog_mean)]))  # OvNS - Frogs
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
OvNS.Frog <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "darkorchid4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "darkorchid4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

# Column 5 - Fungi

x <- data.frame(x = unlist(BC_Fun_mean[upper.tri(BC_Fun_mean)]))  # BayesComm - Fungi
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
BC.Fungi <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "red4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "red4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(Pol_Fun_mean[upper.tri(Pol_Fun_mean)]))  # Pollock - Fungi
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
Pol.Fungi <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "lawngreen", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "lawngreen", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

Ov.Fungi <- ggplot(x) +  # Ov - Fungi
  theme_minimal() +
  xlim(-1,1) +
  ylim(0,3.5)

x <- data.frame(x = unlist(OvNS_Fun_mean[upper.tri(OvNS_Fun_mean)]))  # OvNS - Fungi
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
OvNS.Fungi <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "darkorchid4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "darkorchid4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

# Column 6 - Mosquito

x <- data.frame(x = unlist(BC_Mosq_mean[upper.tri(BC_Mosq_mean)]))  # BayesComm - Mosquito
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
BC.Mosquito <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "red4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "red4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(Pol_Mosq_mean[upper.tri(Pol_Mosq_mean)]))  # Pollock - Mosquito
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
Pol.Mosq <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "lawngreen", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "lawngreen", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(Ov_mosq_mean[upper.tri(Ov_Mosq_mean)]))  # Ov - Mosquito
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
Ov.Mosq <- ggplot(x) +
  theme_minimal() +
  geom_area(data = subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "magenta1", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "magenta1", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,3.5) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))

x <- data.frame(x = unlist(OvNS_Mosq_mean[upper.tri(OvNS_Mosq_mean)]))  # OvNS - Mosquito
q.05 <- quantile(x$x, 0.05)
q.95 <- quantile(x$x, 0.95)
x.dens <- density(x$x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
OvNS.Mosq <- ggplot(x) +
  theme_minimal() +
  geom_area(data =subset(df.dens, x > q.05 & x < q.95), aes(x=x, y=y), fill = "darkorchid4", alpha = 0.8) +
  geom_density(aes(x=x, y = ..density..), fill = "darkorchid4", alpha = 0.2) +
  xlim(-1,1) +
  ylim(0,2) +
  labs(x = NULL, y = NULL) +
  geom_vline(xintercept = quantile(unlist(x), probs = 0.5))