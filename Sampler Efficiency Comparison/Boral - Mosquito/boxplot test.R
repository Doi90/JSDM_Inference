a1 <- length(as.vector(ess.factor.loadings))
a2 <- length(as.vector(ess.Rho[upper.tri(ess.Rho)]))


test <- data.frame("a" = c(as.vector(ess.factor.loadings), rep(NA, a2 - a1)),
                   "b" = as.vector(ess.Rho[upper.tri(ess.Rho)]))

boxplot(test)

ggplot() +
  geom_boxplot(data = test)
