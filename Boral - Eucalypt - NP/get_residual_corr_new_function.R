get.residual.cor.new <- function (object, est = "median", prob = 0.95) 
{
  fit.mcmc <- object$jags.model$BUGSoutput
  if (is.null(fit.mcmc)) 
    stop("MCMC samples not found")
  fit.mcmc <- mcmc(object$jags.model$BUGSoutput$sims.matrix, 
                   start = 1, thin = object$mcmc.control$n.thin)
  y <- object$y
  X <- object$X
  num.lv <- object$num.lv
  if (length(grep("lvs", colnames(fit.mcmc))) == 0) 
    stop("Cannot find MCMC samples corresponding to latent variables.")
  n <- nrow(y)
  p <- ncol(y)
  sig.rescor.mat <- rescor.mat <- rescov.mat <- matrix(0, 
                                                       p, p)
  if (is.null(colnames(y))) 
    colnames(y) <- 1:ncol(y)
  rownames(rescor.mat) <- colnames(rescor.mat) <- colnames(y)
  rownames(sig.rescor.mat) <- colnames(sig.rescor.mat) <- colnames(y)
  rownames(rescov.mat) <- colnames(rescov.mat) <- colnames(y)
  all.rescor.mat <- all.rescov.mat <- array(0, dim = c(nrow(fit.mcmc), 
                                                       p, p))
  all.trace.rescor <- numeric(nrow(fit.mcmc))
  for (t in 1:nrow(fit.mcmc)) {
    lv.coefs <- matrix(fit.mcmc[t, grep("lv.coefs", colnames(fit.mcmc))], 
                       nrow = p)
    lambdalambdaT <- as.matrix(lv.coefs[, 2:(num.lv + 1)]) %*% 
      t(as.matrix(lv.coefs[, 2:(num.lv + 1)])) + diag(ncol(PA))  ## Added identity matrix after conversation with Nick/Francis
    all.rescov.mat[t, , ] <- (lambdalambdaT)
    all.trace.rescor[t] <- sum(diag(lambdalambdaT))
    if (all(object$family == "negative.binomial")) {
      get.var.phis <- numeric(p)
      for (j in 1:p) get.var.phis[j] <- var(log(rgamma(2000, 
                                                       shape = 1/lv.coefs[j, ncol(lv.coefs)], rate = 1/lv.coefs[j,
                                                                                                                ncol(lv.coefs)])))
      all.rescov.mat[t, , ] <- lambdalambdaT + diag(x = get.var.phis, 
                                                    nrow = p)
    }
    all.rescor.mat[t, , ] <- cov2cor(all.rescov.mat[t, , 
                                                    ])
  }
  for (j in 1:p) {
    for (j2 in 1:p) {
      if (est == "median") {
        rescor.mat[j, j2] <- median(all.rescor.mat[,j, j2])
        rescov.mat[j, j2] <- median(all.rescov.mat[,j, j2])
      }
      if (est == "mean") {
        rescor.mat[j, j2] <- mean(all.rescor.mat[, j, j2])
        rescov.mat[j, j2] <- mean(all.rescov.mat[, j, j2])
      }
      if (est == "sd") {
        rescor.mat[j, j2] <- sd(all.rescor.mat[, j, j2])
        rescov.mat[j, j2] <- sd(all.rescov.mat[, j, j2])
      }
      if (est == "q_lower") {
        rescor.mat[j, j2] <- quantile(all.rescor.mat[, j, j2], probs = 0.025)
        rescov.mat[j, j2] <- quantile(all.rescov.mat[, j, j2], probs = 0.025)
      }
      if (est == "q_upper") {
        rescor.mat[j, j2] <- quantile(all.rescor.mat[, j, j2], probs = 0.975)
        rescov.mat[j, j2] <- quantile(all.rescov.mat[, j, j2], probs = 0.975)
      }
      sig.rescor.mat[j, j2] <- rescor.mat[j, j2]
      get.hpd.cors <- HPDinterval(as.mcmc(all.rescor.mat[, 
                                                         j, j2]), prob = 0.95)
      if (0 > get.hpd.cors[1] & 0 < get.hpd.cors[2]) 
        sig.rescor.mat[j, j2] <- 0
    }
  }
  if (est == "median") 
    final.trace <- median(all.trace.rescor)
  if (est == "mean") 
    final.trace <- mean(all.trace.rescor)
  return(list(cor = rescor.mat, sig.cor = sig.rescor.mat, 
              cov = rescov.mat))
}
