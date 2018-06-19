# load libraries to run Bayes models in parallel
# library(R2jags)
# library(parallel)
# library(random)
# library(abind)
# library(MCMCpack)
# library(MASS)
# library(mclust)

if(!exists('.DEBUG')) .DEBUG <- FALSE

RNGkind("L'Ecuyer-CMRG")
set.seed(suppressWarnings(tryCatch(as.vector(randomNumbers(1, max=999, col=1)), 
                                   error=function(x) {return(sample(seq_len(999), 1))})))

# make sure X is a matrix
.X <- cbind(X)

# center and scale X by sd(X)
if(!is.null(.X)) .X <- scale(.X)

.X <- cbind(rep(1, nrow(Occur)), .X)

# number of covariates
.K <- ncol(.X)

# number of sites in Occurance matrix
.n.sites <- nrow(Occur)

# number of species in Occurance matrix
.n.species <- ncol(Occur)

# identity matrix for Wishart prior
.ID <- diag(.n.species)

# degrees of freedom for Wishart prior
.df <- .n.species + df

# write the BUGS model and save to a tempfile
cat(
  '
  model {
  
  for (sites in 1:n.sites) {
  Z[sites, 1:n.species] ~ dmnorm(Mu[sites, ], Tau[, ])
  for (species in 1:n.species) {
  Mu[sites, species] <- inprod(Beta.raw[species, ], X[sites, ]) 
  Occur[sites, species] ~ dbern(p[sites, species])
  p[sites, species] <- step(Z[sites, species])
  }        
  }
  
  for(species in 1:n.species) {
  for(k in 1:K) {
  Beta.raw[species, k] ~ dnorm(mu.raw[k], tau[k]) 
  }
  }
  
  for(k in 1:K) {
  mu.raw[k] ~ dnorm(0, .0001)
  tau[k] <- pow(sigma.raw[k], -2) 
  sigma.raw[k] ~ dunif(0, 100) 
  }
  
  Tau[1:n.species, 1:n.species] ~ dwish(ID[, ], df)
  }
  '
  , file=(.modelfile <- tempfile()))


# Function to sensibly intialise Tau, Z, Beta.raw, mu.raw and sigma.raw
.inits <- function() {
  Tau <- rwish(.df, .ID)
  Sigma <- solve(Tau)
  Z <- abs(t(replicate(.n.sites, mvrnorm(1, rep(0, .n.species), Sigma))))
  Z <- ifelse(as.matrix(Occur), Z, -1 * Z)
  Sigma <- mvnXXX(Z)$parameters$variance$sigma[, , 1]
  Beta <- t(sapply(seq_len(ncol(Occur)), 
                   function(x) {unname(coef(glm(Occur[, x] ~ .X[, -1], 
                                                family=binomial(link=probit))))}))
  Beta.raw <- Beta * sqrt(diag(Sigma))
  mu.raw <- apply(Beta.raw, 2, mean)
  sigma.raw <- apply(Beta.raw, 2, sd)    
  return(list(Tau=solve(Sigma), Z=Z, Beta.raw=Beta.raw, mu.raw=mu.raw, sigma.raw=sigma.raw))
}

.call <- "jags(data=list(Occur=Occur, X=.X, 
K=.K, n.sites=.n.sites, n.species=.n.species, 
ID=.ID, df=.df), .inits, parameters.to.save=c('Beta.raw', 'Tau'),
model.file=.modelfile, DIC=FALSE, n.iter=n.iter, n.burnin=n.burn,
n.chains=1, n.thin=n.thin)"

if(!.DEBUG) .applyfn <- function(x, y) {
  mclapply(x, y, mc.cores= detectCores())
} else {.applyfn <- lapply}

.model <- .applyfn(seq_len(n.chains), function(chain) {
  if(!.DEBUG) {sink(tempfile()); sink(file(tempfile(), open="wt"), type='message')}
  if(!.DEBUG) {
    suppressWarnings(assign('model', eval(parse(text=.call)))) 
  } else {
    cat('Chain took', system.time(assign('model', eval(parse(text=.call))))[3], 'seconds\n')    
  }
  if(!.DEBUG){sink(file=NULL, type='message'); sink(file=NULL)}
  # import the model into the Global environment
  attach.jags(model)
  
  # Calcuate the covariance matrices from Tau
  Sigma2 <- apply(Tau, 1, solve)
  dim(Sigma2) <- rev(dim(Tau))
  Sigma2 <- aperm(Sigma2, c(3, 2, 1))
  
  # Calculate the correlation matrices from Sigma2
  Rho <- apply(Sigma2, 1, cov2cor)
  dim(Rho) <- rev(dim(Sigma2))
  Rho <- aperm(Rho, c(3, 2, 1))
  
  dim(Beta.raw) <- c(dim(Beta.raw)[1], dim(Beta.raw)[2], .K)
  
  # Calculate Beta from Beta.raw and Sigma2
  Beta <- apply(Beta.raw, 3, function(x) x / t(sqrt(apply(Sigma2, 1, diag))))
  dim(Beta) <- dim(Beta.raw)
  
  # Calculate the Mu, average prob of occurance on probit scale, from Beta.raw and X
  Mu <- array(dim=c(n.sims, .n.sites, .n.species))
  for(sims in seq_len(n.sims)) {
    for(sites in seq_len(.n.sites)) {
      for(species in seq_len(.n.species)) {
        Mu[sims, sites, species] <- Beta.raw[sims, species, ] %*% .X[sites, ] 
      }
    }
  }
  
  # Calculate the correlation due to the environment
  EnvRho <- apply(Beta, 1, 
                  function(x) {
                    matrix(rowSums(apply(cbind(x[, -1]), 2, function(y) outer(y, y))), .n.species)
                  }
  )
  dim(EnvRho) <- rev(dim(Sigma2))
  EnvRho <- aperm(EnvRho, c(3, 2, 1))
  
  COVX <- cov(.X)
  
  for(sims in seq_len(n.sims)) {
    for(species in seq_len(.n.species)) {
      for(species.prime in seq_len(.n.species)) {
        EnvRho[sims, species, species.prime] <- (EnvRho[sims, species, species.prime] +
                                                   sum(sapply(seq_len(.K)[-1], function(k) {
                                                     sum(Beta[sims, species, k] * 
                                                           Beta[sims, species.prime, seq_len(.K)[-c(1, k)]] * 
                                                           COVX[k, seq_len(.K)[-c(1, k)]]
                                                     )}
                                                   )))
      }
    }
  }
  
  EnvRho <- apply(EnvRho, 1, cov2cor)
  dim(EnvRho) <- rev(dim(Sigma2))
  EnvRho <- aperm(EnvRho, c(3, 2, 1))
  
  # remove the model from the env
  detach.jags()
  
  # garbage collection
  gc(FALSE)
  
  return(list(model, Rho, Beta, Mu, EnvRho))
  
}
)

if(!.DEBUG) rm(.ID, .K, .X, .df, .inits, .modelfile, .n.sites, .n.species, .call, .applyfn)
gc(FALSE)

.models <- Rho <- Beta <- Mu <- EnvRho <- vector('list', length=n.chains)

for(.chain in 1:n.chains) {
  .models[[.chain]] <- .model[[.chain]][[1]]
  Rho[[.chain]] <- .model[[.chain]][[2]]
  Beta[[.chain]] <- .model[[.chain]][[3]]
  Mu[[.chain]] <- .model[[.chain]][[4]]
  EnvRho[[.chain]] <- .model[[.chain]][[5]]
}  

attr(.models, 'seed') <- .Random.seed
assign(model_name, .models)

if(!.DEBUG) rm(.models, .model, .chain)
gc(FALSE)

TPLOT <- function(x, y, z, burn=0) {
  x <- lapply(x, function(x) x[seq.int(burn + 1, dim(x)[1], 
                                       length.out=min(dim(x)[1], 500)), y, z])
  x <- do.call(mcmc.list, lapply(x, as.mcmc))
  traceplot(x)
}

DPLOT <- function(x, y, z, burn=0) {
  x <- lapply(x, function(x) x[seq(burn + 1, dim(x)[1], 1), y, z])
  x <- do.call(mcmc.list, lapply(x, as.mcmc))
  densityplot(x)
}

Diagnose <- function(x,  diagnostic=c('rhat', 'effn'), burn=0) {
  require(coda)
  diagnostic <- match.arg(diagnostic)
  matrix(mapply(
    function(x, y, z, burn=0) {
      x <- lapply(x, function(x) x[seq(burn + 1, dim(x)[1], 1), y, z])
      x <- do.call(mcmc.list, lapply(x, as.mcmc))
      switch(diagnostic,
             rhat=return(unname(gelman.diag(x)$psrf[, 1])),
             effn=return(sum(unlist(lapply(x, effectiveSize)))))
    }, list(x), matrix(rep(seq_len(dim(x[[1]])[2])), dim(x[[1]])[2], dim(x[[1]])[3]),
    matrix(rep(seq_len(dim(x[[1]])[3]), each=dim(x[[1]])[2]), dim(x[[1]])[2])
  ), dim(x[[1]])[2])
}

BURN <- function(burn) {
  Rho <<- lapply(Rho, function(x) x[seq(burn + 1, dim(x)[1], 1), , ])
  Mu <<- lapply(Mu, function(x) x[seq(burn + 1, dim(x)[1], 1), , ])
  Beta <<- lapply(Beta, function(x) x[seq(burn + 1, dim(x)[1], 1), , ])
  EnvRho <<- lapply(EnvRho, function(x) x[seq(burn + 1, dim(x)[1], 1), , ])
}

THIN <- function(thin) {
  Rho <<- lapply(Rho, function(x) x[seq(1, dim(x)[1], thin), , ])
  Mu <<- lapply(Mu, function(x) x[seq(1, dim(x)[1], thin), , ])
  Beta <<- lapply(Beta, function(x) x[seq(1, dim(x)[1], thin), , ])
  EnvRho <<- lapply(EnvRho, function(x) x[seq(1, dim(x)[1], thin), , ]) 
}

SUMMARY <- function(x, FUN=mean) {
  require(abind)
  apply(do.call(abind, c(x, along=1)), c(2, 3), FUN)
}

SUMMARY2 <- function(g, FUN=mean, ...) {
  require(abind)
  apply(do.call(abind, c(g, along=1)), c(2, 3), FUN, ...)
}

rm(.DEBUG)
