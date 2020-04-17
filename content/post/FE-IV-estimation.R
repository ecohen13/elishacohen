library(lfe)
library(plm)
library(texreg)

## Simulate data
set.seed(2345)
# Covariates
x <- rnorm(1000)
x2 <- rnorm(length(x))
# Individuals and firms
id <- factor(sample(20,length(x),replace=TRUE))
firm <- factor(sample(13,length(x),replace=TRUE))
# Effects for them
id.eff <- rnorm(nlevels(id))
firm.eff <- rnorm(nlevels(firm))
# Left hand side
u <- rnorm(length(x))
y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u


# Q and W are instrumented by x3 and the factor x4.
x3 <- rnorm(length(x))
x4 <- sample(12,length(x),replace=TRUE)
Q <- 0.3*x3 + x + 0.2*x2 + id.eff[id] + 0.3*log(x4) - 0.3*y + rnorm(length(x),sd=0.3)
W <- 0.7*x3 - 2*x + 0.1*x2 - 0.7*id.eff[id] + 0.8*cos(x4) - 0.2*y+ rnorm(length(x),sd=0.6)
# Add them to the outcome variable
y <- y + Q + W

## Estimate the IV model and report robust SEs
ivest <- felm(y ~ x + x2 | id + firm | (Q|W ~ x3 + factor(x4)))
summary(ivest, robust=TRUE)
condfstat(ivest)


# Create a large cluster group (500 clusters) and a small one (20 clusters)
cl1 <- factor(sample(rep(1:500, length.out=length(x))))
cl2 <- factor(sample(rep(1:20, length.out=length(x))))
# Function for adding clustered noise to our outcome variable 
cl_noise <- function(cl) {
  obs_per_cluster <- length(x)/nlevels(cl)
  unlist(replicate(nlevels(cl), rnorm(obs_per_cluster, mean=rnorm(1), sd=runif(1)), simplify=FALSE))
}
# New outcome variable
y_cl <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + cl_noise(cl1) + cl_noise(cl2)

## Estimate and print the model with cluster-robust SEs (default)
est_cl <- felm(y_cl ~ x + x2 | id + firm | (Q|W ~ x3 + factor(x4)) | cl1 + cl2)
summary(est_cl)

ci <- confint(est_cl)
ci
screenreg(est_cl,ci.force=T, ci.force.level = 0.95)

# t distribution to calculate CIs
df <- summary(est_cl)$df[1]
summary(est_cl)$coefficients[,1] + abs(qt(.025,df))*summary(est_cl)$coefficients[,2]
summary(est_cl)$coefficients[,1] - abs(qt(.025,df))*summary(est_cl)$coefficients[,2]
