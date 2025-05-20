# models---------
set.seed(122)
m13.1 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0, 1.5)
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores()-1)

m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ) , data = dat, 
  chains = 4, 
  log_lik = TRUE,
  cores = detectCores()-1)

# Are the intercepts in model 13.2 correlated?------------
m13.2_sims <- extract.samples(m13.2, n = 1000)
m13.2_sims$a_bar
m13.2_sims$sigma
m13.2_sims$a

cov_a <- cov(m13.2_sims$a)
cor_a <- cov2cor(cov_a)
max(cor_a - diag(nrow(cor_a)))

# plot posterior for a1 a2 from model 13.2--------------
a1 <- m13.2_sims$a[, 1]
a2 <- m13.2_sims$a[, 2]
plot(a1, a2, xlab = "a1", ylab = "a2")
abline(lm(a2 ~ a1), col = "red")
cor(a1, a2) # ~ 0

# what about the intercepts in model 13.1?--------------
m13.1_sims <- extract.samples(m13.1, n = 1000)
cov_a_13.1 <- cov(m13.1_sims$a)
cor_a_13.1 <- cov2cor(cov_a_13.1)
max(cor_a_13.1 - diag(nrow(cor_a_13.1)))

# plot posterior for a1 a2 from model 13.1------------
a1 <- m13.1_sims$a[, 1]
a2 <- m13.1_sims$a[, 2]
plot(a1, a2, xlab = "a1", ylab = "a2")
abline(lm(a2 ~ a1), col = "red")
cor(a1, a2) # ~ 0

# -> still not correlated. The hyperpriors just shrink the estimates,
#    but do not force them to be correlated.


# plot some posteriors
dens(m13.2_sims$a[, 1])
dens(m13.2_sims$a_bar[,1])
dens(m13.2_sims$sigma[, 1])
dens(m13.2_sims$p[, 1])
