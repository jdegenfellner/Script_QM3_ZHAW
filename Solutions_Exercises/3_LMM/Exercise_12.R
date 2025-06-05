# ex 12 simple log reg

# maybe Figure 2 here helps:
# file:///Users/juergen/Downloads/cdc_23546_DS1.pdf
# assuming an age range of 18 to 80 (normally distributed
AGE_unstandardized <- rnorm(100, mean = 50, sd = 10) # standardized age of 100 participants
range(AGE_unstandardized) # good enough
AGE <- scale(AGE_unstandardized) # standardize to mean 0, sd 1
y <- rbinom(100, size = 1, prob = inv_logit(AGE)) # the probability of a 1 increases with AGE
dat <- data.frame(AGE = AGE, y = y)

# according to figure 2 the the rate (i.e. risk, i.e. probability)
# risk for person with mean age: 1000/100000=0.01
# 2 SD upwards -> approx 10 fold increaso to: 10000/100000=0.1
# apriori difference on the pobability scale is 0.1 - 0.01 = 0.09
inv_logit(beta_1*1) - inv_logit(beta_1*0) # == 0.09 -> solve for beta_1
inv_logit(0.36*1) - inv_logit(beta_1*0) - 0.09 # -> beta_1 = 0.36 -> beta_1 -> Normal(0.36, 1)
# -> but no base rate was defined in the simple model. 
# In a realistic setting one would add beta_0 for the base probability as well

# priors:---------

# How well does it reconstruct the true beta_1?
m_logistic <- ulam(
  alist(
    y ~ dbinom(1, p),
    logit(p) <- a + b * AGE,
    a ~ normal(0, 1.5),
    b ~ normal(0.36, 1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
precis(m_logistic, depth = 2) # -> credible interval contains true param.
# estimate is now lower than 1, before it was 50% higher though
