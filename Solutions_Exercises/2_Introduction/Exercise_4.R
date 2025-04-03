# Exercise 4
ChickWeight
head(ChickWeight)
str(ChickWeight)
dim(ChickWeight)

# Exploratory analysis to understand the chicken weights---------
hist(ChickWeight$weight, main = "Histogram of Chick Weights (in g)", 
     xlab = "Weight", ylab = "Frequency", col = "lightblue", border = "black")
mean(ChickWeight$weight)
stats::sd(ChickWeight$weight)
#shapiro.test(ChickWeight$weight) # dont do this

# Set seed for reproducibility
set.seed(123)

# Estimated log-normal parameters
mu <- 4.5  # Location parameter (log-scale mean)
sigma <- 0.5  # Scale parameter (log-scale standard deviation)

# Generate log-normal sample
sample_size <- 1000  # Adjust as needed
lognormal_sample <- rlnorm(sample_size, meanlog = mu, sdlog = sigma)

# Plot histogram
hist(lognormal_sample, 
     breaks = 20, 
     col = rgb(0.2, 0.4, 0.8, 0.5), 
     main = "Histogram of Log-normal Sample", 
     xlab = "Weight", 
     border = "black",
     probability = TRUE)

# maybe a lognormal distribution is fiting to describe the chicken weights.

# Priors for parameters lognormal distribution:---------
# mu_prior ~ N(4.5, 2)
# sigma_prior ~ Uniform(0, 1.5)

# Prior predictive checks
sample_mu <- rnorm(10^4, 4.5, 0.3)
sample_sigma <- runif(10^4, 0.4, 0.6)
prior_h <- rlnorm(10^4, 
                  meanlog = sample_mu, 
                  sdlog = sample_sigma)
length(prior_h)
rethinking::dens(prior_h)
hist(prior_h, breaks = 20, col = rgb(0.2, 0.4, 0.8, 0.5), 
     main = "Prior Predictive Distribution", xlab = "Weight", border = "black")
# ... improve on demand.... to reflext reality better...

# Model specification:---------
colnames(ChickWeight)
library(rethinking)
flist <- alist(
  weight ~ dlnorm(mu, sigma), # weights are described using a lognormal distribution
  mu ~ dnorm(4.5, 0.3), # prior for mean parameter
  sigma ~ dunif(0.4, 0.6) # prior for standard deviation parameter
)
m_weights <- quap(flist, data = ChickWeight)
precis(m_weights) # summarize the posterior for the parameters
plot(m_weights)

# Posterior predictive checks:---------
posterior_h <- extract.samples(m_weights, n = 1e4) # create new observations based on model
head(posterior_h)
cor(posterior_h$mu, posterior_h$sigma) # check!!!

# create chicken weights:
sim_weights <- sim(m_weights) # simulate data
hist(sim_weights)

