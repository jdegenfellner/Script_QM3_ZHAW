# load data again, since it's a long way back
library(rethinking)
library(tidyverse)

data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]
xbar <- mean(d2$weight)
# fit model
mod <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar),
    a ~ dnorm(171.1, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ) ,
  data = d2)
precis(mod)

post <- extract.samples(mod)
dim(post)
head(post)
# basic density plots using dens from package rethinking------
dens(post$a) # alpha
dens(post$b) # beta
dens(post$sigma) # sigma

# density of a using ggplot and 99% credible interval:---------
# Compute the 99% credible interval
ci_lower <- quantile(post$a, 0.005)
ci_upper <- quantile(post$a, 0.995)

# Create density data
density_data <- density(post$a)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Plot density with shaded credible interval
ggplot(density_df, aes(x = x, y = y)) +
  geom_line() +  # Density line
  geom_area(data = subset(density_df, x >= ci_lower & x <= ci_upper), 
            aes(x = x, y = y), fill = "blue", alpha = 0.4) +  # Shaded 99% interval
  geom_vline(xintercept = c(ci_lower, ci_upper), color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(title = "Density of a with 99% Credible Interval", x = "a", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5))


# density of b using ggplot and 99% credible interval:--------
# Compute the 99% credible interval
ci_lower <- quantile(post$b, 0.005)
ci_upper <- quantile(post$b, 0.995)

# Create density data
density_data <- density(post$b)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Plot density with shaded credible interval
ggplot(density_df, aes(x = x, y = y)) +
  geom_line() +  # Density line
  geom_area(data = subset(density_df, x >= ci_lower & x <= ci_upper), 
            aes(x = x, y = y), fill = "blue", alpha = 0.4) +  # Shaded 99% interval
  geom_vline(xintercept = c(ci_lower, ci_upper), color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(title = "Density of b with 99% Credible Interval", x = "b", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5))

# density of sigma using ggplot and 99% credible interval:-----
# Compute the 99% credible interval
( ci_lower <- quantile(post$sigma, 0.005) )
( ci_upper <- quantile(post$sigma, 0.995) )
mean(post$sigma) # sigma hat
median(post$sigma) # sigma median

# Create density data
density_data <- density(post$sigma)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Plot density with shaded credible interval
ggplot(density_df, aes(x = x, y = y)) +
  geom_line() +  # Density line
  geom_area(data = subset(density_df, x >= ci_lower & x <= ci_upper), 
            aes(x = x, y = y), fill = "blue", alpha = 0.4) +  # Shaded 99% interval
  geom_vline(xintercept = c(ci_lower, ci_upper), color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(title = "Density of sigma with 99% Credible Interval", x = "sigma", y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5))

post$sigma
post$sigma > 6
sum(post$sigma > 6) / length(post$sigma)
mean(post$sigma > 4.5 & post$sigma < 5) # 0.3452 a posteriori



