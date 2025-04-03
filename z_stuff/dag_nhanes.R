#install.packages("NHANES")
library(pacman)
p_load(NHANES, tidyverse, ggdag, dagitty)
data(NHANES)
head(NHANES)
unique(NHANES$SurveyYr)

colnames(NHANES)


df <- NHANES # shorter

# Define the DAG
dag <- dagitty('dag {
  PhysActive -> BPSysAve
  Age -> PhysActive
  Age -> BPSysAve
  PhysActive -> BMI
  BMI -> BPSysAve
  Gender -> PhysActive
  Gender -> BMI
  Gender -> BPSysAve
}')

# Set node coordinates for a nice layout
dagitty::coordinates(dag) <- list(
  x = c(PhysActive = 0, BPSysAve = 2, Age = 1, BMI = 1, Gender = 0.5),
  y = c(PhysActive = 1, BPSysAve = 1, Age = 2, BMI = 1.5, Gender = 2)
)

# Plot the DAG with larger node labels and bubbles
ggdag(dag) + 
  theme_minimal() + 
  geom_dag_point(size = 20, color = "black") +  # Increase node size
  geom_dag_text(size = 2.5, color = "white") +    # Increase label size
  ggtitle("Hypothesized Relationships") + 
  theme(plot.title = element_text(hjust = 0.5))

# explore associations-------
# Age and BMI
df %>% 
  dplyr::filter(Age >= 20) %>%
  dplyr::select(Age, BMI) %>%
  ggplot(aes(Age, BMI)) +
  geom_point() + 
  geom_smooth(method = "loess", se = TRUE)
# once could argue that there is a weakly curved relationship between Age and BMI
summary(lm(BMI ~ Age + I(Age^2), data = df_age)) 
# which can be confirmed due to the very large sample size

# Gender and PyhsActive
df_age <- df %>% 
  dplyr::filter(Age >= 20)
table(df_age$Gender, df_age$PhysActive)
# add margins to table:
addmargins(table(df_age$Gender, df_age$PhysActive))/sum(table(df_age$Gender, df_age$PhysActive))
chisq.test(table(df_age$Gender, df_age$PhysActive))

df %>% 
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x=Gender, fill=PhysActive)) + 
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", title = "Physical Activity by Gender") +
  theme_minimal()

# gender and PBSysAve
hist(df$BPSysAve)
t.test(df$BPSysAve ~ df$Gender)
df %>% 
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x = as.factor(Gender), y = BPSysAve)) +  # Corrected `y`
  geom_boxplot() +
  labs(x = "Gender", y = "Systolic Blood Pressure (BPSysAve)", title = "Blood Pressure by Gender") +
  theme_minimal()

# age and BPSysAve
df %>% 
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x = Age, y = BPSysAve)) +  # Corrected `y`
  geom_point() +
  geom_smooth(method = "loess", se = TRUE)
  labs(x = "Age", y = "Systolic Blood Pressure (BPSysAve)", title = "Blood Pressure by Age") +
  theme_minimal()
  
# age and physical activity
df %>%
  dplyr::filter(Age >= 20) %>%
  ggplot(aes(x = Age, fill = PhysActive)) +
  geom_density(alpha = 0.5) +
  labs(x = "Age", y = "Density", title = "Physical Activity by Age") +
  theme_minimal()
t.test(df_age$Age ~ df_age$PhysActive)



# Research question: Influence of Physical Activity on Blood Pressure
# Estimate the total effect of Physical Activity on Blood Pressure
# using a linear regression model
lm(BPSysAve ~ PhysActive, data = df_age) %>% 
  summary()

impliedConditionalIndependencies(dag)
adjustmentSets(dag, exposure = "PhysActive", outcome = "BPSysAve")

# Testing implied conditional independencies:
# Age _||_ BMI | Gnder, PhyA
summary(lm(Age ~ BMI + Gender + PhysActive, data = df %>% dplyr::filter(Age >= 20)))
# check

# Age _||_ Gndr
summary(lm(Age ~ Gender, data = df %>% dplyr::filter(Age >= 20)))
# check by logic

# outcome
hist(df_age$BPSysAve)
#shapiro.test(df_age[sample(1:nrow(df_age),4000,replace = FALSE),]$BPSysAve)
df_age %>%
  ggplot(aes(x = BPSysAve)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", alpha = 0.6) +  # Ensure density scaling
  geom_density(color = "blue", size = 1) +  # Smoothed density of actual data
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(df_age$BPSysAve, na.rm = TRUE), sd = sd(df_age$BPSysAve, na.rm = TRUE)), 
    color = "red", size = 1, linetype = "dashed"
  ) +  # Theoretical normal curve
  labs(
    x = "Systolic Blood Pressure (BPSysAve)", 
    y = "Density", 
    title = "Distribution of Systolic Blood Pressure with Normal Curve"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

summary(df_age$BPSysAve)

















# new DAG

# Define the DAG
dag <- dagitty('dag {
  PhysActive -> BPSysAve
  Age -> PhysActive
  Age -> BPSysAve
  Age -> BMI
  BMI -> PhysActive
  BMI -> BPSysAve
  Gender -> PhysActive
  Gender -> BMI
  Gender -> BPSysAve
}')

# Set node coordinates for a nice layout
dagitty::coordinates(dag) <- list(
  x = c(PhysActive = 0, BPSysAve = 2, Age = 1, BMI = 1, Gender = 0.5),
  y = c(PhysActive = 1, BPSysAve = 1, Age = 2, BMI = 1.5, Gender = 2)
)

# Plot the DAG with larger node labels and bubbles
ggdag(dag) + 
  theme_minimal() + 
  geom_dag_point(size = 20, color = "black") +  # Increase node size
  geom_dag_text(size = 2.5, color = "white") +    # Increase label size
  ggtitle("Hypothesized Relationships") + 
  theme(plot.title = element_text(hjust = 0.5))

impliedConditionalIndependencies(dag)
adjustmentSets(dag, exposure = "PhysActive", outcome = "BPSysAve")



set.seed(123)
n_sims <- dim(df_age)[1] # 7235
beta_0_vec <- rnorm(n_sims, 140, 20)
beta_1_vec <- rnorm(n_sims, 0, 50)
beta_2_vec <- rnorm(n_sims, 0, 10)
beta_3_vec <- rnorm(n_sims, 0, 10)
sigma_vec <- runif(n_sims, 0, 50)
BPSysAve_sim <- rnorm(n_sims, beta_0_vec + beta_1_vec + beta_2_vec + beta_3_vec, sigma_vec)
length(BPSysAve_sim) # 10^4

df_sim_vs_obs <- data.frame(
  BPSysAve_sim = BPSysAve_sim,
  BPSysAve_obs = df_age$BPSysAve
)
df_sim_vs_obs %>%
  ggplot(aes(x = BPSysAve_sim, y = BPSysAve_obs)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Simulated Systolic Blood Pressure", y = "Observed Systolic Blood Pressure", title = "Simulated vs. Observed Systolic Blood Pressure") +
  theme_minimal()

# negative for blood pressure not possible

set.seed(123)
n_sims <- dim(df_age)[1] # 7235
beta_0_vec <- rnorm(n_sims, 120, 7)
beta_1_vec <- rnorm(n_sims, 0, 10)
beta_2_vec <- rnorm(n_sims, 5, 10)
beta_3_vec <- rnorm(n_sims, 0, 10)
sigma_vec <- runif(n_sims, 0, 20)
BPSysAve_sim <- rnorm(n_sims, beta_0_vec + beta_1_vec + beta_2_vec + beta_3_vec, sigma_vec)
length(BPSysAve_sim) # 10^4

df_sim_vs_obs <- data.frame(
  BPSysAve_sim = BPSysAve_sim,
  BPSysAve_obs = df_age$BPSysAve
)
df_sim_vs_obs %>%
  ggplot(aes(x = BPSysAve_sim, y = BPSysAve_obs)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Simulated Systolic Blood Pressure", y = "Observed Systolic Blood Pressure", title = "Simulated vs. Observed Systolic Blood Pressure") +
  theme_minimal()

# Combine observed and simulated values into one long-format data frame
df_long <- df_sim_vs_obs %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Type", values_to = "BPSysAve") %>%
  mutate(Type = factor(Type, levels = c("BPSysAve_obs", "BPSysAve_sim"), labels = c("Observed", "Simulated")))

# Plot densities of both observed and simulated values
ggplot(df_long, aes(x = BPSysAve, fill = Type)) +
  geom_density(alpha = 0.5) +  # Semi-transparent density curves
  labs(
    x = "Systolic Blood Pressure", 
    y = "Density", 
    title = "Observed vs. Simulated Systolic Blood Pressure"
  ) +
  scale_fill_manual(values = c("Observed" = "blue", "Simulated" = "red")) +  # Custom colors
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


library(rethinking)
library(dplyr)
library(data.table)

# Ensure correct numeric conversion (1-based indexing)
df <- NHANES # shorter
df <- as.data.table(df)

# Compute age mean for centering
Age_mean <- mean(df[Age >= 20,]$Age, na.rm = TRUE)

# Fit Bayesian model
df <- df %>% dplyr::filter(Age >= 20) %>% 
  dplyr::select(BPSysAve, PhysActive, Age, Gender, BMI) %>%
  drop_na()
dim(df) # 6919    7
sum(is.na(df)) # 0
m_NHANES <- quap(
  alist(
    BPSysAve ~ dnorm(mu, sigma),  # Use correct variable name
    mu <- beta_0 + beta_1[PhysActive] + beta_2 * (Age - Age_mean) + beta_3[Gender],
    beta_0 ~ dnorm(120, 7),
    beta_1[PhysActive] ~ dnorm(0, 10),  # Explicitly define levels (assuming two levels for PhysActive)
    beta_2 ~ dnorm(5, 10),
    beta_3[Gender] ~ dnorm(0, 10),  # Explicitly define levels (assuming two levels for Gender)
    sigma ~ dunif(0, 20)  # Proper prior for residual standard deviation
  ),
  data = df
)

precis(m_NHANES, depth = 2)

# we want to know the expecte difference for the levels of PhysActive and Gender:
post <- extract.samples(m_NHANES)
post$diff_PhysActive <- post$beta_1[,2] - post$beta_1[,1]
post$diff_G <- post$beta_3[,2] - post$beta_3[,1]

conflicts_prefer(posterior::sd)
precis(post, depth = 2)

# classical model
df <- df %>% mutate(Age_center = Age - mean(Age))
mod <- lm(BPSysAve ~ PhysActive + Age_center + Gender, data = df)
summary(mod)
# PhysActiveYes  -0.73693 "significant", at least p-value rather small.
check_model(mod)
qqPlot(mod)

mod_incl_bmi <- lm(BPSysAve ~ PhysActive + Age + Gender  + BMI, data = df)
summary(mod_incl_bmi)
# PhysActiveYes -0.22870 # not "significant"
check_model(mod_incl_bmi) # as bad as before
qqPlot(mod_incl_bmi) # 

# adding the predictor changed the estimate massively.


hist(df$Age)


# use a different distribution of blood pressure

library(fitdistrplus)  # For fitting distributions

# Remove NA values before fitting
bps_values <- as.numeric(na.omit(df_age$BPSysAve))

# Fit a Weibull distribution
weibull_fit <- fitdist(bps_values, "weibull")

# Fit a Gamma distribution
gamma_fit <- fitdist(bps_values, "gamma")

# Display parameters
weibull_fit$estimate
gamma_fit$estimate

library(ggplot2)

ggplot(df_age, aes(x = BPSysAve)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.6) +
  geom_density(color = "blue", size = 1) +  # Smoothed density of actual data
  stat_function(
    fun = dweibull,
    args = list(shape = weibull_fit$estimate["shape"], scale = weibull_fit$estimate["scale"]),
    color = "green", size = 1, linetype = "dashed"
  ) +  # Weibull distribution
  stat_function(
    fun = dgamma,
    args = list(shape = gamma_fit$estimate["shape"], rate = gamma_fit$estimate["rate"]),
    color = "purple", size = 1, linetype = "dashed"
  ) +  # Gamma distribution
  labs(
    x = "Systolic Blood Pressure (BPSysAve)", 
    y = "Density", 
    title = "Distribution of Systolic Blood Pressure with Weibull & Gamma Fits"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



sum(is.na(df)) # 0
m_NHANES_gamma <- quap(
  alist(
    BPSysAve ~ dnorm(mu, sigma), # TODO change to gamma!!!
    sigma <- exp(beta_4 + beta_5 * mu),
    mu <- beta_0 + beta_1[PhysActive] + beta_2 * (Age - Age_mean) + beta_3[Gender],
    beta_0 ~ dnorm(140, 10),  # Narrowed prior to avoid extreme values
    beta_1[PhysActive] ~ dnorm(0, 10),  # Explicitly define factor levels for PhysActive
    beta_2 ~ dnorm(0, 10),
    beta_3[Gender] ~ dnorm(0, 10),  # Explicitly define factor levels for Gender
    beta_4 ~ dnorm(0, 10),  # Start with a more stable mean log-scale SD
    beta_5 ~ dnorm(0, 10)  # Prevent extreme changes in `sigma`
  ),
  data = df,
  start = list(beta_0 = 140, beta_1 = c(0.5, 0), beta_2 = 0, beta_3 = c(0.5, 0), beta_4 = 1, beta_5 = 0)
)

# Show results
precis(m_NHANES_gamma, depth = 2)

# dfference in levels of PhysActive and Gender:
post_gamma <- extract.samples(m_NHANES_gamma)
post_gamma$diff_PhysActive <- post_gamma$beta_1[,2] - post_gamma$beta_1[,1]
post_gamma$diff_G <- post_gamma$beta_3[,2] - post_gamma$beta_3[,1]
precis(post_gamma, depth = 2)

# histrogram of posterior of BPSysAve:
sample_BP <- sim(m_NHANES_gamma, n = 1000)

# Convert the first 100 rows of the posterior samples into a long format for ggplot
df_posterior <- as.data.frame(t(sample_BP[1:100,])) %>%
  pivot_longer(cols = everything(), names_to = "Simulation", values_to = "BPSysAve_sim")

# Create the plot
ggplot() +
  # Original BPSysAve density in green
  geom_density(data = df, aes(x = BPSysAve), color = "green", linewidth = 1.2) +
  # Posterior densities as individual lines in blue with transparency
  geom_density(data = df_posterior, aes(x = BPSysAve_sim, group = Simulation), 
               color = "lightblue", alpha = 0.05) +
  labs(title = "Density Estimation: Original vs. Posterior Samples",
       x = "Systolic Blood Pressure",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




set.seed(122)
m_NHANES_lnorm <- quap(
  alist(
    BPSysAve ~ dlnorm(lmu, lsd), # TODO change 
    lsd <- exp(beta_4 + beta_5 * lmu),
    lmu <- beta_0 + beta_1[PhysActive] + beta_2 * (Age - Age_mean) + beta_3[Gender],
    beta_0 ~ dnorm(140, 10),  # 
    beta_1[PhysActive] ~ dnorm(0, 10),  # 
    beta_2 ~ dnorm(0, 10),
    beta_3[Gender] ~ dnorm(0, 10),  # 
    beta_4 ~ dnorm(0, 10),  # 
    beta_5 ~ dnorm(0, 10)  #
  ),
  data = df,
  start = list(beta_0 = 140, beta_1 = c(0.5, 0), beta_2 = 0, beta_3 = c(0.5, 0), beta_4 = 1, beta_5 = 0)
)

precis(m_NHANES_lnorm, depth = 2)

# difference in levels of PhysActive and Gender (catorical variables):
post_lnorm <- extract.samples(m_NHANES_lnorm)
post_lnorm$diff_PhysActive <- post_lnorm$beta_1[,2] - post_lnorm$beta_1[,1]
post_lnorm$diff_G <- post_lnorm$beta_3[,2] - post_lnorm$beta_3[,1]
precis(post_lnorm, depth = 2)

# posterior predictive checks
sample_BP <- sim(m_NHANES_lnorm, n = 1000)

# Convert the first 100 rows of the posterior samples into a long format for ggplot
df_posterior <- as.data.frame(t(sample_BP[1:100,])) %>%
  pivot_longer(cols = everything(), names_to = "Simulation", values_to = "BPSysAve_sim")

# Create the plot
ggplot() +
  geom_density(data = df_posterior, aes(x = BPSysAve_sim, group = Simulation), 
               color = "lightblue", alpha = 0.05) +
  geom_density(data = df, aes(x = BPSysAve), color = "green", linewidth = 1.2) +
  labs(title = "Density Estimation: Original vs. Posterior Samples",
       x = "Systolic Blood Pressure",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


