library(pacman)
p_load(tidyverse, readxl)

# Read file
url <- "https://raw.githubusercontent.com/jdegenfellner/Script_QM2_ZHAW/main/data/chapter%205_assignment%201_2_wide.xls"
temp_file <- tempfile(fileext = ".xls")
download.file(url, temp_file, mode = "wb")  # mode="wb" is important for binary files
df <- read_excel(temp_file)

head(df)
dim(df)

# As in the book, let's randomly select 50 patients.
set.seed(123)
df <- df %>% sample_n(50)
dim(df)

# "as" = affected shoulder
# "nas" = not affected shoulder

df <- df %>%
  dplyr::mutate(diff = abs(ROMnas.Peter - ROMnas.Mary))  # Compute absolute difference

max_diff_point <- df %>%
  dplyr::filter(diff == max(diff, na.rm = TRUE))  # Find the row with the max difference

df %>%
  ggplot(aes(x = ROMnas.Peter, y = ROMnas.Mary)) +
  geom_point() + 
  geom_point(data = max_diff_point, aes(x = ROMnas.Peter, y = ROMnas.Mary), 
             color = "blue", size = 4) +  # Highlight max difference point
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal() +
  ggtitle("ROMnas.Peter vs. ROMnas.Mary") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = max_diff_point$ROMnas.Peter, 
           y = max_diff_point$ROMnas.Mary, 
           label = paste0("Max Diff: ", round(max_diff_point$diff, 2)), 
           vjust = -1, color = "blue", size = 4)

# average abs. difference:
mean(df$diff, na.rm = TRUE) # 7.2

cor(df$ROMnas.Peter, df$ROMnas.Mary, use = "complete.obs")

library(ggExtra)
df <- df %>%
  mutate(diff = abs(ROMas.Peter - ROMas.Mary))  # Compute absolute difference

max_diff_point <- df %>%
  dplyr::filter(diff == max(diff, na.rm = TRUE))  # Find the row with the max difference

p <- df %>%
  ggplot(aes(x = ROMas.Peter, y = ROMas.Mary)) +
  geom_point() + 
  geom_point(data = max_diff_point, aes(x = ROMas.Peter, y = ROMas.Mary), 
             color = "blue", size = 4) +  # Highlight max difference point
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal() +
  ggtitle("ROMas.Peter vs. ROMas.Mary") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = max_diff_point$ROMas.Peter, 
           y = max_diff_point$ROMas.Mary, 
           label = paste0("Max Diff: ", round(max_diff_point$diff, 2)), 
           vjust = -1, color = "blue", size = 4)
# Add marginal histograms
ggMarginal(p, type = "density", fill = "gray", color = "black")

# average abs. difference:
mean(df$diff, na.rm = TRUE) # 7.2
cor(df$ROMas.Peter, df$ROMas.Mary, use = "complete.obs") 

# mean difference
mean(df$ROMas.Peter - df$ROMas.Mary, na.rm = TRUE) 


# ----------

library(rethinking)
library(tictoc)

df_long <- df %>% 
  mutate(ID = row_number()) %>%
  dplyr::select(ID,ROMas.Peter, ROMas.Mary) %>% 
  pivot_longer(cols = c(ROMas.Peter, ROMas.Mary), 
               names_to = "Rater", values_to = "ROM") %>% 
  mutate(Rater = factor(Rater))

tic()
m5.1 <- ulam(
  alist(
    # Likelihood
    ROM ~ dnorm(mu, sigma),
    
    # Patient-specific intercepts (random effects)
    mu <- a[ID],  
    a[ID] ~ dnorm(mu_a, sigma_ID),  # Hierarchical structure for patients
    
    # Priors for hyperparameters
    mu_a ~ dnorm(66, 20),  # Population-level mean
    sigma_ID ~ dunif(0,20),  # Between-patient standard deviation
    sigma ~ dunif(0,20)  # Residual standard deviation
  ), 
  data = df_long, 
  chains = 8, cores = 4
)
toc() # 7s

precis(m5.1, depth = 2)

post <- extract.samples(m5.1)
var_patients <- mean(post$sigma_ID^2)  # Between-patient variance
var_residual <- mean(post$sigma^2)     # Residual variance
var_patients / (var_patients + var_residual) # ICC


# ---------

library(lme4)
m5.2 <- lmer(ROM ~ (1|ID), data = df_long)
summary(m5.2)
print(VarCorr(m5.2), comp = "Variance")

# ICC = 
270.99 / (270.99 + 47.35) # 
# 0.8512597
# -> exactly the same result as the irr package

# ----------

library(rethinking)
library(conflicted)
conflicts_prefer(posterior::sd)
df_long_bias <- df_long %>%
  mutate(ROM = ROM + ifelse(Rater == "ROMas.Mary", 5, 0))
head(df_long_bias)

set.seed(123)
m5.2 <- ulam(
  alist(
    # Likelihood
    ROM ~ dnorm(mu, sigma_eps),
    
    # Model for mean ROM with patient and rater effects
    mu <- alpha[ID] + beta[Rater],
    
    # Patient-specific random effects
    alpha[ID] ~ dnorm(mu_alpha, sigma_alpha),
    
    # Rater effect (Peter/Mary)
    beta[Rater] ~ dnorm(0, sigma_beta),
    
    # Priors for hyperparameters
    mu_alpha ~ dnorm(66, 10),  # Population mean ROM
    sigma_alpha ~ dexp(0.5),  # Between-patient SD (less aggressive shrinkage)
    sigma_beta ~ dexp(1),   # Rater SD (better regularization)
    sigma_eps ~ dexp(1)     # Residual SD (prevents over-shrinkage)
  ), 
  data = df_long_bias, 
  chains = 8, cores = 4
)

precis(m5.2, depth = 2)
precis(m5.2)

# check systematic difference for rater in posterior
post <- extract.samples(m5.2)
mean(post$beta[,1] - post$beta[,2])

# ICC agreement:
post <- extract.samples(m5.2)
(var_patients <- mean(post$sigma_alpha^2))  # Between-patient variance
(var_raters <- mean(post$sigma_beta^2))     # Rater variance
(var_residual <- mean(post$sigma_eps^2))    # Residual variance

# ICC_agreement = 
var_patients / (var_patients + var_raters + var_residual)
# 0.8033613 (sigma_alpha ~ dexp(1))
# 0.83 (sigma_alpha ~ dexp(0.5))

# ICC (Single_fixed_raters) = ICC3 in psych output = 
var_patients / (var_patients + var_residual)
# 0.8415256


#. -----------

library(psych)
library(conflicted)
# needs wide format
#conflicts_prefer(dplyr::select)
df_wide <- df_long_bias %>%
  pivot_wider(names_from = Rater, values_from = ROM)
df_wide_values <- df_wide %>% dplyr::select(-ID)
psych::ICC(df_wide_values) # ICC1 = 0.83


# Verify ICC1k using lmer:
df_long

