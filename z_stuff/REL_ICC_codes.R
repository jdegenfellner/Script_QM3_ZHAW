library(pacman)
p_load(tidyverse, readxl, 
       ggExtra, lme4, irr, 
       tictoc, conflicted, 
       flextable, rethinking)

conflicts_prefer(posterior::sd)

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

# nas-------

df <- df %>%
  mutate(diff = abs(ROMnas.Peter - ROMnas.Mary)) %>%  # Compute absolute difference
  mutate(ID = row_number())  # Add an ID column
  
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

# mean difference
mean(df$ROMnas.Peter - df$ROMnas.Mary, na.rm = TRUE) 



# as---------
df <- df %>%
  mutate(diff = abs(ROMas.Peter - ROMas.Mary))  # Compute absolute difference

max_diff_point <- df %>%
  dplyr::filter(diff == max(diff, na.rm = TRUE))  # Find the row with the max difference
dim(df)
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

# average abs. difference:
mean(df$diff, na.rm = TRUE) # 7.78
cor(df$ROMas.Peter, df$ROMas.Mary, use = "complete.obs") # 0.8284572

hist(df$ROMas.Peter)
hist(df$ROMas.Mary)

# mean difference
mean(df$ROMas.Peter - df$ROMas.Mary, na.rm = TRUE) 

ggMarginal(p, type = "density", fill = "gray", color = "black")



# ICC (consistency) using the irr package:---------
irr::icc(as.matrix(df[, c("ROMas.Peter", "ROMas.Mary")]), 
    model = "oneway", type = "consistency")
# 0.851

# ICC with psych package-------
library(psych)
psych::ICC(df[, c("ROMas.Peter", "ROMas.Mary")])
# ICC1 = Single_raters_absolute in psych output
# = 0.85

# _Verify with lmer:--------
df_long <- df %>% 
  mutate(ID = row_number()) %>%
  dplyr::select(ID, ROMas.Peter, ROMas.Mary) %>% 
  pivot_longer(cols = c(ROMas.Peter, ROMas.Mary), 
               names_to = "Rater", values_to = "ROM") %>% 
  mutate(Rater = factor(Rater))
head(df_long)
df

m5.2 <- lmer(ROM ~ (1 | ID), data = df_long)
summary(m5.2)
print(VarCorr(m5.2), comp = "Variance")
# Groups   Name        Variance
# ID       (Intercept) 270.99  
# Residual              47.35 

# ICC = 
270.99 / (270.99 + 47.35) # 
# 0.8512597
# perfect match.



# _Verify using rethinking---------
df_long$ID

tic()
m5.1 <- ulam(
  alist(
    # Likelihood
    ROM ~ dnorm(mu, sigma),
    
    # Patient-specific intercepts (random effects)
    mu <- a[ID],  
    a[ID] ~ dnorm(a_bar, sigma_ID),  # Hierarchical structure for patients
    
    # Priors for hyperparameters
    a_bar ~ dnorm(66, 20),  # Population-level mean
    sigma_ID ~ dunif(0,20),  # Between-patient standard deviation
    sigma ~ dunif(0,20)  # Residual standard deviation
  ), 
  data = df_long, 
  chains = 4, cores = 4
)
toc() # 7s

precis(m5.1, depth = 2)

post <- extract.samples(m5.1)
var_patients <- mean(post$sigma_ID^2)  # Between-patient variance
var_residual <- mean(post$sigma^2)     # Residual variance
var_patients / (var_patients + var_residual) # ICC
# 0.846323 (vs 0.851)
# not too bad


# ICC (agreement) bias 5 degrees Mary-Peter ~ 5:--------

#_introduce bias:--------
# mean diff before in df:
mean(df$ROMas.Mary - df$ROMas.Peter, na.rm = TRUE) # - 1.22
# hence should be -1.22 + 5 = 3.78 afterwards
length(df$ROMas.Mary) # 50
length(df$ROMas.Peter) # 50

df_long_bias <- df_long %>%
  mutate(ROM = ROM + ifelse(Rater == "ROMas.Mary", 15, 0))
head(df_long_bias) # seems to have worked.

# mean Mary
mean(df_long_bias$ROM[df_long_bias$Rater == "ROMas.Mary"]) # 69.98
# mean Peter
mean(df_long_bias$ROM[df_long_bias$Rater == "ROMas.Peter"], na.rm = TRUE) # 66.2
# diff
mean(df_long_bias$ROM[df_long_bias$Rater == "ROMas.Mary"]) - 
  mean(df_long_bias$ROM[df_long_bias$Rater == "ROMas.Peter"], na.rm = TRUE) # 3.78
# 3.78

# scatterplot
# put in wide format
df_wide <- df_long_bias %>%
  pivot_wider(names_from = Rater, values_from = ROM)

# plot
df_wide %>%
  ggplot(aes(x = ROMas.Peter, y = ROMas.Mary)) +
  geom_point() + 
  geom_abline(intercept = 0, 
              slope = 1, 
              color = "red") +
  geom_abline(intercept = 15, 
              slope = 1, 
              color = "blue") +
  theme_minimal() +
  ggtitle("ROMas.Peter vs. ROMas.Mary") +
  theme(plot.title = element_text(hjust = 0.5))



#_rethinking---------
library(rethinking)
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
mean(post$beta[,1] - post$beta[,2])  # 3.701551 # makes sense with the above

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
# 0.8158461 (sigma_alpha ~ dexp(0.5))
# 0.8403384 (sigma_alpha ~ dexp(1))

str(df_long)
# tibble [100 × 3] (S3: tbl_df/tbl/data.frame)
# $ ID   : int [1:100] 1 1 2 2 3 3 4 4 5 5 ...
# $ Rater: Factor w/ 2 levels "ROMas.Mary","ROMas.Peter": 2 1 2 1 2 1 2 1 2 1 ...
# $ ROM  : num [1:100] 66 75 65 68 96 87 75 85 62 59 ...

# _Verify with irr:----
# use df and introduce bias there too 
head(df) # wide
dim(df)
df_bias <- df %>%
  mutate(ROMnas.Mary = ROMnas.Mary + 5)

#order(df_long$ROM[df_long$Rater == "ROMas.Mary"])
#order(df_bias$ROMas.Mary) # identical!

# ??? find error...
#irr::icc(as.matrix(df_bias[, c("ROMas.Peter", "ROMas.Mary")]), 
#    model = "oneway", type = "consistency")
# ???

# _psych package------
# needs wide format
df_wide <- df_long_bias %>%
  pivot_wider(names_from = Rater, values_from = ROM)
df_wide_values <- df_wide %>% select(-ID)
psych::ICC(df_wide_values) # ICC1 = 0.83

# _lmer------
m5.3 <- lmer(ROM ~ (1 | ID) + (1 | Rater), data = df_long_bias)
summary(m5.3)
print(VarCorr(m5.3), comp = "Variance")
# Groups   Name        Variance
# ID       (Intercept) 270.882 
# Rater    (Intercept)   6.193 
# Residual              47.557 


# ICC (Single_random_raters) = ICC 2 in psych output
270.882 / (270.882 + 6.193 + 47.557) # 
# 0.8344279
# how can this be higher?

# ICC (Single_fixed_raters) = ICC3 in psych output
270.882 / (270.882 + 47.557) #
# 0.85



# summary-----------

library(conflicted, tidyverse, flextable)

# Ensure select() from dplyr is used
conflicts_prefer(dplyr::select)

# Unbiased ICC Calculation
df_wide_unbiased <- df_long %>%
  pivot_wider(names_from = Rater, values_from = ROM)
df_wide_values_unbiased <- df_wide_unbiased %>% select(-ID)
icc_results_unbiased <- psych::ICC(df_wide_values_unbiased)

# Extract relevant ICC values
icc_unbiased_df <- icc_results_unbiased$results %>%
  dplyr::select(type, ICC) %>%
  rename(`Unbiased ICC` = ICC)

# Biased ICC Calculation
df_wide_biased <- df_long_bias %>%
  pivot_wider(names_from = Rater, values_from = ROM)
df_wide_values_biased <- df_wide_biased %>% select(-ID)
icc_results_biased <- psych::ICC(df_wide_values_biased)

# Extract relevant ICC values
icc_biased_df <- icc_results_biased$results %>%
  dplyr::select(type, ICC) %>%
  rename(`Biased ICC` = ICC)

icc_merged_df <- left_join(icc_unbiased_df, 
                           icc_biased_df, 
                           by = "type") %>%
  slice(1:3)

ft <- flextable(icc_merged_df) %>%
  flextable::set_header_labels(type = "ICC Type") %>%
  flextable::set_caption("Intraclass Correlation Coefficients - Unbiased vs. Biased") %>%
  flextable::set_table_properties(width = .5, layout = "autofit")
ft

# verify biased ICC with lmer:------
m1 <- lmer(ROM ~ (1 | ID), data = df_long_bias)
m2 <- lmer(ROM ~ (1 | ID) + (1 | Rater), data = df_long_bias)

print(VarCorr(m1), comp = "Variance")
# Groups   Name        Variance
# ID       (Intercept) 223.89  
# Residual             141.55  
# sum of variances:
223.89 + 141.55

print(VarCorr(m2), comp = "Variance")
# Groups   Name        Variance
# ID       (Intercept) 270.882 
# Rater    (Intercept)  93.992 
# Residual              47.557 
# sum of variances
270.882 + 93.992 + 47.557

# ICC1:
223.89 / (223.89 + 141.55) # 0.6126587
# ICC2 (agreement in MIM):
270.882 / (270.882 + 93.992 + 47.557) # 0.6567935
# ICC3 (consistency in MIM):
270.882 / (270.882 + 47.557) # 0.8506559


