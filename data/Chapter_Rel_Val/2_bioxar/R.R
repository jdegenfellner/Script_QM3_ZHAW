# change working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# -> change path for Rmd-File...

library(pacman)
conflicts_prefer(rethinking::logit)
p_load(
  tidyverse,
  readxl,
  psych)

df <- read_excel("Test-retest dataset.xlsx",
                 sheet = "Sheet1",
                 skip = 1)
head(df)
colnames(df)

df_1 <- df %>% dplyr::select(`Plantar flexion moment (N.m)`, `...11`)
head(df_1)
tail(df_1)
dim(df_1)

ggplot(df_1, aes(x = `Plantar flexion moment (N.m)`, y = `...11`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "Scatterplot of Plantar flexion moment",
    x = "Plantar flexion moment (N.m)",
    y = "Test-retest"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Looks rather nice
# would be interesting what influence the largest 2 and the smallest observation have

colnames(df_1) <- c("test", "retest")

# Mean Session 1
mean(df_1$test, na.rm = TRUE)
# 124.18 check
sd(df_1$test, na.rm = TRUE)
# 15.91872 check

# Mean Session 2
mean(df_1$retest, na.rm = TRUE)
# 126.1748 check
sd(df_1$retest, na.rm = TRUE)
# 20.41306

# Change
mean(df_1$retest - df_1$test, na.rm = TRUE)
# 1.99477 check
sd(df_1$retest - df_1$test, na.rm = TRUE)
# 8.160476 check

# The p-value colum is not needed, as well as the d (effect size)

# create long format for ICC calculation
df_long <- df_1 %>%
  pivot_longer(cols = everything(), names_to = "Timepoint", values_to = "value") 
head(df_long)

psych::ICC(df_1)

# In Table 1, we find the values for ICC1k: 0.95 (95% CI: 0.86-0.98)

# Standard error of measurement:
# SEM = SD_difference/sqrt(2)
sd(df_1$retest - df_1$test, na.rm = TRUE)/sqrt(2)
# 5.770328 vs. 4.2 in the paper

# In "Measurement in Medicine", p. 112, the authors state warn against
# the formula the paper used to calculate the SEM:
# SEM = SD_pooled*sqrt(1-ICC)
SD_pooled <- sqrt((sd(df_1$test, na.rm = TRUE)^2 + sd(df_1$retest, na.rm = TRUE)^2)/2) # correct?
SEM <- SD_pooled*sqrt(1 - 0.95)
SEM
# 4.092977 vs. 4.2 in the paper (rounding error?)

# Formula 5.7 in the book:
# sigma_y is the total variance
# lets get the variance components with lmer:
dim(df_long)
df_long$ID<- rep(1:17, each=2)
mod <- lmer(value ~ Timepoint + (1|ID), data = df_long)
summary(mod)
# check ICC1k:
301.8/(301.8 + 33.3/2)
# 0.9477155 check

# -> sigma_y = SD_pooled = 
sqrt(301.8 + 33.3) # = 18.30574

# or this?
sqrt(301.8 + 33.3/2) # 17.84517
# SEM = 
sqrt(301.8 + 33.3/2)*sqrt(1 - 0.95) # 3.990301

# -> SEM was not exactly replicable

# MCD, Minimally Detectable Difference:
# MCD = 
1.96 * 4.092977 * sqrt(2)
# 11.34515 vs 11.6 in the paper, rather close.

1.96 * 4.2 * sqrt(2)
# 11.64181

# Coefficient of Variation:
# The CV was calculated for each participant as (within-subject SD / mean) * 100, 
# with the mean of values from all participants used as the test-
# retest CV (e.g., [24]).

df_1$mean_i <- rowMeans(df_1, na.rm = TRUE) # mean of test and retest for each person.
df_1$sd_within_i <- abs(df_1$test - df_1$retest) / sqrt(2)
df_1$cv_i <- (df_1$sd_within_i / df_1$mean_i) * 100

# Test-Retest CV
mean(df_1$cv_i, na.rm = TRUE)
sd(df_1$cv_i, na.rm = TRUE)
# 95% CI:
mean(df_1$cv_i, na.rm = TRUE) + c(-1,1)*1.96*sd(df_1$cv_i, na.rm = TRUE)/sqrt(17)
# 2.195750 5.014306 # check



df_1$mean_i <- rowMeans(df_1, na.rm = TRUE) # mean of test and retest for each person.
df_1$sd_within_i <- sd(df_1$test - df_1$retest) / sqrt(2)
df_1$cv_i <- (df_1$sd_within_i / df_1$mean_i) * 100

# Test-Retest CV
mean(df_1$cv_i, na.rm = TRUE)
sd(df_1$cv_i, na.rm = TRUE)
# 95% CI:
mean(df_1$cv_i, na.rm = TRUE) + c(-1,1)*1.96*sd(df_1$cv_i, na.rm = TRUE)/sqrt(17)
# 2.195750 5.014306 # check

