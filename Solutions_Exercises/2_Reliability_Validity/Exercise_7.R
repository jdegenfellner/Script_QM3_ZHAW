library(pacman)
p_load(tidyverse, readxl, lme4, performance, car)

# Read file
url <- "https://raw.githubusercontent.com/jdegenfellner/Script_QM2_ZHAW/main/data/chapter%205_assignment%201_2_wide.xls"
temp_file <- tempfile(fileext = ".xls")
download.file(url, temp_file, mode = "wb")  # mode="wb" is important for binary files
df <- read_excel(temp_file)
head(df)

# As in the book, let's randomly select 50 patients.
set.seed(123)
df <- df %>% sample_n(50)
dim(df)

df_long <- df %>% 
  mutate(ID = row_number()) %>%
  dplyr::select(ID,ROMas.Peter, ROMas.Mary) %>% 
  pivot_longer(cols = c(ROMas.Peter, ROMas.Mary), 
               names_to = "Rater", values_to = "ROM") %>% 
  mutate(Rater = factor(Rater))
df_long
unique(df_long$Rater)

df_long_bias <- df_long %>%
  mutate(ROM = ROM + ifelse(Rater == "ROMas.Mary", 5, 0))
head(df_long_bias)

# Model 1 (for ICC1)-----------
mod1 <- lmer(ROM ~ (1|ID), data = df_long_bias)
summary(mod1)

# Model 2 (for ICC2/3)-----------
mod2 <- lmer(ROM ~ (1|ID) + (1|Rater), data = df_long_bias)
summary(mod2)


# 1) Check model 1
check_model(mod1) # # Linearity is not perfect
check_model(mod1, check = "pp_check") # # PPC seems a bit off
check_model(mod1, check = "normality")
check_model(mod1, check = "qq")

library(qqplotr)
res <- residuals(mod1)
ggplot(data.frame(res = res), aes(sample = res)) +
  qqplotr::stat_qq_band(distribution = "norm", alpha = 0.3) +
  qqplotr::stat_qq_line(distribution = "norm") +
  qqplotr::stat_qq_point(distribution = "norm") +
  labs(title = "QQ Plot with Confidence Band")
# Normality seems ok


# 2) Check model 2
check_model(mod2)
check_model(mod2, check = "pp_check")
check_model(mod2, check = "normality")
check_model(mod2, check = "qq")

res <- residuals(mod2)
ggplot(data.frame(res = res), aes(sample = res)) +
  qqplotr::stat_qq_band(distribution = "norm", alpha = 0.3) +
  qqplotr::stat_qq_line(distribution = "norm") +
  qqplotr::stat_qq_point(distribution = "norm") +
  labs(title = "QQ Plot with Confidence Band")
# normality seems ok

# WAIC(mod1)
# WAIC(mod2) # slightly better
# AIC(mod1)
# AIC(mod2) # slightly better
# BIC(mod1)
# BIC(mod2) # not even slightly better than mod1

# compare residuals of both models
library(patchwork)  # für nebeneinanderstellen

# 1) Residuen extrahieren + in DataFrame
resid_df <- bind_rows(
  data.frame(model = "mod1", resid = residuals(mod1)),
  data.frame(model = "mod2", resid = residuals(mod2))
)

# 2) same range
resid_range <- range(resid_df$resid)

# 3) Plot 1: Histogram mod1
p1 <- resid_df %>% dplyr::filter(model == "mod1") %>%
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  xlim(resid_range) +
  labs(title = "Residuen: mod1", x = "Residual", y = "Häufigkeit") +
  theme_minimal()

# 4) Plot 2: Histogram mod2
p2 <- resid_df %>% dplyr::filter(model == "mod2") %>%
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  xlim(resid_range) +
  labs(title = "Residuen: mod2", x = "Residual", y = "Häufigkeit") +
  theme_minimal()

# 5) show next to each other
p1 + p2

summary(residuals(mod1))
summary(residuals(mod2))
# -> Residuals seem to be slightly tighter in mod2