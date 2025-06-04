# ex 10 LLM

library(readxl)
library(tidyverse)

url <- "https://raw.githubusercontent.com/jdegenfellner/Script_QM3_ZHAW/main/data/Chapter_Further_Regression/Paper_Mulligan%20manual%20therapy%20added%20to%20exercise/1-s2.0-S1836955324000572-mmc1.xls"
temp_file <- tempfile(fileext = ".xls")
download.file(url, destfile = temp_file, mode = "wb")
df <- suppressMessages(
  suppressWarnings(
    readxl::read_xls(temp_file, sheet = 2)
  )
)

df <- df[3:dim(df)[1], 1:6]
head(df)
dim(df)
colnames(df) <- c("ID", "Group", "Week_0", 
                  "Week_4", "Week_13", "Week_26")
df


# Omit missing values since they will be thrown out anyways when using lmer!
df <- na.omit(df) # 8 rows (patients) are out!
dim(df) # 91

df_long <- df %>%
  pivot_longer(
    cols = starts_with("Week_"),
    names_to = "time",
    values_to = "headache_frequency"
  ) %>%
  mutate(
    headache_frequency = as.numeric(headache_frequency),
    time = dplyr::recode(time,
                         "Week_0" = 0,
                         "Week_4" = 4,
                         "Week_13" = 13,
                         "Week_26" = 26)
  )

df_long$ID <- as.factor(df_long$ID)
df_long$time_f <- factor(df_long$time, levels = c(0, 4, 13, 26))
df_long



library(rethinking)

df_long <- df_long %>%
  dplyr::group_by(Group, time) %>%
  dplyr::mutate(interaction = cur_group_id()) %>%
  dplyr::ungroup()
# cur_group_id() gives a unique numeric identifier for the current group.

dat <- list(
  H = df_long$headache_frequency,
  group = as.integer(as.factor(df_long$Group)),   # 1 = Ex, 2 = MMT+ex, 3 = Sham+ex
  time = as.integer(df_long$time_f),              # 1 = Woche 0, ..., 4 = Woche 26
  ID = as.integer(as.factor(df_long$ID)),
  N = nrow(df_long),
  N_ID = length(unique(df_long$ID)),
  N_group = length(unique(df_long$Group)),
  N_time = length(unique(df_long$time_f)),
  interaction = df_long$interaction
)

# m --------
m <- ulam(
  alist(
    H ~ normal(mu, sigma),
    mu <- alpha +
      beta_group[group] + beta_time[time] + beta_interaction[interaction] + u[ID],
    # random intercept for each participant
    u[ID] ~ normal(0, sigma_ID),
    # Priors
    alpha ~ normal(5, 1.5),
    beta_group[group] ~ normal(0, 2),
    beta_time[time] ~ normal(0, 2),
    beta_interaction[interaction] ~ normal(0, 2), # difficult
    sigma ~ exponential(1),
    sigma_ID ~ exponential(1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
post_m <- extract.samples(m, n = 1000)
lapply(post_m, head)
mean(post_m$alpha) # = 4.65483; overall intercept

# m (alpha and u_i combined):-------
m_2 <- ulam(
  alist(
    H ~ normal(mu, sigma),
    mu <- beta_group[group] + beta_time[time] + beta_interaction[interaction] + u[ID], # u[ID] contains alpha
    # random intercept for each participant
    u[ID] ~ normal(alpha, sigma_ID),
    # Priors
    alpha ~ normal(5, 1.5),
    beta_group[group] ~ normal(0, 2),
    beta_time[time] ~ normal(0, 2),
    beta_interaction[interaction] ~ normal(0, 2), # difficult
    sigma ~ exponential(1),
    sigma_ID ~ exponential(1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
post_m_2 <- extract.samples(m_2, n = 1000)
lapply(post_m_2, head)
mean(post_m_2$alpha) # = 4.821562;  -> not too different when using the same priors

precis(post_m_2, depth = 1) # alpha slightly higher, why? sigma identical
precis(post_m, depth = 1)
