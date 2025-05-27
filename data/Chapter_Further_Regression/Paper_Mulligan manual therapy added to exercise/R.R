# Mulligan

library(pacman)
p_load(
  tidyverse,
  readxl,
  lme4,
  emmeans,
  performance
)

# set the working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# READ----------
df <- read_excel("1-s2.0-S1836955324000572-mmc1.xls", 
                 sheet = 2)
df

# hist headache frequency, week 0:

df_sub <- df[3:dim(df)[1], 1:6] %>% dplyr::filter(Group == "Ex")

df_sub$`Headache frequency` <- as.numeric(df_sub$`Headache frequency`)

unique(df_sub$`Headache frequency`) # 5 7 6 4 8

ggplot(df_sub, aes(x = `Headache frequency`)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 10, 
                 fill = "lightblue", 
                 color = "black") +
  geom_density(na.rm = TRUE, 
               color = "red", 
               linewidth = 1.2) +
  labs(
    title = "Headache Frequency at Week 0",
    x = "Headache Frequency",
    y = "Density"
  ) +
  theme_minimal()

# normally distributed?
length(df_sub$`Headache frequency`) # What is the power at n=33?
shapiro.test(df_sub$`Headache frequency`) # p-value = 2.651e-06
# According to this test, headache frequency is not normally distributed
# However, in Table 2 of the paper, Means and SD of all variables are presented
# as if the variables are normally distributed.


df$Group
df_sub_2 <- df[3:dim(df)[1], 1:6] %>% dplyr::filter(Group == "MMT+ex")

df_sub_2$`Headache frequency` <- as.numeric(df_sub$`Headache frequency`)

ggplot(df_sub_2, aes(x = `Headache frequency`)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 10, 
                 fill = "lightblue", 
                 color = "black") +
  geom_density(na.rm = TRUE, 
               color = "red", 
               linewidth = 1.2) +
  labs(
    title = "Headache Frequency at Week 0",
    x = "Headache Frequency",
    y = "Density"
  ) +
  theme_minimal()
# normally distributed?
length(df_sub_2$`Headache frequency`) # What is the power at n=33?
shapiro.test(df_sub_2$`Headache frequency`) # p-value = 0.009306



# Prepare data set nicely for verification of main outcome----------
df
df <- df[3:dim(df)[1], 1:6]
head(df)
dim(df) # 99 participants, 33 in each group at baseline (week 0)
colnames(df) <- c("ID", "Group", "Week_0", 
                  "Week_4", "Week_13", "Week_26")
df
  
# Omit missing values since they will be thrown out anyways when using lmer!
df <- na.omit(df) # 8 rows (patients) are out!
dim(df) # 91 participants without missing values.
  
# assuming your data has columns:
# - headache_frequency (numeric)
# - time (factor or numeric)
# - group (factor)
# - subject (factor or numeric ID)
  
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

df_long$Group <- as.factor(df_long$Group)  
df_long$ID <- droplevels(factor(df_long$ID))
df_long$time_f <- factor(df_long$time, levels = c(0, 4, 13, 26))
levels(df_long$time_f)

length(unique(df_long$ID)) # 91 participants

# spaghetti plot----------
ggplot(df_long, aes(x = time, y = headache_frequency, group = ID, color = Group)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(0, 4, 13, 26)) +
  labs(
    title = "Spaghetti Plot: Headache Frequency Over Time",
    x = "Week",
    y = "Headache Frequency",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "top"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

# lmer---------
model <- lmer(headache_frequency ~ Group * time_f + (1 | ID), 
              data = df_long)
# ref: week 0
summary(model)
ranef(model)  # random effects
re <- ranef(model)
str(re$ID) # 91 obs, 91 participants

# constrasts--------
emm <- emmeans(model, ~ Group | time_f)  # Kontraste zwischen Gruppen zu jedem Zeitpunkt
contrast(emm, method = "pairwise", adjust = "bonferroni")  # oder "tukey", "none" etc.
summary(contrast(emm, method = "pairwise", adjust = "bonferroni"), infer = c(TRUE, TRUE))  # TRUE für CI und p-Werte

ranef(model)  # random effects

check_model(model)
library(car)
qqPlot(residuals(model))
check_model(model, check = "pp_check")



# rethinking---------
library(rethinking)

dat <- list(
  H = df_long$headache_frequency,
  group = as.integer(as.factor(df_long$Group)),   # 1 = Ex, 2 = MMT+ex, 3 = Sham+ex
  time = as.integer(df_long$time_f),              # 1 = Woche 0, ..., 4 = Woche 26
  ID = as.integer(as.factor(df_long$ID)),
  N = nrow(df_long),
  N_ID = length(unique(df_long$ID)),
  N_group = length(unique(df_long$Group)),
  N_time = length(unique(df_long$time_f))
)

# interaction index
dat$interaction <- (dat$group - 1) * dat$N_time + dat$time

m <- ulam(
  alist(
    H ~ normal(mu, sigma),
    mu <- alpha +
      beta_group[group] +
      beta_time[time] +
      beta_interaction[interaction] +
      u[ID],
    # Zufällige Intercepts für IDs
    u[ID] ~ normal(0, sigma_ID),
    # Priors
    alpha ~ normal(0, 10),
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

precis(m, depth = 2)

# mean treatment effect: week4,  (MMT+ex) - (Sham+ex)----------
post <- extract.samples(m)
#lapply(post, head)

# MMT+ex (group = 2), time = 2 → index = (2−1)×4 + 2 = 6
# Sham+ex (group = 3), time = 2 → index = (3−1)×4 + 2 = 10

group_diff <- post$beta_group[,2] - post$beta_group[,3]
interaction_diff <- post$beta_interaction[,6] - post$beta_interaction[,10]
treatment_diff_week4 <- group_diff + interaction_diff
precis(data.frame(treatment_diff_week4))
