# Mulligan

library(pacman)
p_load(
  tidyverse,
  readxl,
  lme4
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
dim(df)
colnames(df) <- c("ID", "Group", "Week_0", 
                "Week_4", "Week_13", "Week_26")
df

# Omit missing values since they will be thrown out anyways when using lmer!
df <- na.omit(df) # 8 rows (patients) are out!

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
    time = recode(time,
                  "Week_0" = 0,
                  "Week_4" = 4,
                  "Week_13" = 13,
                  "Week_26" = 26)
  )

# optional: Zeit als Faktor
df_long$time_f <- factor(df_long$time, levels = c(0, 4, 13, 26))

df_long

model <- lmer(headache_frequency ~ Group * time_f + (1 | ID), 
              data = df_long)
summary(model)
