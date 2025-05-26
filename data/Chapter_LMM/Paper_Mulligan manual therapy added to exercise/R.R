# Mulligan

library(pacman)
p_load(
  tidyverse,
  readxl
)

# set the working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

df <- read_excel("1-s2.0-S1836955324000572-mmc1.xls", 
                 sheet = 2)
df

# hist headache frequency, week 0:

df_sub <- df[3:dim(df)[2], 1:6]

df_sub$`Headache frequency` <- as.numeric(df_sub$`Headache frequency`)

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
length(df_sub$`Headache frequency`) # What is the power at n=44?
shapiro.test(df_sub$`Headache frequency`) # p-value = 0.006539
# According to this test, headache frequency is not normally distributed
# However, in Table 2 of the paper, Means and SD of all variables are presented
# as if the variables are normally distributed.
