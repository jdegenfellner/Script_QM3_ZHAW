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

# ---
# Nur Peter und Mary Spalten behalten
df_icc <- df %>% dplyr::select(ROMas.Peter, ROMas.Mary)
dim(df_icc)

psych::ICC(df_icc)


df_outlier <- df_icc
set.seed(123)
outlier_rows <- sample(1:nrow(df_outlier), 3) # 31 15 14
df_outlier$ROMas.Peter[outlier_rows] <- df_outlier$ROMas.Peter[outlier_rows] + 30

# nice scatterplot
df_outlier %>% 
  ggplot(aes(x = ROMas.Peter, y = ROMas.Mary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot with Outliers")

psych::ICC(df_outlier)

# -> notably lower
