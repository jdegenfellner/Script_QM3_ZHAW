library(pacman)
p_load(tidyverse, readxl)

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

# 1) Fit the linear model-----------
mod <- lm(ROMas.Mary ~ ROMas.Peter, data = df)
summary(mod)
# between -22 degrees and +18 degrees difference using the model

# 2) Residuals-------------
df <- df %>%
  mutate(pred_mary = predict(mod),
         residuals = ROMas.Mary - pred_mary)

ggplot(df, aes(x = pred_mary, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(x = "Predicted Mary's ROM", y = "Residuals")

# 3) MAD-----------
MAD <- mean(abs(df$ROMas.Mary - df$pred_mary))
MAD
