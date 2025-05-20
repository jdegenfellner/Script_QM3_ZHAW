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

# Let's use the values for the affected side (as):
# Introduce biases 5, 15, 15 degrees:

df <- df %>%
  dplyr::mutate(
    ROMas.Mary_bias_5 = ROMas.Mary + 5,
    ROMas.Mary_bias_15 = ROMas.Mary + 15,
    ROMas.Mary_bias_35 = ROMas.Mary + 35
  )

cor(df$ROMas.Peter, df$ROMas.Mary)
cor(df$ROMas.Peter, df$ROMas.Mary_bias_5)
cor(df$ROMas.Peter, df$ROMas.Mary_bias_15)
cor(df$ROMas.Peter, df$ROMas.Mary_bias_35)
# -> does not change!
