#ex 9 LMM

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

hfw0 <- df %>% dplyr::filter(Group == "Ex") %>%
  dplyr::select(`Headache frequency`) %>% # = headache frequency at Week 0
  pull() %>% as.numeric()
hist(hfw0)
shapiro.test(hfw0) # test decision would be "not normal"

df$Group

hfw0_2 <- df %>% dplyr::select(2,3) %>% # Group and headache frequency at Week 0
       dplyr::filter(Group == "MMT+ex") %>%
       dplyr::pull() %>% 
       as.numeric()
hist(hfw0_2)
shapiro.test(hfw0_2) # test decision would be "not normal"