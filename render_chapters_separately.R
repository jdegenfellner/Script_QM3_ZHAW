# Lade benötigte Pakete
library(rmarkdown)
library(bookdown)

# Kapitel 01 rendern
render(
  input = "01-Reliability_Validity.Rmd",
  output_format = bookdown::pdf_document2(latex_engine = "xelatex"),
  output_file = "Chapter_01_Reliability_Validity.pdf"
)

# Kapitel 02 rendern
render(
  input = "02-Further_Regression_Methods.Rmd",
  output_format = bookdown::pdf_document2(latex_engine = "xelatex"),
  output_file = "Chapter_02_Further_Regression_Methods.pdf"
)