# Render the reproducible manuscript and regenerate all outputs.
# Run this file from the root of the repository.

required_packages <- c(
  "tidyverse", "knitr", "kableExtra", "scales", "sf",
  "rnaturalearth", "rnaturalearthdata", "patchwork", "ggrepel",
  "officer", "flextable", "gridExtra", "rmarkdown"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

rmarkdown::render(
  input = "manuscript/cervical_cancer_congo_reproducible_manuscript.Rmd",
  output_format = "word_document",
  output_dir = "outputs"
)
