

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  # drake support
  conflicted,
  dotenv,
  drake,
  # data management
  haven,
  janitor,
  magrittr,
  labelled,
  # data analysis
  tidyverse,
  tidyselect,
  survey,
  splines,
  scales,
  # reporting
  officer,
  glue,
  flextable,
  devEMF,
  magick,
  paletteer,
  patchwork,
  rmarkdown,
  english,
  officer,
  officedown
)

pacman::p_load_gh('bcjaeger/table.glue', update = FALSE)

conflicted::conflict_prefer("roc",       "pROC")
conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflict_prefer("slice",     "dplyr")
conflicted::conflict_prefer('summarise', 'dplyr')
conflicted::conflict_prefer("gather",    "tidyr")
conflicted::conflict_prefer("set_names", "purrr")
