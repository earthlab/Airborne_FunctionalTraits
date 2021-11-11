# Launch python to use scikitlearn from Rstudio to enable parallelization of
# distance functions in diversity metrics calculations

library(tidyverse)
library(reticulate)
library(knitr)

reticulate::conda_list()
reticulate::use_condaenv("py3.8",required=TRUE)
reticulate::py_discover_config()

