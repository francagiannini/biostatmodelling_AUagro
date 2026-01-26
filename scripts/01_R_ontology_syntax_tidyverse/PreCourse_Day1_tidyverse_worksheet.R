## Biostatistical Modeling for Agricultural Science
## Pre-Course Day 1 - Introducing the Tidyverse

# The tidyverse is a collection of packages which share a common design
# philosophy and syntax (which is different in some important ways from base R).
# It is especially useful for "data wrangling", or the processes of modifying
# and organizing data in preparation for subsequent analysis (or other use).

# This lesson only gives a very brief scratch of what you can do. 
# For more information and inspiration, 
# see here: https://r4ds.hadley.nz/
# and here: https://posit.cloud/learn/recipes
# and here: https://rstudio.github.io/cheatsheets/

#### Getting Started ####

# Packages need to be installed once with each update of R, and again whenever
# you need to update the package itself:
install.packages('tidyverse')

# Packages need to be loaded for each R session:
library(tidyverse)

# Set working directory
setwd()

# Import data
canola <- read.csv('brandle_rape.csv')
canola <- tibble(canola)
canola








