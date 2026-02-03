## Biostatistical Modeling for Agricultural Science
## Pre-Course Day 1 - Introducing the Tidyverse

# The tidyverse is a collection of packages which share a common design
# philosophy and syntax (which is different in some important ways from base R).
# It is especially useful for "data wrangling", or the processes of modifying
# and organizing data in preparation for subsequent analysis (or other use).

# This lesson only gives a very brief scratch of what you can do. 
# For more information and inspiration, 
# see here: https://r4ds.hadley.nz/
# And here: https://posit.cloud/learn/recipes
# And here: https://rstudio.github.io/cheatsheets/

#### Getting Started ####

# Packages need to be installed once with each update of R, and again whenever
# you need to update the package itself:
install.packages('tidyverse')

# Packages need to be loaded for each R session:
library(tidyverse)

# Set working directory

# Import data
canola <- read.csv('brandle_rape.csv')
canola <- tibble(canola)
canola

#### Data Wrangling with the Tidyverse ####

# Pipes (%>% or |>) take the data.frame on the left and feed it to the function
# on the right (to insert a pipe, press CTRL + SHIFT + M). So this:
canola |>  summary()

# is the same as this:
summary(canola)

# Some common data wrangling tasks:
# renaming columns
canola |> 
  rename(genotype = gen)

# subsetting
canola |> 
  filter(loc == 'Souris')

# ordering or arranging
canola |> 
  arrange(gen)

# moving columns
canola |> 
  relocate(loc, .before = gen)

# keeping/removing columns
canola |> 
  select(loc, gen, year_1983)

canola |> 
  select(!year_1984:year_1985)

# Two of the most important functions for data wrangling are mutate and 
# summarize. Mutate is used to modify existing variables or to create new ones.

# Modify the class type of a single variable
canola |> 
  mutate(gen = factor(gen)) 

# Modify the class type of several variables
canola |> 
  mutate(across(gen:loc, factor), 
         across(c(year_1983, year_1984, year_1985), as.numeric))


# summarize variables using a grouping factor
canola |> 
  group_by(gen) |> 
  summarize(mean_1983 = mean(year_1983, na.rm = T), 
            sd_1983 = sd(year_1983, na.rm = T), 
            n_1983 = n())

# pivoting from wide to long format (and back)
canola

# pivoting to long format means taking a set of columns and rearranging them 
# into just two columns: one for names and one for values
canola |> 
  pivot_longer(cols = year_1983:year_1985, 
               names_to = 'year', 
               values_to = 'yield')

# pivoting from long to wide is just the reverse
canola |> 
  pivot_longer(cols = year_1983:year_1985, 
               names_to = 'year', 
               values_to = 'yield') |> 
  pivot_wider(names_from = 'year', 
              values_from = 'yield')

#### Putting it All Together ####
# Let's create a long form version of our dataset, with factor variables 
# named "genotype" and "location", and with yield values converted from 
# pounds per acre to kg per ha:
canola_long <- canola |>
  rename(genotype = gen, location = loc) |> 
  mutate(across(genotype:location, factor)) |> 
  pivot_longer(cols = year_1983:year_1985, 
               names_to = 'year', 
               values_to = 'yield') |> 
  mutate(yield = yield*1.12, 
         year = str_remove(year, 'year_'), 
         year = factor(year)) 

canola_long

#### Merging Tables and Working with Dates ####
# Create a "dictionary" containing the dates at which the canola was harvested
# in each year (these data are completely made up).
harvest_dates <- data.frame(year = c(1983, 1984, 1985), 
                            harvest = c('1983-09-29', '1984-10-05', '1985-09-24')) |> 
  mutate(year = factor(year))
 
harvest_dates

# Join the two data sets
canola_long <- canola_long |> 
  left_join(harvest_dates)

canola_long 

# Convert harvest to a date
canola_long <- canola_long |> 
  mutate(harvest = ymd(harvest))

canola_long

year(canola_long$harvest)
month(canola_long$harvest)
month(canola_long$harvest, label = T)
day(canola_long$harvest)
yday(canola_long$harvest)

canola_long$harvest[2] - canola_long$harvest[1]



