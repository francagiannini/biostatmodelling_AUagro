# Data can be found here: https://datarepository.wolframcloud.com/resources/Prussian-Horse-Kick-Data

# The data give the number of soldiers in the Prussian cavalry killed by 
# horse kicks, by corp membership and from 1875 to 1894. 

#
#
#

# Packages ----
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)

# Read in the data ----
horsekicks <- read.csv("C:/Users/au721810/OneDrive - Aarhus universitet/Prussian-Horse-Kick-Data.csv")
head(horsekicks)

# Wrangle the data ----
horsekicks <- horsekicks |>
  pivot_longer(GC:C15, names_to = "corp", values_to = "count") |>
  mutate(Year = substr(Year, start=13, stop=16)) |>
  mutate(Year = as.numeric(Year))


# Figure ----
horsekicks |>
  ggplot(aes(x=corp, y= count, fill=Year)) +
  theme_classic() +
  geom_col() +
  scale_y_continuous(expand=c(0,0)) 
  
# Model ----
horse_model <- glmmTMB(count ~ corp + (1|Year),
                     family = poisson(link="log"),
                     data=horsekicks)

# Assumptions ----
simulateResiduals(horse_model) |> plot()

# In Poisson models, it is important to check that there is no overdispersion
# Poisson assumes that the mean is equal to std. It is strict about it.
simulateResiduals(horse_model) |> testDispersion()

# Results ----
joint_tests(horse_model)
summary(horse_model)

# Post-hoc tests ----
emm <-
  emmeans(horse_model, pairwise~corp, 
        type="response", # To get the outcome in the scale of the response
        adjust="fdr") # For a more lenient p-value adjustment method
emm

emmeans(horse_model, ~corp, type="response") |> multcomp::cld() 

# Notice that the pairwise comparisons are given as odds ratios.
# This is because we have a log-link. If you run the emmeans above without the
# type="response", you can see you get absolute differences.

# If the ratio is 1, the two corps are not different. 
# <1, first corp in order is smaller
# >1, first corp is larger

#
#
# PRACTICE! ----

# Remember the germination examples from Wednesday? Re-do the models, but now
# with a different probability distribution!
# Note - also explore the models after fitting them. See whether anything 
# changes.


# EXAMPLE 1 ----

# Load your data
# Replace 'path_to_your_data.csv' with the actual path to your data file
atriplex <- read.table('2025 Biostatistical modeling for Ag. Science/Day 3/Atriplex.txt', 
                       header=TRUE, sep='\t')

# Inspect the first few rows of the data
head(atriplex)

#Some context
# https://identify.plantnet.org/the-plant-list/species/Atriplex%20cordobensis%20Gand.%20&%20Stuck./data
# Data from Atriplex cordobensis, a forage shrub. The experimental units are arranged in complete blocks.
# Data courtesy of Dr. M. T. Aiazzi, Faculty of Agricultural Sciences, UNC

# Description:
# Size: Size of the seed
# Color: seed coat color
# Germination: Germination percentage
# Normal.seeding
# DW: Dry Weight
# Block: block identification

# The Block is assumed to be continous so I will convert it to a factor
atriplex <- atriplex |>
  mutate(Block = factor(Block))


# Visualize the data ----
atriplex |>  
  ggplot(aes(x = Color, y = Germination, 
             col=Block)) +
  geom_point(size=5) +
  facet_grid(.~Size)+
  theme_bw()

# Models ----
# Fit the first mixed linear model using lmer from lme4 package
# Block is used as a random variable - pay attention to the notation
# (1|Block) the "1" here refers to the intercept (it means block affects the intercept of the model)
model1 <- lmer(Germination ~ Color + Size + (1 | Block), 
               data = atriplex)

#
#
#

# EXAMPLE 2 ----

# Data ----
crowder.seeds <- agridat::crowder.seeds
head(crowder.seeds)
# To see more details of the data:
help(crowder.seeds)

# Visualize the data ----
crowder.seeds |> 
  ggplot(aes(x = gen, y = germ / n)) +
  geom_point() +
  geom_boxplot(aes(group = gen), alpha = 0.5) +
  facet_wrap(~ extract) +
  theme_minimal() +
  labs(title = "crowder.seeds",
       x = "Gen",
       y = "Germination Rate")


# Model ----
# Fit a linear mixed model using glmmTMB
m1.glmmtmb <- glmmTMB(germ / n ~ gen * extract + (1 | plate),
                      data = crowder.seeds)


#
# END ----