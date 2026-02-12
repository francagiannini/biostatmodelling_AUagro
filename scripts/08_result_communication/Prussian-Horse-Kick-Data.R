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

# Notice that the pairwise comparisons are given as odds ratios.
# This is because we have a log-link. If you run the emmeans above without the
# type="response", you can see you get absolute differences.

# If the ratio is 1, the two corps are not different. 
# <1, first corp in order is smaller
# >1, first corp is larger

# END ----