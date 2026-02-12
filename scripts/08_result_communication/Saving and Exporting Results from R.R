#==============================================================================#
# Exporting Results and Estimates in R                                         #
#==============================================================================#

#### Housekeeping ####
# Load packages
library(tidyverse)
library(agridat)
library(nlme)
library(performance)
library(emmeans)
library(writexl)

# Set global options
#  define working directory
setwd("C:/Users/au802896/OneDrive - Aarhus universitet/Workshops & Trainings/Biostatistical Modelling Course/D8B1_CommunicatingResults")

#  define default ggplot behavior
my_ggplot_theme <- theme_classic() +
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14))
theme_set(my_ggplot_theme)
  
#### Data Import & Prep ####
oats <- yates.oats |> 
  mutate(nitro = factor(nitro))

#### Data Exploration & Validation ####

# Results not shown

#### Model Development and Evaluation
# Fit model
oat_mod <- lme(yield ~ gen*nitro, data = oats, 
               random = ~1|block/gen)

# Check model - looks OK
check_model(oat_mod, 
            check = c('linearity', 'qq', 'normality', 'homogeneity'), 
            detrend = F)

#### Estimates, Tests and Plots ####
# ANOVA table - save results to an R object
oat_ftest <- joint_tests(oat_mod) 
oat_ftest

# Calculate marginal means
oat_emm <- emmeans(oat_mod, ~ nitro) 
oat_emm

# Test contrasts
oat_con <- contrast(oat_emm, 'trt.vs.ctrl', ref = 'nitro0')
oat_con

# Convert tables to data frames
oat_ftest <- as.data.frame(oat_ftest)
oat_emm <- as.data.frame(oat_emm)
oat_con <- as.data.frame(oat_con)

# Plot results
oat_plot <- ggplot(oat_emm, aes(x = nitro))+
  geom_point(aes(y = emmean), size = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15) +
  geom_point(aes(y = yield), data = oats, 
             position = position_nudge(.2)) +
  scale_y_continuous(limits = c(0, 180))
oat_plot

# Export/save results:
# Create a named list of the results tables (must be data.frames)
oat_results <- list('F test' = oat_ftest, 
                    'Ests' = oat_emm, 
                    'Tests' = oat_con)

# write tables as sheets in an excel
write_xlsx(x = oat_results, path = 'Oat Results Tables.xlsx')

# Save our ggplot as a jpg
ggsave(filename = 'Oat Results Plot.jpg', plot = oat_plot, device = 'jpg', 
       height = 6, width = 6)



