#==============================================================================#
# Model Evaluation & Selection Practicum                                       #
# Biostatistical Modeling for Agricultural Science                             #
# 11 February 2026 - Aarhus University                                         #
#==============================================================================#

#### Archibold's Apples ####

##### Housekeeping ####
# Load libraries
library(agridat)
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(performance)
library(ggResidpanel)

# Set global options

##### Data Import & Preparation ####
# This data comes from a split-split-plot RCBD, where 
# tree spacing is the main-plot factor, and rootstock is
# the sub-plot factor. Here, we'll treat spacing as a factor
# variable, and subset the data to remove missing values 
# (assumed missing completely at random)
apple <- archbold.apple |> 
  mutate(spacing = factor(spacing)) |> 
  filter(!is.na(yield))
str(apple)

##### Data Exploration & Validation ####
# This is an abbreviated version of this step...
apple |> 
  group_by(spacing, stock, gen) |> 
  summarise(Min = min(yield), 
            Mean = mean(yield), 
            Max = max(yield)) |> 
  print(n = Inf)

apple |> 
  ggplot(aes(x = stock, color = gen, y = yield))+
  facet_grid(cols = vars(spacing))+
  geom_boxplot()

##### Model Development & Evaluation ####
# Start with the simplest model which is consistent with the data architecture
# (the 3x4x2 factorial treatment structure and the split-split-plot RCBD design)
apple_mod_1 <- glmmTMB(yield ~ gen*stock*spacing + (1|rep/spacing/stock), 
                       data = apple, family = gaussian(link = 'identity'), 
                       REML = T)

# A warning is returned which says "non-positive-definite Hessian matrix" - is 
# this a serious issue or something we can safely ignore? Or perhaps it depends?
# Read the help file and decide how we should proceed:
vignette('troubleshooting', package = 'glmmTMB')

# Let's try supplying the optimization algorithm with starting values for the
# random effects variance parameters (which glmmTMB calls "theta"):
apple_mod_2 <- glmmTMB(yield ~ gen*stock*spacing + (1|rep/spacing/stock), 
                       data = apple, family = gaussian(link = 'identity'), 
                       REML = T, start = list(theta = c(10, 10, 10)))

# No more warnings! 

# The performance package has a suite of tools for checking model assumptions.
# Generally, graphical/visual checks of residual diagnostic plots are better
# than formal tests of these model assumptions. For ANOVA-type models (i.e.
# those with only categorical predictors), some of the most useful diagnostic
# plots are:
performance::check_model(apple_mod_2, 
            check = c('linearity', 'homogeneity', 'qq', 'normality'),
            detrend = F)

# Another package providing tools for residual diagnostic plots is the 
# ggResidpanel package
apple_res <- resid(apple_mod_2)
apple_pred <- predict(apple_mod_2)
resid_auxpanel(apple_res, apple_pred)

boxplot(apple_res ~ apple$rep)
boxplot(apple_res ~ apple$spacing)

# Finally, the DHARMa package provides tools for visual model checking which
# work even for models which do not assume normality (i.e. generalized linear
# and generalized linear mixed models).
apple_qres <- simulateResiduals(apple_mod_2, plot = T)

# These simulated quantile residuals can be converted to normal residuals
# if one wants to:
apple_res_2 <- resid(apple_qres, quantileFunction = qnorm, 
                     outlierValues = c(-5, 5))
resid_auxpanel(apple_res_2, apple_pred)

# Only the performance package, however, has tools for checking the
# distributional assumptions for the random effects:
check_model(apple_mod_2, check = c('reqq'))

# None of the plots strongly suggested heteroscedasticity,
# but for the sake of illustration, let us fit a model for 
# which the residual variance differs for each genotype, and
# see if model fit is improved:
apple_mod_3 <- glmmTMB(yield ~ gen*stock*spacing + (1|rep/spacing/stock), 
                       data = apple, family = gaussian(link = 'identity'), 
                       REML = T, start = list(theta = c(10, 10, 10)), 
                       dispformula = ~ gen)

# Now we get a different warning, about "NA/NaN function evaluation". Go back to
# the troubleshooting vignette and determine if this is a major issue or
# something we can safely ignore. In the space below, either create code for a
# new model or make a note explaining why this warning can be ignored.



# Now, let's first repeat the check of the residuals and other model
# assumptions
check_model(apple_mod_3, check = c('linearity', 'homogeneity', 'normality', 'qq'), 
            detrend = F)

apple_res_3 <- resid(apple_mod_3)
apple_pred_3 <- predict(apple_mod_3)
resid_auxpanel(apple_res_3, apple_pred_3)

# Now, we can use information criteria to compare model fit
AIC(apple_mod_2, apple_mod_3)
BIC(apple_mod_2, apple_mod_3)
compare_performance(apple_mod_2, apple_mod_3, metrics = c('AIC', 'BIC', 'AICc'))

#### Your Data ####
##### Housekeeping ####



##### Data Exploration & Validation ####
# You can skip this step

##### Model Development & Evaluation ####
# Initial model fit:


# Create residual diagnostic plots to check model assumptions:






# Create plots checking distributional assumptions of the random 
# effects, if appropriate:






# Create a second model on the basis of potential issues identified
# in the diagnostic plots (or, if all the plots looked perfect, 
# then create an alternative just for illustrative purposes)



# Create diagnostic plots to check the model assumptions of the
# revised model:



# Use AICc to compare the two models:









