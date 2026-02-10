# Load necessary packages
library(tidyverse)
library(lme4)
library(lmerTest) # This package helps you get p-values for lme4-models
library(glmmTMB)
library(DHARMa)
library(nlme)
library(emmeans)

#
#
#

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



# Different ways of fitting mixed models.
# We have many tools for fitting models. Look at two other options for defining this model below.
# Compare how it is being defined to model1.

# Fit the second mixed linear model using lme from the nlme package
# Notice that the random effect notation is different, but also contains the "1" for intercept
model2 <- lme(Germination ~ Color + Size, 
                     random = ~1 | Block, 
                     data = atriplex)

# Fit the third mixed linear model using glmmTMB
model3 <- glmmTMB(Germination ~ Color + Size + (1 | Block), 
                  data = atriplex, 
                  REML = T)

# Diagnostics ----
# Compare the models using AIC
AIC(model1, model2, model3)
# What do you see?

# Evaluate model1 
par(mfrow=c(1,3))
qqnorm(resid(model1));qqline(resid(model1))
hist(resid(model1))
plot(fitted(model1), resid(model1));abline(h=0)
par(mfrow=c(1,1))

# Evaluate model2
par(mfrow=c(1,3))
qqnorm(resid(model2));qqline(resid(model2))
hist(resid(model2))
plot(fitted(model2), resid(model2));abline(h=0)
par(mfrow=c(1,1))


# Evaluate model3
par(mfrow=c(1,3))
qqnorm(resid(model3));qqline(resid(model3))
hist(resid(model3))
plot(fitted(model3), resid(model3));abline(h=0)
par(mfrow=c(1,1))

# Or, we can create these plots using the DHARMa package
# Evaluate the residuals using DHARMa for model1
simulationOutput1 <- simulateResiduals(fittedModel = model1)
plot(simulationOutput1)

# Evaluate the residuals using DHARMa for model2
simulationOutput2 <- simulateResiduals(fittedModel = model2)
plot(simulationOutput2)

# Evaluate the residuals using DHARMa for model3
simulationOutput3 <- simulateResiduals(fittedModel = model3)
plot(simulationOutput3)

# What can you see based on the residuals?

#
# Extract the fitted values and residuals
atriplex$fitlme <- fitted(model1)
atriplex$reslme <- residuals(model1)

# Plot the fitted values vs. the observed values
ggplot(atriplex, aes(x = fitlme, y = Germination)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Fitted vs Observed Values",
       x = "Fitted Values",
       y = "Observed Values")

# Plot the residuals
ggplot(atriplex, aes(x = fitlme, y = reslme)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

# Results ----
# Summarize the results of model1
car::Anova(model1)
summary(model1)

# Summarize the results of model2
car::Anova(model2)
summary(model2)

# Summarize the results of model3
car::Anova(model3)
summary(model3)

# Visualise the results ----
# Calculate marginal effects using emmeans 
marginal_effects_glmm <- emmeans(model1, ~Color | Size)
marginal_effects_glmm

# Plot marginal effects for m1.glmm
plot(marginal_effects_glmm) +
  theme_bw() +
  labs(x = "Estimated marginal mean",
       y = "Predictors")

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

# Diagnostics ---
# Evaluate the residuals using DHARMa for m1.glmmtmb
plot(simulateResiduals(fittedModel = m1.glmmtmb))

# Extract the fitted values and residuals for m1.glmm
crowder.seeds$fit_glmm <- fitted(m1.glmmtmb)
crowder.seeds$res_glmm <- residuals(m1.glmmtmb)

# Plot the fitted values vs. the observed values for m1.glmm
ggplot(crowder.seeds, aes(x = fit_glmm, y = germ/n)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Fitted vs Observed Values (glmmPQL)",
       x = "Fitted Values",
       y = "Observed Values")

# Results ----
# Summarize the results of m1.glmmtmb
car::Anova(m1.glmmtmb)
summary(m1.glmmtmb)

# Visualise the results ----
# Calculate marginal effects using emmeans 
marginal_effects_glmm <- emmeans(m1.glmmtmb, ~ extract | gen)
marginal_effects_glmm

# Plot marginal effects for m1.glmm
plot(marginal_effects_glmm) +
  theme_bw() +
  labs(x = "Estimated marginal mean",
       y = "Predictors")

#
#
#

# EXAMPLE 3 ----

# Data ----
yates.oats <- agridat::yates.oats
head(yates.oats)
# To see more details of the data:
help(yates.oats)

# We want to treat nitrogen application as a factor, se we have to convert it
yates.oats$nitro <- factor(yates.oats$nitro)

# Visualize the data ----
yates.oats |> 
  ggplot(aes(x = nitro, y = yield)) +
  theme_bw() +
  #geom_point() + # Uncomment to see the raw values
  geom_boxplot(fill="steelblue2") +
  facet_wrap(~ gen) +
  labs(x = "Nitrogen treatment (hundredweight per acre)",
       y = "Yield (1/4 pounds per sub-plot)")


# Model ----
yates_mod <- lmer(yield ~ nitro + gen + nitro:gen +
                    (1 | block/gen),
                  data = yates.oats)
# Notice how the random effects are defined.
# If you look at the help() for this dataset, you can see the description of
# the experimental desing. genotypes are nested within the block.

# Results ----
# Summarize the results of m1.glmmtmb
anova(yates_mod)
summary(yates_mod)

# Visualise the results ----
# Calculate marginal effects using emmeans 
marginal_effects_glmm <- emmeans(yates_mod, ~ nitro | gen)
marginal_effects_glmm

# Plot marginal effects for m1.glmm
plot(marginal_effects_glmm) +
  theme_bw() +
  labs(x = "Estimated marginal mean",
       y = "Predictors")


# END ----