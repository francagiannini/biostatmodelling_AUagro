# Packages ----
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(performance)

# Multiple linear regression

# Data ----

# We use an example dataset from package agridat. Read it in and look for
# the help file to understand the dataset.
thompson.cornsoy <- agridat::thompson.cornsoy 
head(thompson.cornsoy)
help("thompson.cornsoy")

# Make a plot showing how corn yields vary along time


# Make a plot showing how corn yield varies with weather variables.
# Note: One plot is unlikely to be enough.

# Model how corn yields are predicted by the different weather variables


#
# Check for multicollinearity of your model
multicollinearity(model)
check_model(model, check = "vif")
# Are there problems? If there are, fix them by removing the variable with 
# highest VIF value first, and check again.
# Repeat until you don't see problems!

#
# Which weather variable is the best predictor?
drop1(model)
# Larger the sum of squares, the larger the effect on variance explained
# by the model 

# So, which is the best predictor for corn yields?

#
#
#

# CENTS splines ----

# This example goes beyond the scope of our course. But it serves as an
# example on further directions you can go to. Splines model covariates
# in non-linear ways. And to fit them is not that different from the models
# you have been fitting so far! 

# Look through the example and see how much of the model fitting you can 
# understand.

# Data ----
CENTS_splines <- read.csv("CENTS_splines.csv")

str(CENTS_splines)

# Change class of factors
CENTS_splines <- CENTS_splines |>
  mutate(across(c(Block, Cover), factor))

# Initial figure
ggplot(CENTS_splines, aes(x=Week, y=NitrateN, colour=Cover)) +
  theme_bw() +
  geom_point() 

# Linear model ----
mod_linear <- glmmTMB(NitrateN ~ Cover + Week +
                        Week:Cover +
                        (1 | Block),
                      dispformula = ~ Cover,
                      family=Gamma(link="log"),
                      data = CENTS_splines)

# Model assumptions
CENTS_res <- resid(mod_linear)
CENTS_pred <- predict(mod_linear)
resid_auxpanel(CENTS_res, CENTS_pred)

# Model output
summary(mod_linear)

# Spline glmm ----
library(splines)

mod_glmm <- glmmTMB(NitrateN ~ Cover +
                      bs(Week, df = 5) * Cover +
                      (1 | Block),
                    dispformula = ~ Cover,
                    family=Gamma(link="log"),
                    data = CENTS_splines)


# Model assumptions
CENTS_res <- resid(mod_glmm)
CENTS_pred <- predict(mod_glmm)
resid_auxpanel(CENTS_res, CENTS_pred)

# Model output
summary(mod_glmm)

# Plot predictions from the glmm ----

# Make a grid of data over all the components of the model
# Allow the continuous covariate to have values in the entire range
newdat <- expand.grid(
  Week = seq(min(CENTS_splines$Week), max(CENTS_splines$Week), length.out = 200),
  Cover = levels(CENTS_splines$Cover),
  Block = levels(CENTS_splines$Block)[1]
)

# Get predictions from the model
pred <- predict(mod_glmm, newdat, se.fit = TRUE, type = "response")

# Add the predictions into the new data grid
newdat$fit   <- pred$fit
newdat$lower <- pred$fit - 1.96 * pred$se.fit # Confidence interval
newdat$upper <- pred$fit + 1.96 * pred$se.fit # Confidence interval

# Plot the predictions
ggplot(newdat, aes(x = Week, y = fit, color = Cover, fill = Cover)) +
  geom_point(data = CENTS_splines,
             aes(x = Week, y = NitrateN, color = Cover),
             alpha = 0.4, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.7, color = NA) +
  geom_line(linewidth = 1.1) +
  theme_bw() +
  labs(x = "Week",
       y = "Predicted Nitrate-N")

# GAM ----
library(mgcv)
library(gratia)

mod_gam <- gam(NitrateN ~ Cover +
                 s(Week, k=5, by=Cover, bs="ts") + 
                 s(Block, bs="re"), # random effect
               family=tw(link="log"), # tweedie distribution
               data = CENTS_splines)

# Check assumptions
appraise(mod_gam)

# Quick illustration
draw(mod_gam, residuals=T)

# Model output
summary(mod_gam)

# Plot predictions

# Create a datagrid
newdat <- expand.grid(
  Week = seq(min(CENTS_splines$Week), max(CENTS_splines$Week), length.out = 200),
  Cover = levels(CENTS_splines$Cover),
  Block = levels(CENTS_splines$Block)[1])

# Predict from the model into the new data
pred <- predict(mod_gam, newdat, se.fit = TRUE, type = "response")

# Extract predictions
newdat$fit   <- pred$fit
newdat$lower <- pred$fit - 1.96 * pred$se.fit
newdat$upper <- pred$fit + 1.96 * pred$se.fit

# Plot predictions
ggplot(newdat, aes(x = Week, y = fit, color = Cover, fill = Cover)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(data = CENTS_splines,
             aes(x = Week, y = NitrateN, color = Cover),
             alpha = 0.4, inherit.aes = FALSE) +
  theme_bw() +
  labs(x = "Week",
       y = "Predicted Nitrate-N")

# END ----