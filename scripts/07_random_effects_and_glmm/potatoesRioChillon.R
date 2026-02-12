# 0. Load  libraries ----
library(tidyverse) # Data manipulation and visualization
library(agricolae) # Agricultural research tools
library(glmmTMB)   # Generalized linear mixed models
library(DHARMa)    # Residual diagnostics for hierarchical models

# 1. The data ----

# Analysis of potatoes in Peru using the Mother/Baby Trial Design.
# The Mother trial is a central, researcher-managed site.
# The Baby trials are decentralized, farmer-managed plots.
data(RioChillon)

# View dataset documentation
?RioChillon

# Split into two datasets for comparison
chillon_b <- RioChillon$babies
chillon_m <- RioChillon$mother

# 2. Variance Component Analysis (VCA) ----

# We want to determine how much yield variability is driven by:
# 1. Genetic factors (clones)
# 2. Environmental/Management factors (blocks or farmers)

# First, analyze the mother trial (controlled experimental station)
moms_vca <- 
  lme4::lmer(yield ~ 1 + (1 | block) + (1 | clon), 
             data = chillon_m)

summary(moms_vca)
var_comp_moms <- VarCorr(moms_vca)

# Second, analyze the baby trials (variable farmer conditions)
babies_vca <- 
  lme4::lmer(yield ~ 1 + (1 | farmer) + (1 | clon), 
             data = chillon_b)

summary(babies_vca)
var_comp_babies <- VarCorr(babies_vca)

# Combine and visualize the variance proportions
# This shows how genetic influence differs between station and farm environments
var_comp_moms |> 
  as_tibble() |>
  mutate(proportion = vcov / sum(vcov),
         trial = "moms") |> 
  bind_rows(var_comp_babies |> 
              as_tibble() |>
              mutate(proportion = vcov / sum(vcov),
                     trial = "babies")) |>
  rename('component' = 'grp') |> 
  ggplot(aes(y = proportion, x = trial, fill = component)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(proportion)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Variance components: mother vs. baby trials",
       x = "Trial type",
       y = "Proportion of total variance") +
  theme_bw()

# 3. Evaluating clone performance through BLUPs ----

# Best Linear Unbiased Predictors (BLUPs) allow us to estimate the 
# genetic potential of each clone while accounting for site/farmer effects.

# Fitting the mixed model for baby trials
mod_babies_tmb <- glmmTMB(yield ~ farmer + (1 | clon), 
                          data = chillon_b, 
                          family = gaussian())

# Residual analysis with DHARMa
# Simulates residuals to check for model assumptions in mixed models
res_babies <- simulateResiduals(mod_babies_tmb)
plot(res_babies) 

# Extracting BLUPs for clones in baby trials
blups_babies <- ranef(mod_babies_tmb)$cond$clon |> 
  rownames_to_column("clon") |> 
  as.data.frame() |> 
  rename("yield_blup_babies" = `(Intercept)`)

# Fitting the mixed model for the mother trial
mod_mother_tmb <- glmmTMB(yield ~ block + (1 | clon), 
                          data = chillon_m, 
                          family = gaussian())

# Check residuals for the mother model
res_mother <- simulateResiduals(mod_mother_tmb)
plot(res_mother)

# Extracting BLUPs for clones in the mother trial
blups_mother <- ranef(mod_mother_tmb)$cond$clon |> 
  rownames_to_column("clon") |> 
  as.data.frame() |> 
  rename("yield_blup_mother" = `(Intercept)`) |>
  arrange(desc(yield_blup_mother))

# Preview top-performing clones from the mother trial
print(head(blups_mother))

# 4. Comparative analysis ----

# Combine both sets of BLUPs to see if clones perform consistently 
# across both research environments and farmer fields.
comparison_df <- inner_join(blups_babies, blups_mother, by = "clon")

# Visualize the ranking of clones in both trials
comparison_df |> 
  pivot_longer(cols = c(yield_blup_babies, yield_blup_mother),
               names_to = 'trial',
               values_to = 'yield_blup') |> 
  ggplot(aes(x = reorder(clon, yield_blup), y = yield_blup)) +
  geom_col(width = 0.5) +
  facet_grid(trial ~ .) +
  labs(title = "Clone ranking by trial type",
       x = "Clone",
       y = "Yield BLUP") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Final correlation plot: Do Mother trial results predict Baby trial results?
ggplot(comparison_df, aes(x = yield_blup_mother, y = yield_blup_babies)) +
  geom_point(shape = "🥔", size = 4) +
  geom_abline(slope = 1)+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkgrey") +
  geom_text(aes(label = clon), vjust = -1.2, size = 3) +
  labs(title = "Scatter plot of BLUPs: mother vs. baby trials",
       subtitle = "Each potato represents a specific clone",
       x = "Mother trial BLUP",
       y = "Baby trial BLUP") +
  theme_bw()
