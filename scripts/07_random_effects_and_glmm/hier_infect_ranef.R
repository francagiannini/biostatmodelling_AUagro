# Hierarquical progression and random effects based on 
# https://mfviz.com/hierarchical-models/ 

library(dplyr)
library(lme4)
library(ggplot2)
library(patchwork)
library(ggrepel)

# --- 1. Parameters and Data Generation ---

genotypes <- c('Resistant', 'Susceptible', 'Hybrid_A', 'Hybrid_B', 'Wild_Type')
base_inf <- c(5, 20, 10, 8, 15)         # Baseline infection (Intercepts)
inf_rates <- c(0.4, 2.8, 1.1, 1.5, 0.7) # Sensitivity to humidity (Slopes)
n_per_genotype <- 100
total_n <- n_per_genotype * length(genotypes)

df <- data.frame(
  id = 1:total_n,
  genotype = rep(genotypes, n_per_genotype),
  humidity = runif(total_n, 10, 40) 
)

# Apply noise for realistic variation
df <- df %>%
  mutate(
    b0 = rep(base_inf, n_per_genotype) + rnorm(total_n, 0, 2),
    b1 = rep(inf_rates, n_per_genotype) + rnorm(total_n, 0, 0.3),
    infection = b0 + (humidity * b1) + rnorm(total_n, 0, 4)
  )

# --- 2. Model 0: Simple Linear Regression ---

m0 <- lm(infection ~ humidity, data = df)
df$simple_model <- predict(m0)
fe0 <- coef(m0)

plot0 <- ggplot(df, aes(x = humidity, y = simple_model)) +
  geom_line(size = 1.2, color = "black") +
  annotate("text", x = 15, y = 90, 
           label = paste0("y == beta[0] + beta[1] * x"), parse = TRUE, size = 5) +
  labs(title = "Simple linear model", subtitle = "Ignores genotype grouping", 
       y = "Predicted infection %", x = "Humidity (%)") +
  theme_bw()

# --- 3. Model 1: Random Intercept ---

m1 <- lmer(infection ~ humidity + (1 | genotype), data = df)
df$rand_int_preds <- predict(m1)
fe1 <- fixef(m1)

plot1 <- ggplot(df, aes(x = humidity, y = rand_int_preds)) +
  geom_line(aes(group = genotype, color = genotype), size = 0.8) +
  # Population average line
  geom_abline(intercept = fe1[1], slope = fe1[2], size = 1.5, color = "black") +
  annotate("text", x = 15, y = 90, 
           label = "y == beta[0] + u[0] + beta[1] * x", parse = TRUE, size = 5) +
  labs(title = "Varying intercept", subtitle = "Different baselines per genotype", 
       y = "Predicted infection %", x = "Humidity (%)") +
  theme_bw()

# --- 4. Model 2: Random Slope ---

m2 <- lmer(infection ~ humidity + (0 + humidity | genotype), data = df)
df$rand_slope_preds <- predict(m2)
fe2 <- fixef(m2)

plot2 <- ggplot(df, aes(x = humidity, y = rand_slope_preds)) +
  geom_line(aes(group = genotype, color = genotype), size = 0.8) +
  # Population average line (intercept is 0 in the random part, but fixed effect remains)
  geom_abline(intercept = fe2[1], slope = fe2[2], size = 1.5, color = "black") +
  annotate("text", x = 15, y = 90, 
           label = "y == beta[0] + (beta[1] + u[1]) * x", parse = TRUE, size = 5) +
  labs(title = "Varying slope", subtitle = "Different sensitivity per genotype", 
       y = "Predicted infection %", x = "Humidity (%)") +
  theme_bw()

# --- 5. Model 3: Full Mixed Model ---

m3 <- lmer(infection ~ humidity + (1 + humidity | genotype), data = df)
df$full_mixed_preds <- predict(m3)
fe3 <- fixef(m3)

plot3 <- ggplot(df, aes(x = humidity, y = full_mixed_preds)) +
  geom_point(aes(y = infection, color = genotype), alpha = 0.1) +
  geom_line(aes(group = genotype, color = genotype), size = 0.8) +
  # Population average line
  geom_abline(intercept = fe3[1], slope = fe3[2], size = 1.5, color = "black") +
  annotate("label", x = 15, y = 100, 
           label = "y == beta[0] + u[0] + (beta[1] + u[1]) * x", 
           parse = TRUE, size = 4, fill = "white") +
  labs(title = "Full mixed model", subtitle = "Varying slope and intercept",
       x = "Humidity (%)", y = "Predicted infection %") +
  theme_bw()

# Combine plots
(plot0 + plot1) / (plot2 + plot3) + plot_layout(guides = 'collect')

# Ranking Blups ---- 
library(ggrepel) # For better label placement

# 1. Extract Random Effects (BLUPs)
random_effects <- ranef(m3)$genotype
random_effects$genotype <- rownames(random_effects)
colnames(random_effects) <- c("Intercept_Dev", "Slope_Dev", "Genotype")

# 2. Plot the distribution of Intercept Deviations (Baseline Resistance)
# Negative values = more resistant than average; Positive = more susceptible
intercept_plot <- ggplot(random_effects, aes(x = reorder(Genotype, Intercept_Dev), y = Intercept_Dev)) +
  geom_col(aes(fill = Intercept_Dev)) +
  coord_flip() +
  scale_fill_gradient2(low = "darkgreen", mid = "grey", high = "darkred") +
  labs(title = "BLUPs: Intercept Deviations",
       subtitle = "Baseline infection level relative to population average",
       x = "Genotype", y = "Deviation (Intercept)") +
  theme_minimal()

# 3. Plot the distribution of Slope Deviations (Humidity Sensitivity)
# Negative values = less sensitive to humidity; Positive = high risk in wet conditions
slope_plot <- ggplot(random_effects, aes(x = reorder(Genotype, Slope_Dev), y = Slope_Dev)) +
  geom_col(aes(fill = Slope_Dev)) +
  coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "grey", high = "orange") +
  labs(title = "BLUPs: Slope Deviations",
       subtitle = "Sensitivity to humidity relative to population average",
       x = "Genotype", y = "Deviation (Slope)") +
  theme_minimal()

# 4. Scatter plot of BLUPs (Intercept vs Slope)
# This identifies genotypes that are good in ALL conditions vs those that fail as it gets wet
caterpillar_plot <- ggplot(random_effects, aes(x = Intercept_Dev, y = Slope_Dev, label = Genotype)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  geom_text_repel() +
  labs(title = "Genotype Characterization (BLUPs)",
       x = "Baseline Infection (Intercept Deviation)",
       y = "Humidity Sensitivity (Slope Deviation)") +
  theme_light()

print(intercept_plot)
print(slope_plot)
print(caterpillar_plot)

# Combine plots
(intercept_plot+ slope_plot)+ plot_layout(guides = 'collect')

