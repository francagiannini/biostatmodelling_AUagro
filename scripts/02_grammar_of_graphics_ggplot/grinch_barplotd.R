# Welcome to the "Grinch reveal"

# Set up ----

library(tidyverse)
library(patchwork)

# Grinch poster palette
grinch_green <- "#68A357"
grinch_dark  <- "#2E4C28"
xmas_red     <- "#C41E3A"
ink_grey     <- "#2B2B2B"

# Clean poster theme
theme_grinch <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", color = ink_grey, size = 14),
      plot.subtitle = element_text(color = "#555555", size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_text(color = ink_grey, face = "bold")
    )
}


# Same Bar-plot different distributions -----

set.seed(2026)

# Helper function: forces any vector to have the exact same Mean and SE
force_mean_se <- function(x, target_mean = 25, target_se = 1.5) {
  target_sd <- target_se * sqrt(length(x))
  as.numeric(target_mean + scale(x) * target_sd)
}

# Create 4 different realities with identical Mean (25) and SE (1.5)
df_reveal <- bind_rows(
  tibble(group = "Normal\n(n = 20)",  value = force_mean_se(rnorm(20))),
  tibble(group = "Outlier\n(n = 20)", value = force_mean_se(c(rnorm(19, 0, 0.2), 6))),
  tibble(group = "Bimodal\n(n = 20)", value = force_mean_se(c(rnorm(10, -2, 0.3), rnorm(10, 2, 0.3)))),
  tibble(group = "Small n\n(n = 4)",  value = force_mean_se(c(-1.5, -0.5, 0.5, 1.5)))
) |>
  mutate(group = factor(group, levels = unique(group)))

# Summary stats for the barplot
df_summary <- df_reveal |>
  group_by(group) |>
  summarise(
    mean = mean(value),
    se   = sd(value) / sqrt(n()),
    .groups = "drop"
  )

# Panel A: The Naughty Dynamite Plot
p_naughty <- ggplot(df_summary, aes(x = group, y = mean)) +
  geom_col(fill = xmas_red, width = 0.6, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 0.8, color = ink_grey) +
  coord_cartesian(ylim = c(0, 55)) +
  labs(
    title = "NAUGHTY: What the barplot shows",
    subtitle = "Four identical bars (Mean ± SE)",
    y = "Measured Variable"
  ) +
  theme_grinch()

# Panel B: The Grinch Reveal (Raw Data)
p_nice <- ggplot(df_reveal, aes(x = group, y = value)) +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.85, color = grinch_green) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.45, color = ink_grey, linewidth = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = ink_grey, linewidth = 0.8) +
  coord_cartesian(ylim = c(0, 55)) +
  labs(
    title = "NICE: What the Grinch hid inside",
    subtitle = "Same Mean ± SE, four completely different stories",
    y = NULL
  ) +
  theme_grinch()

# Combine side-by-side
poster_centerpiece <- p_naughty + p_nice +
  plot_annotation(
    title = "HOW THE BARPLOT STOLE YOUR DISTRIBUTION",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = grinch_dark))
  )

poster_centerpiece

# Save high-res for your poster
# ggsave("grinch_centerpiece.png", poster_centerpiece, width = 11, height = 5, dpi = 300, bg = "white")

# The Alternatives ----

# --- Alternative 1: Strip / Dot Plot (Best for small n < 15) ---
df_small <- tibble(
  treatment = rep(c("Control", "Treatment A"), each = 7),
  value     = c(rnorm(7, 18, 3), rnorm(7, 26, 4))
)

p_alt1_s <- ggplot(df_small, aes(x = treatment, y = value)) +
  geom_jitter(width = 0.12, size = 3, color = grinch_green, alpha = 0.9) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, color = ink_grey, linewidth = 0.6) +
  labs(
    title = "1. Strip Plot",
    subtitle = "Small n (points + median)",
    y = "Response"
  ) +
  theme_grinch()

p_alt1 <- ggplot(df_small, aes(x = treatment, y = value)) +
  # Raw data points lightly in the background
  geom_jitter(width = 0.12, size = 2.2, alpha = 0.45, color = grinch_green) +
  # Mean + 95% CI (swap mean_cl_normal for mean_se if Hmisc isn't installed)
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "pointrange",
    size = 0.9,
    linewidth = 1,
    color = grinch_dark
  ) +
  labs(
    title = "1. Point-Range + Raw Data",
    subtitle = "Mean ± 95% CI (small n)",
    y = "Response"
  ) +
  theme_grinch()


# --- Alternative 2: Boxplot + Jittered Points (Best for medium/large n) ---
# Tip: For a raincloud plot, add ggdist::stat_halfeye() next to this boxplot
df_large <- tibble(
  treatment = rep(c("Control", "Treatment A"), each = 40),
  value     = c(rnorm(40, 20, 4), rlnorm(40, log(24), 0.35))
)

p_alt2 <- ggplot(df_large, aes(x = treatment, y = value)) +
  geom_boxplot(width = 0.4, fill = grinch_green, alpha = 0.3, outlier.shape = NA, color = grinch_dark) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.7, color = grinch_dark) +
  labs(
    title = "2. Boxplot + Raw Data",
    subtitle = "Medium-to-large n",
    y = NULL
  ) +
  theme_grinch()


# --- Alternative 3: Slopegraph (Best for paired / before-after data) ---
df_paired <- tibble(
  id     = factor(1:10),
  Before = rnorm(10, 20, 4),
  After  = Before + rnorm(10, 5, 2.5)
) |>
  pivot_longer(cols = c(Before, After), names_to = "time", values_to = "value") |>
  mutate(time = factor(time, levels = c("Before", "After")))

p_alt3 <- ggplot(df_paired, aes(x = time, y = value, group = id)) +
  geom_line(color = grinch_green, linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 2.8, color = grinch_dark) +
  labs(
    title = "3. Slopegraph",
    subtitle = "Paired / longitudinal data",
    y = NULL
  ) +
  theme_grinch()


# Combine all 3 alternatives into one horizontal strip for the bottom of the poster
poster_alternatives <- p_alt1 + p_alt2 + p_alt3 +
  plot_annotation(
    title = "WHAT TO USE INSTEAD OF BARPLOTS",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = grinch_dark))
  )

poster_alternatives

# Save high-res for your poster
# ggsave("grinch_alternatives.png", poster_alternatives, width = 12, height = 4.2, dpi = 300, bg = "white")
