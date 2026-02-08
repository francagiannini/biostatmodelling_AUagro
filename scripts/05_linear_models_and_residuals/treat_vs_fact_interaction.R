library(dplyr)
library(tidyr)
library(ggplot2)

# Load data ----
url <- "https://raw.githubusercontent.com/francagiannini/plot_validation_rCTOOL/main/data_lte/Soil_parameters_Soil_C_N.txt"
raw_data <- read.table(url, header = TRUE, sep = "\t")

# Wrangling data ----
# Process data for the 1st and Last year only
soil_plots <- raw_data |>
  pivot_longer(
    cols = c(starts_with("C_"), starts_with("N_")),
    names_sep =  "_",
    names_to = c('variable', 'year')#,
    #values_to =c('C','N')
  ) |>
  pivot_wider(names_from = 'variable',
              values_from = 'value') |>
  mutate(Topsoil_C_obs= ifelse(year==1981,1.54,Bulk_Density_2020)*25*C,
         CN=C/N,
         year=as.numeric(year),
         year_cat=as.factor(year),
         year_cont=as.numeric(year),
         plotyear=interaction(Sample_ID,year),
         treatment=interaction(Straw_Rate,Cover_Crop,sep="_")) |> 
  filter(!year==1981) |>
  filter(year == min(year) | year == max(year))

head(soil_plots)

# Exploring alternatives ----

# --- Alternative  1: the treatment approach ----

soil_plots |> 
  ggplot(aes(x = treatment, y = Topsoil_C_obs, colour = treatment)) +
  stat_summary(
    fun = mean, fun.min = function(x) mean(x) - sd(x), 
    fun.max = function(x) mean(x) + sd(x),
    shape=15, alpha=0.6)+
  geom_jitter(width = 0.2)+
  facet_grid(year_cat~. ) +
  theme_minimal()

model_treatment <- lm(Topsoil_C_obs~ treatment + year_cat, data = soil_plots)

summary(model_treatment)

anova(model_treatment)

# --- Alternative 2: the factorial approach ----

soil_plots |>
  ggplot(aes(x = as.factor(Straw_Rate), y = Topsoil_C_obs, color = Cover_Crop)) +
  geom_jitter(width = 0.2) +
    stat_summary(
      fun = mean, fun.min = function(x) mean(x) - sd(x), 
      fun.max = function(x) mean(x) + sd(x),
      shape=15, alpha=0.6, 
    aes(group = Cover_Crop)) +
  facet_grid(year_cat~ .) +
  theme_minimal()


model_factorial <- lm(
  Topsoil_C_obs ~ Straw_Rate + Cover_Crop + Straw_Rate * Cover_Crop + year_cat, 
                      data = soil_plots)

summary(model_factorial)

anova(model_factorial)

# --- Alternative 3: year as a continuous variable (without interaction) ----

soil_plots |> 
  ggplot(aes(x = year, y = Topsoil_C_obs, color = Cover_Crop)) +
  geom_point() +
  stat_summary(
    fun = mean,
    fun.min = function(x) mean(x) - sd(x),
    fun.max = function(x) mean(x) + sd(x),
    shape = 15,
    alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ Straw_Rate) +
  theme_minimal()

model_cont_cov <- lm(
  Topsoil_C_obs ~ Straw_Rate + Cover_Crop + year_cont, 
  data = soil_plots)

summary(model_cont_cov)

anova(model_cont_cov)

# --- Alternative 4: year as a continuous variable (with interaction)----

model_cont_cov_int <- lm(
  Topsoil_C_obs~ Straw_Rate + Cover_Crop + year_cont+ Straw_Rate*year_cont, 
  data = soil_plots)

summary(model_cont_cov_int)

anova(model_cont_cov_int)
