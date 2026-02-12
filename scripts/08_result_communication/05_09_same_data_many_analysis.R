library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)

# Load data ----
url <- "https://raw.githubusercontent.com/francagiannini/plot_validation_rCTOOL/main/data_lte/Soil_parameters_Soil_C_N.txt"
raw_data <- read.table(url, header = TRUE, sep = "\t")

# Wrangling data ----
# Process data for the 1st and Last year only
soil_plots <- 
  raw_data |>
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
         year2=year^2,
         plotyear=interaction(Sample_ID,year),
         treatment=interaction(Straw_Rate,Cover_Crop,sep="_")) 

head(soil_plots)

# Exploring alternatives ----


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


soil_plots |> 
  ggplot(aes(x = year, y = C, color = as.factor(Straw_Rate))) +
  geom_point() +
  stat_summary(
    fun = mean,
    fun.min = function(x) mean(x) - sd(x),
    fun.max = function(x) mean(x) + sd(x),
    shape = 15,
    alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ Cover_Crop) +
  theme_minimal()

mod.glmm <- glmmTMB(formula = C ~ Straw_Rate + 
                      Cover_Crop + 
                      year_cont+ 
                      Straw_Rate*year_cont+
                      year2+ (Block),
                       # + ar1(factweek + 0 | plot), # add ar1 structure as random term to mimic error variance
                        dispformula = ~ 0, # fix original error variance to 0
                        REML = TRUE,       # needs to be stated since default = ML
                        data = soil_plots)

summary(mod.glmm)

car::Anova(mod.glmm)

# --- Alternative 4: year as a continuous variable (with interaction)----

model_cont_cov_int <- lm(
  Topsoil_C_obs~ Straw_Rate + Cover_Crop + year_cont+ Straw_Rate*year_cont, 
  data = soil_plots)

summary(model_cont_cov_int)

anova(model_cont_cov_int)
