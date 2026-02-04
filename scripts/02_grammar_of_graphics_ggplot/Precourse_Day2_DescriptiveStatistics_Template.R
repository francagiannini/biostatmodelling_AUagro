# Descriptive statistics ----
# Template for calculations

#
# TASK: Read in data from Sharepoint ----

#
# TASK: Calculate proportion of white beans of total ----


# Below, replace anything fully written in CAPITALS with names 
# you use in your own data.

# Graph the distribution ----

# Histogram
ggplot(DATA, aes(x=PROPORTION)) +
  theme_classic() +
  geom_histogram(bins = 5, # Change here to increase or decrease number of bars in the graph
                 fill="steelblue", 
                 colour="black")

# Boxplot
ggplot(DATA, aes(x=PROPORTION)) +
  theme_classic() +
  geom_boxplot(fill="steelblue")

#
#
#

# Descriptive statistics ----

# List of functions

# Measures of central tendency
mean(DATA$PROPORTION)
median(DATA$PROPORTION)
DescTools::Mode(DATA$PROPORTION)
# Note: For calculating mode, you need package DescTools. You may need to install it.

# Measures of variability
sd(DATA$PROPORTION)
var(DATA$PROPORTION) 
range(DATA$PROPORTION)
quantile(DATA$PROPORTION)

# Keeping it tidy: applying the function to get one output
DATA %>%
  summarise(mean = mean(PROPORTION),
            median = median(PROPORTION),
            sd = sd(PROPORTION)) # Feel free to add other measures here

# END ----